open! Core
open! Hardcaml
open! Signal

let result_width = 64
let row_width = 150

module Loader = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_in : 'a Uart.Byte_with_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a
      ; row : 'a [@bits row_width]
      ; last: 'a
      }
    [@@deriving hardcaml]
  end

  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    = 
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw eol	= uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
    let%hw eof  = uart_in.valid &&: (uart_in.value ==: (of_unsigned_int ~width:8 4)) in
    let%hw splitter = uart_in.valid &&: ((uart_in.value ==: (of_char '^')) ||: (uart_in.value ==: (of_char 'S'))) in

    let shreg = reg_fb spec ~width:row_width ~enable:uart_in.valid 
    				~f:(fun x -> mux2 eol (zero row_width) (drop_top ~width:1 (x @: (mux2 splitter vdd gnd)))) in
    let row = sll ~by:1 shreg in
    {valid_out = (eol ||: eof); row; last = eof}
  ;;
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day07_loader" create
  ;;
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid_in : 'a
    ; row : 'a [@bits row_width]
    ; last: 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { 
      valid_out : 'a 
    ; result : 'a [@bits result_width]
    }
  [@@deriving hardcaml]
end

(*
let create scope ({clock; clear; valid_in; row; last; _} : _ I.t) : _ O.t
  =
	let spec = Reg_spec.create ~clock ~clear () in

	let%hw row_count = reg_fb spec ~width:16 ~enable:valid_in ~f:(fun x -> x +:. 1) in
	let next_beam_pos = wire row_width in
	let%hw beam_pos = reg spec ~enable:valid_in (mux2 (row_count ==:. 0) row next_beam_pos) in

	next_beam_pos <-- (gnd @: 
					  (List.init (row_width - 2) ~f:(fun x -> x + 1) |>
					  List.map ~f:(fun x -> mux2 (beam_pos.:(x) &&: row.:(x)) gnd 
						  (mux2 ((beam_pos.:(x + 1) &&: row.:(x + 1)) ||: (beam_pos.:(x - 1) &&: row.:(x - 1)) ) vdd beam_pos.:(x) )) |>
					  concat_lsb) 
					  @: gnd); 


	let%hw n = uresize ~width:result_width (popcount (beam_pos &: row)) in

	let count = reg_fb spec ~width:result_width ~enable:valid_in
				~f:(fun x -> x +: n) in
	let valid_out = reg spec last in
	{ valid_out; result = count }
;;
*)
let create scope ({clock; clear; valid_in; row; last; _} : _ I.t) : _ O.t
  =
	let spec = Reg_spec.create ~clock ~clear () in

	let%hw row_count = reg_fb spec ~width:16 ~enable:valid_in ~f:(fun x -> x +:. 1) in

	let next_beam_weights = List.init row_width ~f:(fun _ -> wire result_width) in
	let beam_weights = List.init row_width ~f:(fun x -> reg spec ~enable:valid_in 
						(mux2 (row_count ==:. 0) (uresize ~width:result_width row.:(x)) (List.nth_exn next_beam_weights x))) in

	List.init (row_width - 2) ~f:(fun x -> x + 1) |>
	List.iter ~f:(fun x -> ((List.nth_exn next_beam_weights x) <-- (
		mux2 row.:(x) (zero result_width) 
			((List.nth_exn beam_weights x) +: 
			(mux2 row.:(x + 1) (List.nth_exn beam_weights (x + 1)) (zero result_width)) +:
			(mux2 row.:(x - 1) (List.nth_exn beam_weights (x - 1)) (zero result_width)))
	)));
	(List.nth_exn next_beam_weights 0)                <-- (zero result_width);
	(List.nth_exn next_beam_weights (row_width - 1))  <-- (zero result_width);

	let count = tree ~arity:2 beam_weights ~f:(reduce ~f:(+:)) in
	let valid_out = reg spec last in
	{ valid_out; result = count }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day07" create
;;