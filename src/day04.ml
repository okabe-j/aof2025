open! Core
open! Hardcaml
open! Signal

let row_width = 150
let count_width = 16
let result_width = 64

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
      ; count: 'a [@bits count_width]
      ; last: 'a
      }
    [@@deriving hardcaml]
  end

  (* An extra roll/column of 0 are added to wrap around the input *)
  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    = 
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw roll = uart_in.valid &&: ((uart_in.value ==: (of_char '.')) ||: (uart_in.value ==: (of_char '@'))) in
    let%hw eol	= uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
    let%hw eof  = uart_in.valid &&: (uart_in.value ==: (of_unsigned_int ~width:8 4)) in

    let counting_done = reg_fb spec ~width:1 ~f:(fun x -> mux2 eol vdd x) in
    let count = reg_fb spec ~width:count_width ~f:(fun x -> mux2 ((~: counting_done) &&: roll) (x +:. 1) x) in

    let%hw shreg = reg_fb spec 
    	~width:row_width
    	~f:(fun x -> mux2 eol (zero (width x))
    				(mux2 roll (drop_top ~width:1 (x @: (uart_in.value ==: (of_char '@')))) x)) in
    let%hw shreg_valid = eol ||: eof in

    let pad1 = reg spec eof in
    let pad2 = reg spec pad1 in
    let valid_out = shreg_valid ||: pad1 ||: pad2 in
    let row = mux2 shreg_valid (sll ~by:1 shreg) (zero (width shreg)) in

    {valid_out; row; count; last = pad2}
  ;;
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day04_loader" create
  ;;
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid_in : 'a
    ; row : 'a [@bits row_width]
    ; count: 'a [@bits count_width]
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

let create scope ({clock; clear; valid_in; row; last; _} : _ I.t) : _ O.t
  =
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw next_r = reg spec ~enable:valid_in row in
    let%hw r 	  = reg spec ~enable:valid_in next_r in
    let%hw prev_r = reg spec ~enable:valid_in r in

    let%hw row_count = reg_fb spec ~width:count_width ~enable:valid_in ~f:(fun x -> x +:. 1) in

    let%hw_list n_1   = List.init (row_width - 2) ~f:(fun x -> x + 1) |> 
    		  List.map ~f:(fun x -> mux2 (r.:(x) ==: gnd) (ones 4) 
    		  	(popcount (prev_r.:[x + 1, x - 1] @: r.:(x + 1) @: r.:(x - 1) @: next_r.:[x + 1, x - 1])) ) in
    let%hw_list n_2 = 
    		  List.map ~f:(fun x -> mux2 (x <:. 4) vdd gnd) n_1 in
    let%hw n = 
    		  List.map ~f:(fun x -> uresize ~width:(num_bits_to_represent row_width) x) n_2 |>
    		  reduce ~f:(+:) in
    let acc = reg_fb spec ~width:result_width ~f:(fun x -> mux2 (valid_in &&: (row_count >:. 1)) (x +: (uresize ~width:result_width n)) x) in
    let valid_out = reg spec last in
    (*let valid_out = reg spec valid_in in*)
    { valid_out; result = acc }
;;


let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04" create
;;