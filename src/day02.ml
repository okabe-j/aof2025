open! Core
open! Hardcaml
open! Signal

let result_width = 64
let max_digits = 10
let bcd_width = 4

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
      ; range_begin : 'a [@bits max_digits * bcd_width]
      ; range_end : 'a [@bits max_digits * bcd_width]
      ; last: 'a
      }
    [@@deriving hardcaml]
  end

  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    = 
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw eof   = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in
    let%hw comma = uart_in.valid &: (uart_in.value ==: of_char ',') in
    let%hw dash  = uart_in.valid &: (uart_in.value ==: of_char '-') in

    let shreg = reg_fb spec 
    				~width:(max_digits * bcd_width) 
    				~enable:uart_in.valid 
    				~f:(fun x -> mux2 (comma |: dash |: eof) 
    								  (zero @@ width x) 
    								  (drop_top ~width:bcd_width x @: sel_bottom ~width:bcd_width uart_in.value)
    				) in

    let range_begin = reg spec ~enable:dash shreg in
    let range_end = reg spec ~enable:(comma |: eof) shreg in
    let valid_out = reg spec comma |: eof in
    let last = reg spec eof in
    { valid_out ; range_begin; range_end; last }
  ;;
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day02_loader" create
  ;;
end

let bcd_add_one bcd = 
	let bitmap = List.init max_digits ~f:(fun x -> bcd.:+[x * bcd_width, Some bcd_width] <>:. 9) |>
			     concat_lsb in

	let index  = (trailing_zeros bitmap) @: (of_string "00") in
	let pos    = log_shift (one @@ width bcd) ~f:sll ~by:index in
	(bcd +: pos) &: ~:(pos -:. 1)
;;


let rec check_repeat_twice l =
	match l with 
	| []     -> vdd
	| [a; b] -> (a ==: b) &&: (sel_top a ~width:bcd_width <>:. 0)
	| h :: t -> (no_bits_set h) &&: (check_repeat_twice t)
;;  

let check_bcd bcd = 
	List.init 5 ~f:(fun x -> (x + 1) * bcd_width) |>
	List.map ~f:(fun x -> (split_lsb ~part_width:x ~exact:false bcd |> List.rev |> check_repeat_twice )) |>
	reduce ~f:(|:)
;;

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid_in : 'a
    ; range_begin : 'a [@bits max_digits * bcd_width]
    ; range_end : 'a [@bits max_digits * bcd_width]
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

let create scope ({clock; clear; valid_in; range_begin; range_end; last} : _ I.t) : _ O.t
  =
	let spec = Reg_spec.create ~clock ~clear () in
	let%hw fifo_rd = wire 1 in
	let%tydi { q = fifo_begin_q; empty = fifo_empty; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo_begin")
	      ~capacity:128
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:valid_in
	      ~d:range_begin
	      ~rd:fifo_rd
	      ()
	in
	let%tydi { q = fifo_end_q; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo_end")
	      ~capacity:128
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:valid_in
	      ~d:range_end
	      ~rd:fifo_rd
	      ()
	in
	let open Always in
	let%hw_var bcd_valid = Variable.reg spec ~width:1 in
	let%hw_var bcd       = Variable.reg spec ~width:(max_digits * bcd_width) in
	compile [
		if_ (~: (bcd_valid.value)) [
			when_ (~: fifo_empty) [
				bcd_valid 	<-- vdd;
				bcd 		<-- fifo_begin_q
			]
		][
			bcd <-- bcd_add_one bcd.value;
			when_ (bcd.value ==: fifo_end_q) [
				bcd 		<--. 0;
				bcd_valid 	<-- gnd
			]
		]
	];
	Signal.(fifo_rd <-- ((bcd.value ==: fifo_end_q) &&: ~:fifo_empty));
	let%hw fifo_bcd_rd = wire 1 in
	let%hw fifo_bcd_wr = bcd_valid.value &&: (check_bcd bcd.value) in
	let%tydi { q = fifo_bcd_q; empty = fifo_bcd_empty; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo_bcd")
	      ~capacity:128
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:fifo_bcd_wr
	      ~d:bcd.value
	      ~rd:fifo_bcd_rd
	      ()
	in
	let bcd_to_binary_valid = wire 1 in
	let bin = Util.bcd_to_binary ~clock ~clear ~output_width:result_width 
		{ valid = bcd_to_binary_valid; value = fifo_bcd_q } in
	let processing = reg_fb spec ~width:1 
		~f:(fun x -> mux2 x (mux2 bin.valid gnd x) (mux2 bcd_to_binary_valid vdd x) ) in
	Signal.(fifo_bcd_rd <-- bin.valid);
	Signal.(bcd_to_binary_valid <-- (~: (processing ||: fifo_bcd_empty)));

	let result = reg_fb spec ~width:result_width ~enable:bin.valid ~f:(fun x -> x +: bin.value) in
	let last_received = reg spec ~enable:last vdd in
	let valid_out_sent = wire 1 in
	let valid_out = reg_fb spec ~width:1 ~f:(fun x -> mux2 x gnd (mux2 (last_received &&: fifo_empty &&: fifo_bcd_empty &&: ~:valid_out_sent) vdd x)) in
	let valid_out_sent_reg = reg spec ~enable:valid_out vdd in
	Signal.(valid_out_sent <-- valid_out_sent_reg);
	{ valid_out; result }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day02" create
;;