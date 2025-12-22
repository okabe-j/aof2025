open! Core
open! Hardcaml
open! Signal

let result_width = 64

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
    { 
      valid_out : 'a 
    ; result : 'a [@bits result_width]
    }
  [@@deriving hardcaml]
end
(*
let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
  =
  	let spec = Reg_spec.create ~clock ~clear () in

  	let%hw is_digit        = uart_in.valid &&: ((uart_in.value >=: (of_char '0')) &&: (uart_in.value <=: (of_char '9'))) in 
  	let%hw is_mul_op       = uart_in.valid &&: (uart_in.value ==: (of_char '*')) in
  	let%hw is_add_op       = uart_in.valid &&: (uart_in.value ==: (of_char '+')) in
  	let%hw eol			   = uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
	let%hw eof             = uart_in.valid &&: (uart_in.value ==: (of_unsigned_int ~width:8 4)) in  

	let%hw first_row_done  = reg spec ~enable:eol vdd in
	let    is_digit_d      = reg spec ~enable:uart_in.valid is_digit in

	let%hw number 		   = reg_fb spec ~width:result_width ~enable:uart_in.valid
							 	~f:(fun x -> mux2 (~:is_digit) (zero (width x)) 
							 	((Util.mul_10 x) +: (uresize ~width:(width x) uart_in.value.:[3, 0])) ) in

	let%hw fifo_wr         = uart_in.valid &&: (~: is_digit) &&: is_digit_d in
	let%hw fifo_rd         = uart_in.valid &&: (first_row_done) &&: (fifo_wr ||: is_mul_op ||: is_add_op)  in
	let%hw fifo_add_out    = wire result_width in
	let%hw fifo_mul_out    = wire result_width in

	let%hw fifo_add_in     = mux2 first_row_done (number +: fifo_add_out) number in
	let%hw fifo_mul_in     = mux2 first_row_done (uresize ~width:result_width (number *: fifo_mul_out)) number in

	let%tydi { q = fifo_add_q; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo_add")
	      ~capacity:1024
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:fifo_wr
	      ~d:fifo_add_in
	      ~rd:fifo_rd
	      ()
	in
  	fifo_add_out <-- fifo_add_q;
	let%tydi { q = fifo_mul_q; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo_mul")
	      ~capacity:1024
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:fifo_wr
	      ~d:fifo_mul_in
	      ~rd:fifo_rd
	      ()
	in
  	fifo_mul_out <-- fifo_mul_q;

  	let result = reg_fb spec ~width:result_width ~enable:(is_mul_op ||: is_add_op) 
  					~f: (fun x -> mux2 is_add_op (x +: fifo_add_out) (x +: fifo_mul_out) ) in
  	let valid_out = reg spec eof in
  	{ valid_out; result }
;;
*)
let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
  =
  	let spec = Reg_spec.create ~clock ~clear () in
  	let fifo_data_width    = 16 in
  	let%hw is_digit        = uart_in.valid &&: ((uart_in.value >=: (of_char '0')) &&: (uart_in.value <=: (of_char '9'))) in 
  	let%hw is_mul_op       = uart_in.valid &&: (uart_in.value ==: (of_char '*')) in
  	let%hw is_add_op       = uart_in.valid &&: (uart_in.value ==: (of_char '+')) in
  	let%hw eol			   = uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
	let%hw eof             = uart_in.valid &&: (uart_in.value ==: (of_unsigned_int ~width:8 4)) in

	let%hw first_row_done  = reg spec ~enable:eol vdd in
	let%hw fifo_wr         = uart_in.valid &&: ((~: eol) &&: (~: eof)) in
	let%hw fifo_rd         = fifo_wr &&: first_row_done in
	let%hw fifo_out        = wire fifo_data_width in

	let uart_in_bcd        = uresize ~width:fifo_data_width uart_in.value.:[3, 0] in
	let open Always in
	let%hw_var fifo_in     = Variable.wire ~default:fifo_out () in
	compile [
		if_ (~: first_row_done) [
			if_ (is_digit) [
				fifo_in <-- uart_in_bcd
			][
				fifo_in <-- ones fifo_data_width
			]
		][
			if_ (is_digit) [
				if_ (fifo_out ==: (ones fifo_data_width)) [
					fifo_in <-- uart_in_bcd
				][
					fifo_in <-- (Util.mul_10 fifo_out) +: uart_in_bcd
				]
			][
				fifo_in <-- fifo_out
			]
		]
	];
	let%tydi { q = fifo_q; _ } =
	    Fifo.create
	      ~showahead:true
	      ~scope:(Scope.sub_scope scope "fifo")
	      ~capacity:3750
	      ~overflow_check:true
	      ~underflow_check:true
	      ~clock
	      ~clear
	      ~wr:fifo_wr
	      ~d:fifo_in.value
	      ~rd:fifo_rd
	      ()
	in
  	Signal.(fifo_out <-- fifo_q);
  	let%hw mul_op = reg_fb spec ~width:1 ~f:(fun x -> mux2 is_mul_op vdd (mux2 (fifo_out ==: (ones fifo_data_width)) gnd x)) in
  	let%hw add_op = reg_fb spec ~width:1 ~f:(fun x -> mux2 is_add_op vdd (mux2 (fifo_out ==: (ones fifo_data_width)) gnd x)) in
  	let%hw_var number = Variable.reg spec ~enable:uart_in.valid ~width:result_width in
  	compile [
  		if_ (is_mul_op ||: is_add_op) [
  			number <-- uresize ~width:result_width fifo_out
  		][
   			when_ (add_op) [
  				number <-- number.value +: (uresize ~width:result_width fifo_out)
  			];
  			when_ (mul_op) [
  				number <-- uresize ~width:result_width (number.value *: fifo_out)
  			]
  		];
  		when_ (fifo_out ==: (ones fifo_data_width)) [
  			number <-- zero result_width
  		]
  	];
  	let result = reg_fb spec ~width:result_width ~enable:((uart_in.valid &&: (fifo_out ==: (ones fifo_data_width))) ||: eof)
  					~f:(fun x -> x +: number.value) in
  	let valid_out = reg spec eof in
  	{ valid_out; result } 
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day06" create
;;