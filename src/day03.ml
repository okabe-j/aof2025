open! Core
open! Hardcaml
open! Signal

let bcd_width = 4
let num_digits = 12
let result_width = 64
let digit_count_width = 16

module Loader = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_in : 'a Uart.Byte_with_valid.t
      ; ready: 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a
      ; bcd : 'a [@bits bcd_width]
      ; count: 'a [@bits digit_count_width]
      ; last: 'a
      }
    [@@deriving hardcaml]
  end

  let create scope ({clock; clear; uart_in; ready} : _ I.t) : _ O.t
    = 
    let spec = Reg_spec.create ~clock ~clear () in
    let%hw number_valid    = uart_in.valid &&: ((uart_in.value >=: (of_char '0')) &&: (uart_in.value <=: (of_char '9'))) in 
    let%hw eol  		       = uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
    let%hw eof             = uart_in.valid &&: (uart_in.value ==: (of_unsigned_int ~width:8 4)) in

    let counting_done = reg_fb spec ~width:1 ~f:(fun x -> mux2 eol vdd x) in
    let count = reg_fb spec ~width:digit_count_width ~f:(fun x -> mux2 ((~: counting_done) &&: (number_valid)) (x +:. 1) x) in

  	let%tydi { q = fifo_out; empty = fifo_empty; _ } =
  	  Fifo.create
  	    ~showahead:true
  	    ~scope:(Scope.sub_scope scope "fifo")
  	    ~capacity:1024
  	    ~overflow_check:true
  	    ~underflow_check:true
          ~clock
  	    ~clear
  	    ~wr:number_valid
  	    ~d:uart_in.value.:[bcd_width-1, 0]
  	    ~rd:ready
  	    ()
  	in
  	let valid_out = ((~: fifo_empty) &&: counting_done) in
  	let last_received = reg_fb spec ~width:1 ~f:(fun x -> mux2 eof vdd (mux2 fifo_empty gnd x)) in
  	{valid_out; bcd = fifo_out; count; last = (last_received &&: fifo_empty) }
  ;;
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day03_loader" create
  ;;
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid_in : 'a
    ; bcd : 'a [@bits bcd_width]
    ; count: 'a [@bits digit_count_width]
    ; last: 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { 
      valid_out : 'a 
    ; result : 'a [@bits result_width]
    ; ready : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({clock; clear; valid_in; bcd; count; last} : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in 
  let %hw_var shreg = Variable.reg spec ~width:(num_digits * bcd_width) in
  let %hw_var shreg_next = Variable.wire ~default:shreg.value () in
  let %hw_var valid_digits = Variable.reg spec ~width:digit_count_width in
  let         ready = Variable.wire ~default:gnd () in
  let %hw processed_digits = reg_fb spec ~width:digit_count_width ~enable:(valid_in &&: ready.value)
  	~f:(fun x -> mux2 ((x +:. 1) ==: count) (zero digit_count_width) (x +:. 1)) in
  let %hw remaining_digits = (count -: processed_digits) in
  let %hw should_drop = (shreg.value.:[bcd_width - 1, 0] >=: bcd) &&:
  					(valid_digits.value ==:. num_digits) in
  let %hw should_shift = (shreg.value.:[bcd_width - 1, 0] <: bcd) &&:
  					 (valid_digits.value >:. 0) &&:
  					 (valid_digits.value +: remaining_digits >:. num_digits) in 

  (* Using the shreg like a stack, smaller bcd digits at top are popped out if the input is bigger
     However also need to make sure there'll be enough digits in shreg when approaching the end of the input line
  *)

  compile [
    shreg <-- shreg_next.value;

  	when_ (valid_in) [
   		if_ (should_drop) [
   			ready <-- vdd;
        when_ (remaining_digits ==:. 1) [
            valid_digits <-- (zero digit_count_width);
            shreg <-- (zero (width shreg.value))
        ]
   		][
  	 		if_ (should_shift) [
  	  			shreg_next <-- srl ~by:bcd_width shreg.value;
  	  			valid_digits <-- valid_digits.value -:. 1
  	  		][
  	  			shreg_next <-- drop_top ~width:bcd_width (shreg.value @: bcd);
  	  			valid_digits <-- valid_digits.value +:. 1;
  	  			ready <-- vdd;
            when_ (remaining_digits ==:. 1) [
                valid_digits <-- (zero digit_count_width);
                shreg <-- (zero (width shreg.value))
            ]
  	  		]
   		]
    ]
  ];
  
  let%hw should_convert = valid_in &&: ready.value &&: (remaining_digits ==:. 1) in
  let    convert_result = Util.bcd_to_binary ~clock ~clear ~output_width:result_width 
    {valid = should_convert; value = shreg_next.value} in 

  let%hw acc = reg_fb spec ~width:result_width 
  	~f:(fun x -> mux2 convert_result.valid (x +: convert_result.value) x) in

  (* Give some cycles to allow bcd -> bin conversion *)
  let valid_out = pipeline spec ~n:20 last in
  {valid_out; result = acc; ready =ready.value}
;;


let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day03" create
;;

