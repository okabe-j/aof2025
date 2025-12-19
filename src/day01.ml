open! Core
open! Hardcaml
open! Signal

module Byte_with_valid = With_valid.Vector (struct
    let width = 8
  end)

let num_width    = 32
let output_width = 64
let ascii_eof    = (of_unsigned_int ~width:8 4)
let term         = (of_string "11111111")
let mul_10       = fun x -> (sll x ~by:3) +: (sll x ~by:1)

module Loader = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; uart_in : 'a Byte_with_valid.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { valid_out : 'a
      ; direction : 'a
      ; number : 'a [@bits num_width]
      ; last: 'a
      }
    [@@deriving hardcaml]
  end

  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    = 
    let num_buffered_byte = 4 in
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw direction_valid = uart_in.valid &&: ((uart_in.value ==: (of_char 'L')) ||: (uart_in.value ==: (of_char 'R'))) in
    let%hw number_valid    = uart_in.valid &&: ((uart_in.value >=: (of_char '0')) &&: (uart_in.value <=: (of_char '9'))) in   
    let%hw eol             = uart_in.valid &&: (uart_in.value ==: (of_char '\n')) in
    let%hw eof             = uart_in.valid &&: (uart_in.value ==: ascii_eof) in

    let  direction = reg spec ~enable:direction_valid (mux2 (uart_in.value ==: (of_char 'L')) gnd vdd) in
    let%hw counter = reg_fb spec ~width:(num_bits_to_represent num_buffered_byte)
                ~f:(fun x -> mux2 (eol ||: eof) (zero (width x)) (mux2 number_valid (x +:. 1) x)) in
    let%hw   shreg = reg_fb spec ~width:(num_buffered_byte * 8)
                ~f:(fun x -> mux2 (eol ||: eof) (zero (width x)) 
                            (mux2 number_valid (drop_top ~width:8 (x @: uart_in.value)) x)) in
    let     number = reg spec ~enable:(eol ||: eof) ((uresize ~width:num_width shreg.:[3, 0]) +: 
                            (mux2 (counter >:. 1) 
                              (mul_10 (uresize ~width:num_width shreg.:[11, 8])) 
                              (zero num_width) )) in

    let valid_out = reg spec (eol ||: (eof &&: (counter >:. 0))) in
    let last = reg spec eof in 
    {
      valid_out; number; direction; last
    }
  ;;
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day01_loader" create
  ;;
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; valid_in : 'a
    ; direction : 'a
    ; number : 'a [@bits num_width]
    ; last: 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { 
      valid_out : 'a 
    ; count : 'a [@bits output_width]
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in 
  let%hw signed_number = mux2 i.direction i.number (~:(i.number) +: (of_unsigned_int ~width:32 1)) in
  let%hw_var dial = Variable.reg spec 
  	~width: num_width 
  	~clear_to: (of_signed_int ~width:num_width 50) 
    ~enable: i.valid_in in 
  let%hw dial_next = dial.value +: signed_number in
  compile [
    dial <-- dial_next;
  	when_ (dial_next <+. 0) [
 		dial <-- dial_next +:. 100
  	] ;
  	when_ (dial_next >=+. 100) [
  		dial <-- dial_next -:. 100
  	]
  ];
  let valid_in_d = reg spec i.valid_in in
  let counter = reg_fb spec ~width: output_width 
  	~enable: ((dial.value ==:. 0) &: (valid_in_d)) 
  	~f: (fun d -> d +:. 1) in
  let valid_out = pipeline spec ~n:2 i.last in
  {valid_out; count = counter}
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day01" create
;;
