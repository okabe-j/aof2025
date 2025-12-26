open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

  let num_width    = 32

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
        ; direction : 'a
        ; circles : 'a [@bits num_width]
        ; offset : 'a [@bits num_width]
        ; last: 'a
        }
      [@@deriving hardcaml]
    end

    let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
      = 
      let num_buffered_byte = 4 in
      let spec = Reg_spec.create ~clock ~clear () in

      let%hw direction_valid = uart_in.valid &: ((uart_in.value ==: of_char 'L') |: (uart_in.value ==: of_char 'R')) in
      let%hw number_valid    = uart_in.valid &: ((uart_in.value >=: of_char '0') &: (uart_in.value <=: of_char '9')) in   
      let%hw eol             = uart_in.valid &: (uart_in.value ==: of_char '\n') in
      let%hw eof             = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in

      let  direction = reg spec ~enable:direction_valid (uart_in.value <>: of_char 'L') in
      let%hw counter = reg_fb spec ~width:(num_bits_to_represent num_buffered_byte)
                  ~f:(fun x -> mux2 (eol |: eof) (zero @@ width x) (mux2 number_valid (x +:. 1) x)) in
      let%hw   shreg = reg_fb spec ~width:(num_buffered_byte * 8)
                  ~f:(fun x -> mux2 (eol |: eof) (zero @@ width x) 
                              (mux2 ~:number_valid x @@ drop_top ~width:8 (x @: uart_in.value))) in
      let     offset = reg spec ~enable:(eol |: eof) ((uresize ~width:num_width shreg.:+[0, Some 4]) +: 
                              (mux2 (counter >:. 1) 
                                (Util.mul_10 @@ uresize ~width:num_width shreg.:+[8, Some 4]) 
                                (zero num_width) )) in
      let    circles = reg spec ~enable:(eol |: eof)
                              (mux2 (counter >:. 2) 
                                (uresize ~width:num_width shreg.:+[16, Some 4]) 
                                (zero num_width)) in

      let valid_out = reg spec (eol |: (eof &: (counter >:. 0))) in
      let last = reg spec eof in 
      {
        valid_out; offset; direction; circles; last
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

  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let%tydi { valid_out = valid_in; direction; circles; offset; last } = 
      Loader.hierarchical scope { clock; clear; uart_in }
    in
    let open Always in 
    (* Convert to 2's complement if rolling left *)
    let%hw signed_offset = mux2 direction offset (~:offset +:. 1) in
    let%hw_var dial = Variable.reg spec ~width:num_width ~clear_to:(of_signed_int ~width:num_width 50) ~enable:valid_in in 
    let%hw dial_next = dial.value +: signed_offset in
    compile [
      dial <-- dial_next;
      when_ (dial_next <+. 0) [
        dial <-- dial_next +:. 100
      ];
      when_ (dial_next >=+. 100) [
        dial <-- dial_next -:. 100
      ]
    ];
    if String.equal part "part1" then
      let valid_in_d = reg spec valid_in in
      let counter = reg_fb spec ~width:result_width ~enable:((dial.value ==:. 0) &: valid_in_d) ~f: (fun x -> x +:. 1) in
      let valid_out = pipeline spec ~n:2 last in
      {valid_out; result = counter}
    else
      let advance_counter = ((dial.value <>:. 0) &: ((dial_next <=:. 0) |: (dial_next >=:. 100))) in
      let counter = reg_fb spec ~width:result_width ~enable:valid_in
        ~f: (fun x -> x +: (uresize ~width:result_width circles) +: 
          (mux2 advance_counter (of_unsigned_int ~width:result_width 1) (zero result_width))) in
      let valid_out = reg spec last in
      {valid_out; result = counter}
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day01" create
  ;;
end
