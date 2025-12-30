open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

  let bcd_width = 4
  let num_digits = if String.equal part "part1" then 2 else 12
  let digit_count_width = 16

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
        ; bcd : 'a [@bits bcd_width]
        ; count: 'a [@bits digit_count_width]
        ; last: 'a
        }
      [@@deriving hardcaml]
    end

    let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
      = 
      let spec = Reg_spec.create ~clock ~clear () in
      let%hw number_valid    = uart_in.valid &: ((uart_in.value >=: of_char '0') &: (uart_in.value <=: of_char '9')) in 
      let%hw eol  		       = uart_in.valid &: (uart_in.value ==: of_char '\n') in
      let%hw eof             = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in

      let counting_done = reg spec ~enable:eol vdd in
      let count = reg_fb spec ~width:digit_count_width ~enable:(~:counting_done &: number_valid) ~f:(fun x -> x +:. 1) in
      let fifo_rd = wire 1 in
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
    	    ~d:(sel_bottom ~width:bcd_width uart_in.value)
    	    ~rd:fifo_rd
    	    ()
    	in
    	let valid_out = (~:fifo_empty &: counting_done) in
      fifo_rd <-- valid_out;
      let last = reg spec eof in
      { valid_out; bcd = fifo_out; count; last }
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

  let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in

    let%tydi { valid_out = valid_in; bcd; count; last } = 
        Loader.hierarchical scope { clock; clear; uart_in }
    in

    let open Always in 
    let %hw_var shreg        = Variable.reg spec ~width:(num_digits * bcd_width) ~clear_to:(ones (num_digits * bcd_width)) in
    let %hw_var shreg_next   = Variable.wire ~default:shreg.value () in
    let %hw_var valid_digits = Variable.reg spec ~width:digit_count_width in
    let %hw processed_digits = reg_fb spec ~width:digit_count_width ~enable:valid_in
    	~f:(fun x -> mux2 ((x +:. 1) ==: count) (zero digit_count_width) (x +:. 1)) in
    let %hw remaining_digits = (count -: processed_digits) in
    let %hw should_drop = ((sel_bottom ~width:bcd_width shreg.value) >=: bcd) &:
    					(valid_digits.value ==:. num_digits) in

    let %hw max_pop_count_1 = List.init num_digits ~f:(fun x -> shreg.value.:+[x * bcd_width, Some 4] <: bcd) |>
                              concat_lsb |> popcount |> uresize ~width:digit_count_width in
    let %hw max_pop_count_2 = valid_digits.value +: remaining_digits -:. num_digits in

    let %hw pop_count       = mux2 (max_pop_count_1 <: max_pop_count_2) max_pop_count_1 max_pop_count_2 in
    let %hw pop_bits        = sll ~by:2 pop_count in

    (* Using the shreg like a stack, smaller bcd digits at top are popped out if the input is bigger
       However also need to make sure there'll be enough digits in shreg when approaching the end of the input line
    *)

    compile [
      shreg <-- shreg_next.value;

    	when_ (valid_in) [
     		if_ (should_drop) [
          when_ (remaining_digits ==:. 1) [
              valid_digits <--. 0;
              shreg        <-- ones (num_digits * bcd_width)
          ]
     		][
    	 		if_ (pop_count >:. 0) [
    	  			shreg_next   <-- drop_top ~width:bcd_width (log_shift shreg.value ~f:srl ~by:pop_bits |: 
                                 log_shift (ones @@ width shreg.value) ~f:sll ~by:((of_unsigned_int ~width:(width pop_bits) (width shreg.value)) -: pop_bits)) @: bcd;
    	  			valid_digits <-- valid_digits.value -: (uresize ~width:digit_count_width pop_count) +:. 1
    	  		][
    	  			shreg_next   <-- drop_top ~width:bcd_width (shreg.value @: bcd);
    	  			valid_digits <-- valid_digits.value +:. 1;
    	  		];
          when_ (remaining_digits ==:. 1) [
            valid_digits <--. 0;
            shreg        <-- ones (num_digits * bcd_width) 
          ]
     		]
      ]
    ];
    
    let%hw should_convert = valid_in &: (remaining_digits ==:. 1) in
    let    convert_result = Util.bcd_to_binary ~clock ~clear ~output_width:result_width 
      {valid = should_convert; value = shreg_next.value} in 

    let%hw acc = reg_fb spec ~width:result_width ~enable:convert_result.valid
    	~f:(fun x -> x +: convert_result.value) in

    (* Give some cycles to allow bcd -> bin conversion *)
    let valid_out = pipeline spec ~n:20 last in
    {valid_out; result = acc}
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day03" create
  ;;
end
