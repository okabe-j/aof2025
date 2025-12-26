open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

  let row_width = 150
  let count_width = 16

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

    (* Extra roll/column of 0 are added to wrap around the input *)
    let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
      = 
      let spec = Reg_spec.create ~clock ~clear () in

      let%hw roll = uart_in.valid &: ((uart_in.value ==: of_char '.') |: (uart_in.value ==: of_char '@')) in
      let%hw eol	= uart_in.valid &: (uart_in.value ==: of_char '\n') in
      let%hw eof  = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in

      let counting_done = reg spec ~enable:eol vdd in
      let count = reg_fb spec ~width:count_width ~enable:(~:counting_done &: roll) ~f:(fun x -> x +:. 1) in

      let%hw shreg = reg_fb spec ~width:row_width ~f:(fun x -> mux2 eol (zero row_width)
      				@@ mux2 roll (drop_top ~width:1 (x @: (uart_in.value ==: of_char '@'))) x) in
      let%hw shreg_valid = eol |: eof in

      let pad1 = reg spec eof in
      let pad2 = reg spec pad1 in
      let valid_out = shreg_valid |: pad1 |: pad2 in
      let row = mux2 shreg_valid (sll ~by:1 shreg) (zero row_width) in

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
    let%tydi { valid_out = valid_in; row; last; _ } = 
        Loader.hierarchical scope { clock; clear; uart_in }
    in
    let%hw row_count = reg_fb spec ~width:count_width ~enable:valid_in ~f:(fun x -> x +:. 1) in

    if String.equal part "part1" then
      let%hw next_r = reg spec ~enable:valid_in row in
      let%hw r      = reg spec ~enable:valid_in next_r in
      let%hw prev_r = reg spec ~enable:valid_in r in

      let%hw      n = List.init (row_width - 2) ~f:(fun x -> x + 1) |> 
                      List.map ~f:(fun x -> mux2 (~:r.:(x)) (ones 4) 
                      (popcount (prev_r.:[x + 1, x - 1] @: r.:(x + 1) @: r.:(x - 1) @: next_r.:[x + 1, x - 1])) ) |>
                      List.map ~f:(fun x -> uresize ~width:(num_bits_to_represent row_width) (x <:. 4)) |>
                      reduce ~f:(+:) in
      let acc = reg_fb spec ~width:result_width ~enable:(valid_in &: (row_count >:. 1)) ~f:(fun x -> x +: (uresize ~width:result_width n)) in
      let valid_out = reg spec last in
      { valid_out; result = acc }
    else
    	let%hw last_received = reg spec ~enable:last vdd in
    	let%hw pipeline_en = valid_in |: last_received in
    	let%hw fifo_out = wire row_width in
    	let%hw pipeline_in = mux2 valid_in row fifo_out in
    	let%hw row_index = reg_fb spec ~width:count_width ~enable:last_received 
    			~f:(fun x -> mux2 (x ==: row_count) (zero count_width) (x +:. 1)) in

    	let%hw r_next = reg spec ~enable:pipeline_en pipeline_in in
    	let%hw r 	    = reg spec ~enable:pipeline_en r_next in
    	let%hw r_prev = reg spec ~enable:pipeline_en r in

    	let%hw_list n_list = List.init (row_width - 2) ~f:(fun x -> x + 1) |> 
    			                 List.map ~f:(fun x -> mux2 (~:r.:(x)) (ones 4) 
    			  	             (popcount (r_prev.:[x + 1, x - 1] @: r.:(x + 1) @: r.:(x - 1) @: r_next.:[x + 1, x - 1])) ) |>
    			                 List.map ~f:(fun x -> x <:. 4) in
    	let%hw n = List.map ~f:(fun x -> uresize ~width:(num_bits_to_represent row_width) x) n_list |> reduce ~f:(+:) in
      let%hw mask = (gnd @: (concat_lsb n_list) @: gnd) in
    	let%hw fifo_in = mask ^: r in

    	let%tydi { q = fifo_q; _ } =
    	    Fifo.create
    	      ~showahead:true
    	      ~scope:(Scope.sub_scope scope "fifo")
    	      ~capacity:150
    	      ~overflow_check:true
    	      ~underflow_check:true
    	      ~clock
    	      ~clear
    	      ~wr:((row_count >:. 1) &: pipeline_en)
    	      ~d:fifo_in
    	      ~rd:(last_received &: pipeline_en)
    	      ()
    	in
      fifo_out <-- fifo_q;

      let%hw count      = reg_fb spec ~width:result_width ~enable:pipeline_en ~f:(fun x -> x +: (uresize ~width:result_width n)) in
      let%hw prev_count = reg spec ~enable:(row_index ==:. 0) count in

      let%hw result_ready = last_received &: (row_index ==:. 0) &: (count ==: prev_count) in
      let%hw result_sent = reg spec ~enable:result_ready vdd in

      { valid_out = (result_ready &: ~:result_sent); result = count }
  ;;


  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day04" create
  ;;
end