open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

  let data_width = 10
  let count_width = 4
  let wires_count = 13
  let joltages_count = 10

  let _part = part

  (* Regarding loader's output - below types of data are sent on the data/count ports
     light bitmap: count = 1
     wire bitmaps: count = n (n groups of wires) 
     joltages    : count = m (m binary numbers representing joltages of the current row)
     *)
  module Loader = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; uart_in : 'a Uart.Byte_with_valid.t
        ; data_ready : 'a
        ; count_ready : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { valid_out : 'a
        ; data : 'a [@bits data_width]
        ; count: 'a [@bits count_width]
        ; last: 'a
        }
      [@@deriving hardcaml]
    end

    let create scope ({ clock; clear; uart_in; data_ready; count_ready } : _ I.t) : _ O.t
      = 
      let spec = Reg_spec.create ~clock ~clear () in

      (*let%hw eol  = uart_in.valid &: (uart_in.value ==: of_char '\n') in*)
      let%hw eof  = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in
      let%hw light_done    = uart_in.valid &: (uart_in.value ==: of_char ']') in
      let%hw wires_done    = uart_in.valid &: (uart_in.value ==: of_char '{') in
      let%hw joltages_done = uart_in.valid &: (uart_in.value ==: of_char '}') in
      
      let%hw light_bitpos = reg_fb spec ~width:(num_bits_to_represent data_width) ~enable:uart_in.valid
                  ~f:(fun x ->  mux2 light_done (zero @@ width x) @@
                                mux2 ((uart_in.value ==: of_char '#') |: (uart_in.value ==: of_char '.')) (x +:. 1) x) in
      let%hw light_bits = reg_fb spec ~width:data_width ~enable:uart_in.valid
                  ~f:(fun x -> mux2 light_done (zero data_width) @@
                               mux2 ((uart_in.value ==: of_char '#') |: (uart_in.value ==: of_char '.')) 
                                    (x |: log_shift ~f:sll ~by:light_bitpos @@ uresize ~width:data_width (uart_in.value ==: of_char '#')) x) in

      let%hw processing_wire    = reg_fb spec ~width:1 ~enable:uart_in.valid ~f:(fun x -> mux2 light_done vdd @@ mux2 wires_done gnd x) in
      let%hw processing_joltage = reg_fb spec ~width:1 ~enable:uart_in.valid ~f:(fun x -> mux2 wires_done vdd @@ mux2 joltages_done gnd x) in

      let%hw bcd  = reg_fb spec ~width:data_width ~enable:uart_in.valid
                  ~f:(fun x -> mux2 ((uart_in.value ==: of_char ',') |: (uart_in.value ==: of_char ')') |: (uart_in.value ==: of_char '}')) (zero data_width) @@
                               mux2 ((uart_in.value >=: of_char '0') &: (uart_in.value <=: of_char '9')) (Util.mul_10 x +: uresize ~width:data_width uart_in.value.:+[0, Some 4]) x) in
    
      let%hw wire_group = reg_fb spec ~width:data_width ~enable:uart_in.valid
                  ~f:(fun x -> mux2 (uart_in.value ==: of_char '(') (zero data_width) @@
                               mux2 (processing_wire &: ((uart_in.value ==: of_char ',') |: (uart_in.value ==: of_char ')')) ) 
                                    (x |: (one data_width |> log_shift ~f:sll ~by:bcd)) x) in
      let%hw wire_group_valid = reg spec (processing_wire &: uart_in.valid &: (uart_in.value ==: of_char ')')) in

      let%hw count = reg_fb spec ~width:count_width ~enable:uart_in.valid
                  ~f:(fun x -> mux2 (wires_done |: joltages_done) (zero count_width) @@
                               mux2 ((processing_joltage &: (uart_in.value ==: of_char ',')) |: (uart_in.value ==: of_char ')')) (x +:. 1) x) in
      
      let%hw data_fifo_wr   = light_done |: 
                              wire_group_valid |: 
                              (processing_joltage &: uart_in.valid &: (uart_in.value ==: of_char ',') |: joltages_done ) (* A joltage number is done *)
                              in
      let%hw data_fifo_d    = mux2 light_done light_bits @@ mux2 processing_wire wire_group bcd in
      let%hw count_fifo_wr  = light_done |: wires_done |: joltages_done in
      let%hw count_fifo_d   = mux2 light_done (one count_width) @@ mux2 processing_joltage (count +:. 1) count in

      let%tydi { q = data_fifo_q; _} =
          Fifo.create
            ~showahead:true
            ~scope:(Scope.sub_scope scope "data_fifo")
            ~capacity:4096
            ~overflow_check:true
            ~underflow_check:true
            ~clock
            ~clear
            ~wr:data_fifo_wr
            ~d:data_fifo_d
            ~rd:data_ready
            ()
      in
      let%tydi { q = count_fifo_q; empty = fifo_empty; _ } =
          Fifo.create
            ~showahead:true
            ~scope:(Scope.sub_scope scope "count_fifo")
            ~capacity:512
            ~overflow_check:true
            ~underflow_check:true
            ~clock
            ~clear
            ~wr:count_fifo_wr
            ~d:count_fifo_d
            ~rd:count_ready
            ()
      in
      let eof_received = reg spec ~enable:eof vdd in
      let fifo_empty_q = reg spec fifo_empty in
      let last = (eof &: fifo_empty) |: (eof_received &: fifo_empty &: ~:fifo_empty_q) in
      { valid_out = ~:fifo_empty; data = data_fifo_q; count = count_fifo_q; last }
    ;;
    let hierarchical scope =
      let module Scoped = Hierarchy.In_scope (I) (O) in
      Scoped.hierarchical ~scope ~name:"loader" create
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

  module States = struct
    type t =
      | Load_light
      | Load_wires
      | Load_joltages
      | Process
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let data_ready = wire 1 in
    let count_ready = wire 1 in
    let%tydi { valid_out = valid_in; data; count; last } = 
      Loader.hierarchical scope { clock; clear; uart_in; data_ready; count_ready }
    in

    let open Always in
    let%hw.Always.State_machine sm = State_machine.create (module States) spec in
    let%hw_var cur_ind = Variable.reg spec ~width:count_width in
    let%hw_var data_fifo_rd = Variable.wire ~default:gnd () in
    let%hw_var count_fifo_rd = Variable.wire ~default:gnd () in
    let%hw_var wire_comb = Variable.reg spec ~width:wires_count in
    compile [ sm.switch
        [ (Load_light, [
            when_ (valid_in) [
              sm.set_next Load_wires;
              data_fifo_rd  <-- vdd;
              count_fifo_rd <-- vdd;
            ]
          ]); (Load_wires, [
            when_ (valid_in) [
              cur_ind                <-- cur_ind.value +:. 1;
              data_fifo_rd           <-- vdd;
              when_ (cur_ind.value ==: (count -:. 1)) [
                sm.set_next Load_joltages;
                count_fifo_rd        <-- vdd;
                cur_ind              <--. 0;
              ]
            ]
          ]); (Load_joltages, [
              when_ (valid_in) [
                cur_ind               <-- cur_ind.value +:. 1;
                data_fifo_rd          <-- vdd;
                when_ (cur_ind.value ==: count -:. 1) [
                  sm.set_next Process;
                  count_fifo_rd        <-- vdd;
                  cur_ind              <--. 0;
                  wire_comb            <--. 1;
                ]
              ]
          ]); (Process, [
              wire_comb              <-- wire_comb.value +:. 1;
              when_ (wire_comb.value ==:. ((Int.pow 2 wires_count) - 1)) [
                sm.set_next Done
              ]
          ]); (Done, [
              sm.set_next Load_light
          ])
        ]
    ];
    Signal.(data_ready <-- data_fifo_rd.value);
    Signal.(count_ready <-- count_fifo_rd.value);
    let%hw light = reg spec ~enable:(sm.is Load_light) data in
    let%hw_array wires = Array.init wires_count ~f:(fun x -> reg_fb spec ~width:data_width ~enable:(sm.is Done |: sm.is Load_wires)
                            ~f:(fun y -> mux2 (sm.is Done) (zero data_width) @@ mux2 (sm.is Load_wires &: (cur_ind.value ==:. x)) data y)) in
    let%hw n = List.init wires_count ~f:(fun x -> mux2 wire_comb.value.:(x) (wires.(x)) (zero data_width)) |>
               reduce ~f:(^:) in
    let%hw light_match = (n ==: light) in
    let%hw min_popcount = reg_fb spec ~width:(num_bits_to_represent wires_count) ~f:(fun x -> mux2 (sm.is Load_joltages) (ones @@ width x) @@
                                                                  mux2 (sm.is Process &: light_match &: (popcount wire_comb.value <: x)) (popcount wire_comb.value) x) in
    let%hw result = reg_fb spec ~width:result_width ~enable:(sm.is Done) ~f:(fun x -> x +: uresize ~width:result_width min_popcount) in
    let%hw last_received = reg spec ~enable:last vdd in
    let%hw valid_out = reg spec (last_received &: sm.is Done) in
    { valid_out; result }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day10" create
  ;;
end