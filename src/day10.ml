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
  let press_count_width = 16
  let pattern_index_width = 5

  let pattern_ram_addr_width = data_width
  let pattern_ram_data_width = 17 * wires_count
  let stack_ram_addr_width = 8

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

  module States_part1 = struct
    type t =
      | Load_light
      | Load_wires
      | Load_joltages
      | Process
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module States_part2 = struct
    type t =
      | Load_light
      | Load_wires
      | Load_joltages
      | Pattern_ram_rd
      | Pattern_ram_wr
      | Load_params
      | Process_pattern
      | Push_stack
      | Pop_stack1
      | Pop_stack2
      | Clear_pattern_ram
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Ram_Port = struct
    type 'a t =
      { address : 'a [@bits pattern_ram_addr_width]
      ; write_data : 'a [@bits pattern_ram_data_width]
      ; write_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  module Stack_Frame = struct
    type 'a t =
      { joltages : 'a [@bits joltages_count * data_width]
      ; min_press : 'a [@bits press_count_width]
      ; pattern_index : 'a [@bits pattern_index_width]
      }
    [@@deriving hardcaml]

    let width = joltages_count * data_width + press_count_width + pattern_index_width
  end  

  let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let data_ready = wire 1 in
    let count_ready = wire 1 in
    let%tydi { valid_out = valid_in; data; count; last } = 
      Loader.hierarchical scope { clock; clear; uart_in; data_ready; count_ready }
    in

    if String.equal part "part1" then
      let open Always in
      let%hw.Always.State_machine sm = State_machine.create (module States_part1) spec in
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

    else (* part2 *)
      let pattern_ram_port = { Ram_Port.address   = wire pattern_ram_addr_width; 
                                   write_data     = wire pattern_ram_data_width;
                                   write_enable   = wire 1 } in 
      let stack_ram_port   = { Ram_Port.address   = wire stack_ram_addr_width; 
                                   write_data     = wire Stack_Frame.width;
                                   write_enable   = wire 1 } in                                    

      let pattern_ram_rdata =
          Ram.create
            ~name:"ram_pattern"
            ~collision_mode:Read_before_write
            ~size:(Int.pow 2 pattern_ram_addr_width)
            ~write_ports:
                [| { Hardcaml.Write_port.write_clock = clock
                 ; write_address = pattern_ram_port.address
                 ; write_enable  = pattern_ram_port.write_enable
                 ; write_data    = pattern_ram_port.write_data
                 }|]    
            ~read_ports: 
               [| { Hardcaml.Read_port.read_clock = clock
                ; read_address   = pattern_ram_port.address
                ; read_enable    = ~:(pattern_ram_port.write_enable)
                } |]
            ()
      in
      let stack_ram_rdata =
          Ram.create
            ~name:"ram_stack"
            ~collision_mode:Read_before_write
            ~size:(Int.pow 2 stack_ram_addr_width)
            ~write_ports:
                [| { Hardcaml.Write_port.write_clock = clock
                 ; write_address = stack_ram_port.address
                 ; write_enable  = stack_ram_port.write_enable
                 ; write_data    = stack_ram_port.write_data
                 }|]    
            ~read_ports: 
               [| { Hardcaml.Read_port.read_clock = clock
                ; read_address   = stack_ram_port.address
                ; read_enable    = ~:(stack_ram_port.write_enable)
                } |]
            ()
      in

      let open Always in
      let%hw.Always.State_machine sm = State_machine.create (module States_part2) spec in

      let%hw_var data_fifo_rd   = Variable.wire ~default:gnd () in
      let%hw_var count_fifo_rd  = Variable.wire ~default:gnd () in
      let%hw_var wire_comb      = Variable.reg spec ~width:wires_count in
      let%hw_var clear_addr     = Variable.reg spec ~width:pattern_ram_addr_width in

      (* Stack related *)
      let stack_frame_rd = Stack_Frame.Of_signal.unpack stack_ram_rdata.(0) in
      let%hw_var min_press_count = Variable.reg spec ~width:press_count_width in

      let%hw_var sp = Variable.reg spec ~width:stack_ram_addr_width in
      let%hw_var ret_valid = Variable.reg spec ~width:1 in
      let%hw     ret_value = reg spec ~enable:(sm.is Pop_stack1) min_press_count.value in

      (* Wires *)
      let%hw_var valid_wires_count = Variable.reg spec ~width:(num_bits_to_represent wires_count) in
      let%hw     max_wire_comb = ~:(log_shift ~f:sll ~by:valid_wires_count.value @@ ones wires_count) in
      let%hw_array wires    = Array.init wires_count ~f:(
                              fun x -> reg_fb spec 
                                              ~width:data_width 
                                              ~enable:(sm.is Done |: sm.is Load_wires)
                                              ~f:(fun y -> mux2 (sm.is Done) (zero data_width) @@ 
                                                           mux2 (sm.is Load_wires &: (valid_wires_count.value ==:. x)) data y)
                           ) in

      (* Pattern Ram *)
      let%hw pattern_wr    = List.init wires_count ~f:(fun x -> mux2 wire_comb.value.:(x) (wires.(x)) (zero data_width)) |>
                             reduce ~f:(^:) in

      let%hw_list patterns_rd   = split_lsb ~part_width:wires_count pattern_ram_rdata.(0) in
      let%hw_var pattern_index  = Variable.reg spec ~width:pattern_index_width in
      let%hw processing_pattern = mux pattern_index.value patterns_rd in

      (* Joltages *)
      let%hw_var valid_joltages_count = Variable.reg spec ~width:(num_bits_to_represent joltages_count) in
      let%hw     max_joltages_count = ~:(log_shift ~f:sll ~by:valid_joltages_count.value @@ ones joltages_count) in

      let%hw_list processed_joltages = List.init joltages_count ~f:(fun _ -> wire data_width) in
      let%hw_list joltages = List.init joltages_count ~f:(
                              fun x -> reg_fb spec 
                                              ~width:data_width 
                                             (* ~enable:(sm.is Done |: sm.is Load_joltages |: sm.is Push_stack |: sm.is) *)
                                              ~f:(fun y -> mux2 (sm.is Done) (zero data_width) @@ 
                                                           mux2 (sm.is Load_joltages &: (valid_joltages_count.value ==:. x)) data @@
                                                           mux2 (sm.is Push_stack) (List.nth_exn processed_joltages x) @@
                                                           mux2 (sm.is Pop_stack2) (List.nth_exn (split_lsb ~part_width:data_width stack_frame_rd.joltages) x) 
                                                           y)
                           ) in
      let%hw joltages_pattern = List.map joltages ~f:(fun x -> x.:(0)) |> concat_lsb in

      let%hw_list pressed_sum  = split_lsb ~part_width:1 processing_pattern |>
                                  List.mapi ~f:(fun i x -> split_lsb ~part_width:1 (mux2 x wires.(i) (zero data_width)) ) |>
                                  reduce ~f:(fun x y -> List.map2_exn ~f:(fun a b -> a @: b) x y) |>
                                  List.map ~f:(fun x -> uresize ~width:data_width @@ popcount x) in
      let%hw is_pattern_valid = List.map2_exn ~f:(fun x y -> x >=: y) joltages pressed_sum |> reduce ~f:(&:) in

      let%hw     all_pattern_processed = wire 1 in
      let%hw     need_push_stack = wire 1 in

      let%hw num_press           = uresize ~width:press_count_width @@ popcount processing_pattern in
      let%hw num_press_from_ret  = (sll ~by:1 ret_value) +: num_press in

      let%hw joltage_process_done = ((concat_lsb processed_joltages) ==:. 0) &: ~:all_pattern_processed &: is_pattern_valid in

      compile [ sm.switch
          [ (Load_light, [
              when_ (valid_in) [
                sm.set_next Load_wires;
                data_fifo_rd  <-- vdd;
                count_fifo_rd <-- vdd
              ]
            ]); (Load_wires, [
              when_ (valid_in) [
                data_fifo_rd           <-- vdd;
                valid_wires_count      <-- valid_wires_count.value +:. 1;
                when_ (valid_wires_count.value ==: (count -:. 1)) [
                  sm.set_next Load_joltages;
                  count_fifo_rd        <-- vdd
                ]
              ]
            ]); (Load_joltages, [
                when_ (valid_in) [
                  valid_joltages_count  <-- valid_joltages_count.value +:. 1;
                  data_fifo_rd          <-- vdd;
                  when_ (valid_joltages_count.value ==: count -:. 1) [
                    sm.set_next Pattern_ram_rd;
                    count_fifo_rd        <-- vdd;
                    wire_comb            <-- max_wire_comb
                  ]
                ]
            ]); (Pattern_ram_rd, [
                sm.set_next Pattern_ram_wr
            ]); (Pattern_ram_wr, [
                sm.set_next Pattern_ram_rd;
                wire_comb              <-- wire_comb.value -:. 1;
                when_ (wire_comb.value ==:. 0) [
                  sm.set_next Load_params;
                  min_press_count      <-- ones press_count_width
                ]
            ]); (Load_params, [
                sm.set_next Process_pattern;

            ]); (Process_pattern, [
                sm.set_next Process_pattern;
                pattern_index         <-- pattern_index.value +:. 1;
                ret_valid             <-- gnd;
                min_press_count       <-- mux2 (joltage_process_done &: (num_press <: min_press_count.value)) num_press @@
                                          mux2 (ret_valid.value &: (ret_value <>: (ones press_count_width)) &: (num_press_from_ret <: min_press_count.value)) num_press_from_ret
                                          min_press_count.value; 

                if_ (all_pattern_processed) [
                  if_ (sp.value >:. 0) [
                    sm.set_next Pop_stack1;
                    sp <-- sp.value -:. 1
                  ][
                    sm.set_next Clear_pattern_ram
                  ]
                ][
                  when_ (need_push_stack) [
                    sm.set_next Push_stack;
                    pattern_index <-- pattern_index.value;
                  ]
                ]
            ]); (Push_stack, [
              sm.set_next Load_params;
              sp                   <-- sp.value +:. 1;
              min_press_count      <-- ones press_count_width;
              pattern_index        <--. 0
            ]); (Pop_stack1, [
              sm.set_next Pop_stack2;
              ret_valid            <-- vdd
            ]); (Pop_stack2, [
              sm.set_next Load_params;
              pattern_index       <-- stack_frame_rd.pattern_index;
              min_press_count     <-- stack_frame_rd.min_press              

            ]); (Clear_pattern_ram, [
                clear_addr            <-- clear_addr.value +:. 1;
                when_ (clear_addr.value ==: max_joltages_count) [
                  sm.set_next Done
                ]
            ]); (Done, [
                sm.set_next Load_light;
                valid_wires_count    <--. 0;
                valid_joltages_count <--. 0;
                wire_comb            <--. 0;
                clear_addr           <--. 0;
                sp                   <--. 0;
                pattern_index        <--. 0
            ])  
          ]
      ];
      Signal.(data_ready <-- data_fifo_rd.value);
      Signal.(count_ready <-- count_fifo_rd.value);

      let _processed_joltages = List.map2_exn ~f:(fun x y -> srl ~by:1 (x -: y)) joltages pressed_sum in
      List.iteri processed_joltages  ~f:(fun i x -> Signal.(x <-- List.nth_exn _processed_joltages i));

      (* 0 press is a valid only when pattern itself is 0, however it must store in the first pattern_index *)
      let%hw _all_pattern_processed = ((processing_pattern ==:. 0) &: ((pattern_index.value <>:. 0) |: (joltages_pattern <>:. 0))) in
      let%hw _need_push_stack = (List.map ~f:(fun x -> x <>:. 0) processed_joltages |> reduce ~f:(|:)) &: is_pattern_valid &: ~:(ret_valid.value) in

      let stack_frame_wr = {
        Stack_Frame.joltages = concat_lsb joltages;
        min_press = min_press_count.value;
        pattern_index = pattern_index.value
      } in

      Signal.(all_pattern_processed          <-- _all_pattern_processed );
      Signal.(need_push_stack                <-- _need_push_stack );

      Signal.(pattern_ram_port.address       <-- (mux2 (sm.is Clear_pattern_ram) clear_addr.value @@
                                                  mux2 (sm.is Pattern_ram_wr |: sm.is Pattern_ram_rd) pattern_wr joltages_pattern ));
      Signal.(pattern_ram_port.write_data    <-- (mux2 (sm.is Clear_pattern_ram) (zero pattern_ram_data_width)
                                                                                 (drop_top ~width:wires_count pattern_ram_rdata.(0) @: wire_comb.value)));
      Signal.(pattern_ram_port.write_enable  <-- (sm.is Pattern_ram_wr |: sm.is Clear_pattern_ram));

      Signal.(stack_ram_port.address         <-- sp.value);
      Signal.(stack_ram_port.write_data      <-- (Stack_Frame.Of_signal.pack stack_frame_wr));
      Signal.(stack_ram_port.write_enable    <-- (sm.is Push_stack));  

      let%hw_list _pattern_ram_wr_data = split_lsb ~part_width:wires_count pattern_ram_port.write_data in

      let%hw result = reg_fb spec ~width:result_width ~enable:(sm.is Process_pattern &: (sp.value ==:. 0) &: all_pattern_processed)
                        ~f:(fun x -> x +: (uresize ~width:result_width min_press_count.value)) in

      let%hw last_received = reg spec ~enable:last vdd in
      let%hw valid_out = reg spec (last_received &: sm.is Done) in
      { valid_out; result }

  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day10" create
  ;;
end