open! Core
open! Hardcaml
open! Signal

let result_width = 64
let bcd_width = 4
let ram_addr_width = 8
let ram_depth = Int.pow 2 ram_addr_width

(*
module Ram = Loadable_pseudo_dual_port_ram.Make (struct
    let width = result_width
    let depth = ram_depth
    let zero_on_startup = false
    let num_ports = 2
  end)
*)

module TDP_Ram = Ram.Dual_port.Make (struct
  let address_bits = ram_addr_width
  let data_bits    = result_width
end)

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
      { range_begin : 'a [@bits result_width]
      ; range_end : 'a [@bits result_width]
      ; range_valid: 'a
      ; id: 'a [@bits result_width]
      ; id_valid: 'a
      ; last: 'a
      }
    [@@deriving hardcaml]
  end
  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    = 
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw eof   = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in
    let%hw dash  = uart_in.valid &: (uart_in.value ==: of_char '-') in
    let%hw eol   = uart_in.valid &: (uart_in.value ==: of_char '\n') in

    let%hw eol_r = reg spec ~enable:uart_in.valid eol in
    let%hw load_id = reg spec ~enable:(eol &: eol_r) vdd in

    let shreg = reg_fb spec 
          ~width:result_width
          ~enable:uart_in.valid 
          ~f:(fun x -> mux2 (dash |: eol) 
                    (zero @@ width x) 
                    ((Util.mul_10 x) +: (uresize ~width:(width x) @@ sel_bottom ~width:bcd_width uart_in.value))
          ) in  

    let range_begin   = reg spec ~enable:dash shreg in
    let range_end     = reg spec ~enable:eol shreg in
    let range_valid   = reg spec (~:load_id &: eol &: ~:eol_r) in
    let id            = reg spec ~enable:(eol |: eof) shreg in
    let id_valid      = reg spec (load_id &: (eol |: eof)) in
    let last          = reg spec eof in

    { range_begin; range_end; range_valid; id; id_valid; last }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day05_loader" create
  ;;

end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; range_begin : 'a [@bits result_width]
    ; range_end : 'a [@bits result_width]
    ; range_valid : 'a
    ; id : 'a [@bits result_width]
    ; id_valid : 'a
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

(*
let create scope ({clock; clear; range_begin; range_end; range_valid; id; id_valid; last} : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in

  let%hw range_count = reg_fb spec ~width:ram_addr_width ~enable:range_valid ~f:(fun x -> x +:. 1) in
  let%hw ram_load_done  = reg spec ~enable:last vdd in
  let ram_port_begin = { Ram.Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 } in
  let ram_port_end   = { Ram.Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 } in
  let%tydi { read_data = ram_rdata_begin } =
    Ram.hierarchical
      ~name:"ram_rbegin"
      scope
      { clock
      ; clear
      ; load_ports = 
        [| { address = range_count
           ; write_data = range_begin
           ; write_enable = range_valid
           }
           ; Ram.Port.unused 
        |]
      ; load_finished = ram_load_done
      ; ram_ports = [|ram_port_begin; Ram.Port.unused |]
      }
  in
  let%tydi { read_data = ram_rdata_end } =
    Ram.hierarchical
      ~name:"ram_rend"
      scope
      { clock
      ; clear
      ; load_ports = 
        [| { address = range_count
           ; write_data = range_end
           ; write_enable = range_valid
           }
           ; Ram.Port.unused 
        |]
      ; load_finished = ram_load_done
      ; ram_ports = [|ram_port_end; Ram.Port.unused |]
      }
  in
  let%hw fifo_rd = wire 1 in
  let%tydi { q = fifo_id_q; empty = fifo_id_empty; _ } =
      Fifo.create
        ~showahead:true
        ~scope:(Scope.sub_scope scope "id_fifo")
        ~capacity:1024
        ~overflow_check:true
        ~underflow_check:true
        ~clock
        ~clear
        ~wr:id_valid
        ~d:id
        ~rd:fifo_rd
        ()
  in
  let fifo_rd_r = reg spec fifo_rd in
  let%hw range_match = (fifo_id_q >=: ram_rdata_begin.(0)) &&: (fifo_id_q <=: ram_rdata_end.(0) &&: ~:fifo_rd_r) in

  let%hw range_addr = reg_fb spec ~width:ram_addr_width ~enable:ram_load_done 
                    ~f:(fun x -> mux2 fifo_rd (zero ram_addr_width) (x +:. 1) ) in

  ram_port_begin.address         <-- range_addr;
  ram_port_begin.write_data      <-- (zero result_width);
  ram_port_begin.write_enable    <-- gnd;
  ram_port_end.address           <-- range_addr;
  ram_port_end.write_data        <-- (zero result_width);
  ram_port_end.write_enable      <-- gnd;  
  fifo_rd                        <-- (((range_addr ==: range_count) ||: range_match) &&: ram_load_done &&: ~:fifo_id_empty);

  let result = reg_fb spec ~width:result_width ~enable:(ram_load_done &&: range_match)
                  ~f:(fun x -> x +:. 1) in
  let valid_out = reg spec (ram_load_done &&: fifo_id_empty &&: fifo_rd_r) in 
  { valid_out; result }
;;
*)

(*
module States = struct
  type t =
    | Ram_load
    | Ram_sort_read
    | Ram_sort_write
    | Ram_count_read
    | Ram_count_process
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({clock; clear; range_begin; range_end; range_valid; last; _ } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let%hw range_count = reg_fb spec ~width:ram_addr_width ~enable:range_valid ~f:(fun x -> x +:. 1) in
  let%hw ram_load_done  = reg spec ~enable:last vdd in
  let ram_port_begin = Array.init 2 ~f:(fun _ -> { Ram.Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 }) in
  let ram_port_end   = Array.init 2 ~f:(fun _ -> { Ram.Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 }) in
  let%tydi { read_data = ram_rdata_begin } =
    Ram.hierarchical
      ~name:"ram_rbegin"
      scope
      { clock
      ; clear
      ; load_ports = 
        [| { address = range_count
           ; write_data = range_begin
           ; write_enable = range_valid
           }
           ; Ram.Port.unused 
        |]
      ; load_finished = ram_load_done
      ; ram_ports = ram_port_begin
      }
  in
  let%tydi { read_data = ram_rdata_end } =
    Ram.hierarchical
      ~name:"ram_rend"
      scope
      { clock
      ; clear
      ; load_ports = 
        [| { address = range_count
           ; write_data = range_end
           ; write_enable = range_valid
           }
           ; Ram.Port.unused 
        |]
      ; load_finished = ram_load_done
      ; ram_ports = ram_port_end
      }
  in
  let open Always in
  let%hw.Always.State_machine sm = State_machine.create (module States) spec in
  (* Variables for sorting stage *)
  let%hw_var bsort_addr_outer = Variable.reg spec ~width:ram_addr_width in
  let%hw_var bsort_addr_inner = Variable.reg spec ~width:ram_addr_width in
  let%hw_var mem_write_enable = Variable.wire ~default:gnd () in
  let%hw bsort_need_swap = mux2 (ram_rdata_begin.(0) ==: ram_rdata_begin.(1)) 
                                (ram_rdata_end.(0) >: ram_rdata_end.(1)) 
                                (ram_rdata_begin.(0) >: ram_rdata_begin.(1)) in
  (* Variables for counting stage *)
  let%hw_var count_addr_lower = Variable.reg spec ~width:ram_addr_width in
  let%hw_var count_addr_upper = Variable.reg spec ~width:ram_addr_width in
  let%hw should_count = (ram_rdata_begin.(1) >: ram_rdata_end.(0)) |: (count_addr_upper.value ==: range_count) in
  let%hw merged_range_end = mux2 (ram_rdata_end.(0) >: ram_rdata_end.(1)) ram_rdata_end.(0) ram_rdata_end.(1) in
  compile
    [ sm.switch
        [ (Ram_load, [
              when_ (ram_load_done) [
                sm.set_next Ram_sort_read;
                bsort_addr_outer <-- range_count -:. 2
              ]
          ]); (Ram_sort_read, [
              sm.set_next Ram_sort_write
          ]); (Ram_sort_write, [
              sm.set_next Ram_sort_read;
              mem_write_enable <-- vdd;
              bsort_addr_inner <-- bsort_addr_inner.value +:. 1;
              when_ (bsort_addr_inner.value ==: bsort_addr_outer.value) [
                bsort_addr_inner <--. 0;
                bsort_addr_outer <-- bsort_addr_outer.value -:. 1;
                when_ (bsort_addr_outer.value ==:. 0) [
                  sm.set_next Ram_count_read;
                  count_addr_upper <--. 1;
                ]
              ]
          ]); (Ram_count_read, [
              sm.set_next Ram_count_process
          ]); (Ram_count_process, [
              mem_write_enable <-- vdd;
              when_ (should_count) [
                count_addr_lower <-- count_addr_upper.value
              ];
              if_ (count_addr_upper.value <: range_count) [
                sm.set_next Ram_count_read;
                count_addr_upper <-- count_addr_upper.value +:. 1
              ][
                sm.set_next Done;
                count_addr_upper <--. 0;
                count_addr_lower <--. 0;
              ]
          ]); (Done, [
          ])
        ]
    ];

  let%hw is_sorting_state = (sm.is Ram_sort_read) |: (sm.is Ram_sort_write) in
  let%hw ram_addr_0 = mux2 is_sorting_state  bsort_addr_inner.value        count_addr_lower.value in
  let%hw ram_addr_1 = mux2 is_sorting_state (bsort_addr_inner.value +:. 1) count_addr_upper.value in

  Signal.(ram_port_begin.(0).address         <-- ram_addr_0);
  Signal.(ram_port_begin.(0).write_data      <-- (mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(1) ram_rdata_begin.(0)));
  Signal.(ram_port_begin.(0).write_enable    <-- mem_write_enable.value);

  Signal.(ram_port_begin.(1).address         <-- ram_addr_1);
  Signal.(ram_port_begin.(1).write_data      <-- (mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(0) ram_rdata_begin.(1)));
  Signal.(ram_port_begin.(1).write_enable    <-- mem_write_enable.value);

  Signal.(ram_port_end.(0).address           <-- ram_addr_0);
  Signal.(ram_port_end.(0).write_data        <-- (mux2 is_sorting_state 
                                              (mux2 bsort_need_swap ram_rdata_end.(1) ram_rdata_end.(0)) 
                                              (mux2 should_count ram_rdata_end.(0) merged_range_end)));
  Signal.(ram_port_end.(0).write_enable      <-- mem_write_enable.value);

  Signal.(ram_port_end.(1).address           <-- ram_addr_1);
  Signal.(ram_port_end.(1).write_data        <-- (mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_end.(0) ram_rdata_end.(1)));
  Signal.(ram_port_end.(1).write_enable      <-- mem_write_enable.value);

  let result = reg_fb spec ~width:result_width ~enable:((sm.is Ram_count_process) &: should_count)
                ~f:(fun x -> x +: (ram_rdata_end.(0) -: ram_rdata_begin.(0)) +:. 1) in

  let valid_out = reg spec ((sm.is Ram_count_process) &: (count_addr_upper.value ==: range_count)) in
  { valid_out; result }
;;
*)

(*
(* The RTL inference of TDP memory on yosys ECP5 flow is extremely picky.
   -> The memory address must be coming from a register directly without muxing / math logic
   -> 2 ports must both perform read or write at the same cycle 
      (?? I don't fully understand this - TDP memory isn't inferred if I change mem_write_enable to an array to allow a different read/write mode per port, 
      maybe related to how Hardcaml is generating Verilog code)
   -> Tried the Ram.Dual_port module but even that doesn't work
*)
module States = struct
  type t =
    | Ram_load
    | Ram_sort_read
    | Ram_sort_write
    | Ram_count_read
    | Ram_count_process
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module Ram_Port = struct
  type 'a t =
    { address : 'a [@bits ram_addr_width]
    ; write_data : 'a [@bits result_width]
    ; write_enable : 'a
    }
  [@@deriving hardcaml ~rtlmangle:"$"]
end

let create scope ({clock; clear; range_begin; range_end; range_valid; last; _ } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let%hw range_count = reg_fb spec ~width:ram_addr_width ~enable:range_valid ~f:(fun x -> x +:. 1) in
  let%hw ram_load_done  = reg spec ~enable:last vdd in

  let ram_port_begin = Array.init 2 ~f:(fun _ -> { Ram_Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 }) in
  let ram_port_end   = Array.init 2 ~f:(fun _ -> { Ram_Port.address = wire ram_addr_width; write_data = wire result_width; write_enable = wire 1 }) in

  let ram_rdata_begin =
      Ram.create
        ~name:"ram_rbegin"
        ~collision_mode:Read_before_write
        ~size:ram_depth
        ~write_ports:
          (Array.map ram_port_begin ~f:(fun { address; write_data; write_enable } ->
             { Hardcaml.Write_port.write_clock = clock
             ; write_address = address
             ; write_enable
             ; write_data
             }))          
        ~read_ports:
          (Array.map ram_port_begin ~f:(fun { address; write_enable; _ } ->     
            { Hardcaml.Read_port.read_clock = clock
            ; read_address = address
            ; read_enable = ~:write_enable
            }))
        ()
  in
  let ram_rdata_end =
      Ram.create
        ~name:"ram_rend"
        ~collision_mode:Read_before_write
        ~size:ram_depth
        ~write_ports:
          (Array.map ram_port_end ~f:(fun { address; write_data; write_enable } ->
             { Hardcaml.Write_port.write_clock = clock
             ; write_address = address
             ; write_enable
             ; write_data
             }))          
        ~read_ports:
          (Array.map ram_port_end ~f:(fun { address; write_enable; _ } ->     
            { Hardcaml.Read_port.read_clock = clock
            ; read_address = address
            ; read_enable = ~:write_enable
            }))
        ()
  in

  let open Always in
  let%hw.Always.State_machine sm = State_machine.create (module States) spec in
  (* Variables for sorting stage *)

  let%hw_var ram_addr_0 = Variable.reg spec ~width:ram_addr_width in
  let%hw_var ram_addr_1 = Variable.reg spec ~width:ram_addr_width in

  let%hw_var bsort_addr_outer = Variable.reg spec ~width:ram_addr_width in
  let%hw_var mem_write_enable = Variable.wire ~default:gnd () in
  let%hw bsort_need_swap = mux2 (ram_rdata_begin.(0) ==: ram_rdata_begin.(1)) 
                                (ram_rdata_end.(0) >: ram_rdata_end.(1)) 
                                (ram_rdata_begin.(0) >: ram_rdata_begin.(1)) in
  (* Variables for counting stage *)
  let%hw should_count = (ram_rdata_begin.(1) >: ram_rdata_end.(0)) |: (ram_addr_1.value ==: range_count) in
  let%hw merged_range_end = mux2 (ram_rdata_end.(0) >: ram_rdata_end.(1)) ram_rdata_end.(0) ram_rdata_end.(1) in
  compile
    [ sm.switch
        [ (Ram_load, [
              when_ (range_valid) [
                mem_write_enable     <-- vdd;
                ram_addr_0           <-- ram_addr_0.value +:. 1;
                ram_addr_1           <-- ram_addr_1.value +:. 1
              ];
              when_ (ram_load_done) [
                sm.set_next Ram_sort_read;
                bsort_addr_outer <-- range_count -:. 2;
                ram_addr_0 <--. 0;
                ram_addr_1 <--. 1
              ]
          ]); (Ram_sort_read, [
              sm.set_next Ram_sort_write
          ]); (Ram_sort_write, [
              sm.set_next Ram_sort_read;
              mem_write_enable <-- vdd;
              ram_addr_0 <-- ram_addr_0.value +:. 1;
              ram_addr_1 <-- ram_addr_1.value +:. 1;
              when_ (ram_addr_0.value ==: bsort_addr_outer.value) [
                ram_addr_0 <--. 0;
                ram_addr_1 <--. 1;
                bsort_addr_outer <-- bsort_addr_outer.value -:. 1;
                when_ (bsort_addr_outer.value ==:. 0) [
                  sm.set_next Ram_count_read;
                  ram_addr_0 <--. 0;
                  ram_addr_1 <--. 1;
                ]
              ]
          ]); (Ram_count_read, [
              sm.set_next Ram_count_process
          ]); (Ram_count_process, [
              mem_write_enable <-- vdd;
              when_ (should_count) [
                ram_addr_0 <-- ram_addr_1.value
              ];
              if_ (ram_addr_1.value <: range_count) [
                sm.set_next Ram_count_read;
                ram_addr_1 <-- ram_addr_1.value +:. 1
              ][
                sm.set_next Done;
                ram_addr_0 <--. 0;
                ram_addr_1 <--. 0;
              ]
          ]); (Done, [
          ])
        ]
    ];

  let%hw is_sorting_state = (sm.is Ram_sort_read) |: (sm.is Ram_sort_write) in

  Signal.(ram_port_begin.(0).address         <-- ram_addr_0.value);
  Signal.(ram_port_begin.(0).write_data      <-- (mux2 (sm.is Ram_load) range_begin @@
                                                  mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(1) ram_rdata_begin.(0)));
  Signal.(ram_port_begin.(0).write_enable    <-- mem_write_enable.value);

  Signal.(ram_port_begin.(1).address         <-- ram_addr_1.value);
  Signal.(ram_port_begin.(1).write_data      <-- (mux2 (sm.is Ram_load) range_begin @@
                                                  mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(0) ram_rdata_begin.(1)));
  Signal.(ram_port_begin.(1).write_enable    <-- mem_write_enable.value);

  Signal.(ram_port_end.(0).address           <-- ram_addr_0.value);
  Signal.(ram_port_end.(0).write_data        <-- (mux2 (sm.is Ram_load) range_end @@
                                                  mux2 is_sorting_state 
                                                 (mux2 bsort_need_swap ram_rdata_end.(1) ram_rdata_end.(0)) 
                                                 (mux2 should_count ram_rdata_end.(0) merged_range_end)));
  Signal.(ram_port_end.(0).write_enable      <-- mem_write_enable.value);

  Signal.(ram_port_end.(1).address           <-- ram_addr_1.value);
  Signal.(ram_port_end.(1).write_data        <-- ( mux2 (sm.is Ram_load) range_end @@
                                                   mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_end.(0) ram_rdata_end.(1)));
  Signal.(ram_port_end.(1).write_enable      <-- mem_write_enable.value);

  let result = reg_fb spec ~width:result_width ~enable:((sm.is Ram_count_process) &: should_count)
                ~f:(fun x -> x +: (ram_rdata_end.(0) -: ram_rdata_begin.(0)) +:. 1) in

  let valid_out = reg spec ((sm.is Ram_count_process) &: (ram_addr_1.value ==: range_count)) in
  { valid_out; result }
;;
*)
module States = struct
  type t =
    | Ram_load
    | Ram_sort_read
    | Ram_sort_write
    | Ram_count_read
    | Ram_count_process
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({clock; clear; range_begin; range_end; range_valid; last; _ } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let%hw range_count = reg_fb spec ~width:ram_addr_width ~enable:range_valid ~f:(fun x -> x +:. 1) in
  let%hw ram_load_done  = reg spec ~enable:last vdd in

  let ram_port_begin = Array.init 2 ~f:(fun _ -> { TDP_Ram.Port.address = wire ram_addr_width; data = wire result_width; enable = wire 1; write = wire 1 }) in
  let ram_port_end   = Array.init 2 ~f:(fun _ -> { TDP_Ram.Port.address = wire ram_addr_width; data = wire result_width; enable = wire 1; write = wire 1 }) in

  let%tydi { qa = ram_rdata_begin_a; qb = ram_rdata_begin_b } =
    TDP_Ram.hierarchical
      ~name:"ram_rbegin"
      ~size:ram_depth
      scope 
      {clock; port_a = ram_port_begin.(0); port_b = ram_port_begin.(1) }
  in
  let%tydi { qa = ram_rdata_end_a; qb = ram_rdata_end_b } =
    TDP_Ram.hierarchical
      ~name:"ram_rend"
      ~size:ram_depth
      scope 
      {clock; port_a = ram_port_end.(0); port_b = ram_port_end.(1) }
  in

  let ram_rdata_begin = [|ram_rdata_begin_a; ram_rdata_begin_b|] in
  let ram_rdata_end   = [|ram_rdata_end_a; ram_rdata_end_b|] in 

  let open Always in
  let%hw.Always.State_machine sm = State_machine.create (module States) spec in

  let%hw_var ram_addr_0 = Variable.reg spec ~width:ram_addr_width in
  let%hw_var ram_addr_1 = Variable.reg spec ~width:ram_addr_width in

  let%hw_var bsort_addr_outer = Variable.reg spec ~width:ram_addr_width in
  let%hw_var_array mem_write = Array.init 2 ~f:(fun _ -> Variable.wire ~default:gnd ()) in
  let%hw bsort_need_swap = mux2 (ram_rdata_begin.(0) ==: ram_rdata_begin.(1)) 
                                (ram_rdata_end.(0) >: ram_rdata_end.(1)) 
                                (ram_rdata_begin.(0) >: ram_rdata_begin.(1)) in
  (* Variables for counting stage *)
  let%hw should_count = (ram_rdata_begin.(1) >: ram_rdata_end.(0)) |: (ram_addr_1.value ==: range_count) in
  let%hw merged_range_end = mux2 (ram_rdata_end.(0) >: ram_rdata_end.(1)) ram_rdata_end.(0) ram_rdata_end.(1) in
  compile
    [ sm.switch
        [ (Ram_load, [
              when_ (range_valid) [
                mem_write.(0)        <-- vdd;
                ram_addr_0           <-- ram_addr_0.value +:. 1
              ];
              when_ (ram_load_done) [
                sm.set_next Ram_sort_read;
                bsort_addr_outer <-- range_count -:. 2;
                ram_addr_0 <--. 0;
                ram_addr_1 <--. 1
              ]
          ]); (Ram_sort_read, [
              sm.set_next Ram_sort_write
          ]); (Ram_sort_write, [
              sm.set_next Ram_sort_read;
              mem_write.(0)        <-- vdd;
              mem_write.(1)        <-- vdd;
              ram_addr_0           <-- ram_addr_0.value +:. 1;
              ram_addr_1           <-- ram_addr_1.value +:. 1;
              when_ (ram_addr_0.value ==: bsort_addr_outer.value) [
                ram_addr_0         <--. 0;
                ram_addr_1         <--. 1;
                bsort_addr_outer   <-- bsort_addr_outer.value -:. 1;
                when_ (bsort_addr_outer.value ==:. 0) [
                  sm.set_next Ram_count_read;
                  ram_addr_0 <--. 0;
                  ram_addr_1 <--. 1;
                ]
              ]
          ]); (Ram_count_read, [
              sm.set_next Ram_count_process
          ]); (Ram_count_process, [
              mem_write.(0) <-- vdd;
              when_ (should_count) [
                ram_addr_0 <-- ram_addr_1.value
              ];
              if_ (ram_addr_1.value <: range_count) [
                sm.set_next Ram_count_read;
                ram_addr_1 <-- ram_addr_1.value +:. 1
              ][
                sm.set_next Done;
                ram_addr_0 <--. 0;
                ram_addr_1 <--. 0;
              ]
          ]); (Done, [
          ])
        ]
    ];

  let%hw is_sorting_state = (sm.is Ram_sort_read) |: (sm.is Ram_sort_write) in

  Signal.(ram_port_begin.(0).address         <-- ram_addr_0.value);
  Signal.(ram_port_begin.(0).data            <-- (mux2 (sm.is Ram_load) range_begin @@
                                                  mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(1) ram_rdata_begin.(0)));
  Signal.(ram_port_begin.(0).write           <-- mem_write.(0).value);
  Signal.(ram_port_begin.(0).enable          <-- vdd);

  Signal.(ram_port_begin.(1).address         <-- ram_addr_1.value);
  Signal.(ram_port_begin.(1).data            <-- (mux2 (sm.is Ram_load) range_begin @@
                                                  mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_begin.(0) ram_rdata_begin.(1)));
  Signal.(ram_port_begin.(1).write           <-- mem_write.(1).value);
  Signal.(ram_port_begin.(1).enable          <-- vdd);

  Signal.(ram_port_end.(0).address           <-- ram_addr_0.value);
  Signal.(ram_port_end.(0).data              <-- (mux2 (sm.is Ram_load) range_end @@
                                                  mux2 is_sorting_state 
                                                 (mux2 bsort_need_swap ram_rdata_end.(1) ram_rdata_end.(0)) 
                                                 (mux2 should_count ram_rdata_end.(0) merged_range_end)));
  Signal.(ram_port_end.(0).write             <-- mem_write.(0).value);
  Signal.(ram_port_end.(0).enable            <-- vdd);

  Signal.(ram_port_end.(1).address           <-- ram_addr_1.value);
  Signal.(ram_port_end.(1).data              <-- ( mux2 (sm.is Ram_load) range_end @@
                                                   mux2 (is_sorting_state &: bsort_need_swap) ram_rdata_end.(0) ram_rdata_end.(1)));
  Signal.(ram_port_end.(1).write             <-- mem_write.(1).value);
  Signal.(ram_port_end.(1).enable            <-- vdd);

  let result = reg_fb spec ~width:result_width ~enable:((sm.is Ram_count_process) &: should_count)
                ~f:(fun x -> x +: (ram_rdata_end.(0) -: ram_rdata_begin.(0)) +:. 1) in

  let valid_out = reg spec ((sm.is Ram_count_process) &: (ram_addr_1.value ==: range_count)) in
  { valid_out; result }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day05" create
;;

