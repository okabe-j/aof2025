open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

  let _part = part

  let num_dests = 25

  let hash_ram_addr_width = 15
  let map_ram_addr_width = 10
  let hash_ram_data_width = map_ram_addr_width
  let map_ram_data_width = num_dests * hash_ram_data_width
  let pass_ram_addr_width = map_ram_addr_width
  let pass_ram_data_width = 32

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
      | Clear_ram
      | Configure_ram
      | Init_search1
      | Init_search2
      | Init_search3
      | Read_map_ram
      | Process_dests
      | Write_pass_ram
      | Clear_pass_ram
      | Output_result
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Ram_Port = struct
    type 'a t =
      { address : 'a [@bits hash_ram_addr_width]
      ; write_data : 'a [@bits hash_ram_data_width]
      ; write_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let mul_26 = fun x -> (sll x ~by:4) +: (sll x ~by:3) +: (sll x ~by:1)

  let get_node_hash node = (Char.to_int node.[0] - Char.to_int 'a') * 26 * 26 + 
                           (Char.to_int node.[1] - Char.to_int 'a') * 26 + 
                           (Char.to_int node.[2] - Char.to_int 'a')
  ;;

  let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t =
    let spec = Reg_spec.create ~clock ~clear () in

    let%hw letter    = uart_in.valid &: ((uart_in.value >=: of_char 'a') &: (uart_in.value <=: of_char 'z')) in
    let%hw colon     = uart_in.valid &: (uart_in.value ==: of_char ':') in
    let%hw space     = uart_in.valid &: (uart_in.value ==: of_char ' ') in
    let%hw eol       = uart_in.valid &: (uart_in.value ==: of_char '\n') in
    let%hw eof       = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in

    let%hw prev_in   = reg spec ~enable:uart_in.valid uart_in.value in

    let hash_ram_port = { Ram_Port.address   = wire hash_ram_addr_width; 
                                 write_data     = wire hash_ram_data_width;
                                 write_enable   = wire 1 } in 
    let map_ram_port  = { Ram_Port.address   = wire map_ram_addr_width; 
                                 write_data     = wire map_ram_data_width;
                                 write_enable   = wire 1 } in  

    let hash_ram_rdata =
        Ram.create
          ~name:"ram_hash"
          ~collision_mode:Read_before_write
          ~size:(Int.pow 2 hash_ram_addr_width)
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = hash_ram_port.address
               ; write_enable  = hash_ram_port.write_enable
               ; write_data    = hash_ram_port.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = hash_ram_port.address
              ; read_enable    = ~:(hash_ram_port.write_enable)
              } |]
          ()
    in    

    let map_ram_rdata =
        Ram.create
          ~name:"ram_map"
          ~collision_mode:Read_before_write
          ~size:(Int.pow 2 map_ram_addr_width)
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = map_ram_port.address
               ; write_enable  = map_ram_port.write_enable
               ; write_data    = map_ram_port.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = map_ram_port.address
              ; read_enable    = ~:(map_ram_port.write_enable)
              } |]
          ()
    in   

    let pass_ram_port = Array.init 2 ~f:(fun _ -> { Ram_Port.address   = wire pass_ram_addr_width; 
                                                        write_data     = wire pass_ram_data_width;
                                                        write_enable   = wire 1 }) in
    let%hw_array pass_ram_rdata = Array.init 2 ~f:(fun x -> 
        let rdata = 
          Ram.create
            ~name:("pass_ram_" ^ Int.to_string x)
            ~collision_mode:Read_before_write
            ~size:(Int.pow 2 pass_ram_addr_width)
            ~write_ports:
                [| { Hardcaml.Write_port.write_clock = clock
                 ; write_address = pass_ram_port.(x).address
                 ; write_enable  = pass_ram_port.(x).write_enable
                 ; write_data    = pass_ram_port.(x).write_data
                 }|]    
            ~read_ports: 
               [| { Hardcaml.Read_port.read_clock = clock
                ; read_address   = pass_ram_port.(x).address
                ; read_enable    = ~:(pass_ram_port.(x).write_enable)
                } |]
            () in rdata.(0)
    ) in

    let%hw input_hash_val_valid = eol |: eof |: colon |: (space &: (prev_in <>: of_char ':')) in
    let%hw hash_ram_wr = input_hash_val_valid &: (hash_ram_rdata.(0) ==:. 0) in

    let%hw next_index = reg_fb spec ~width:map_ram_addr_width 
                                    ~clear_to:(of_unsigned_int ~width:map_ram_addr_width 1)
                                    ~enable:hash_ram_wr
                                    ~f:(fun x -> x +:. 1) in

    let%hw input_hash_val = reg_fb spec ~width:hash_ram_addr_width
                                        ~enable: uart_in.valid
                                        ~f:(fun x -> mux2 letter ((mul_26 x) +: uresize ~width:hash_ram_addr_width (uart_in.value -: of_char 'a')) 
                                                                 (zero hash_ram_addr_width)) in 

    let%hw map_hash_key = reg spec ~enable:colon (mux2 hash_ram_wr next_index hash_ram_rdata.(0)) in
    let%hw map_ram_wdata = wire map_ram_data_width in
    let%hw map_ram_wdata_r = reg spec ~enable:input_hash_val_valid (mux2 colon (zero map_ram_data_width) map_ram_wdata) in

    Signal.(map_ram_wdata <-- (drop_top ~width:hash_ram_data_width map_ram_wdata_r @: (mux2 hash_ram_wr next_index hash_ram_rdata.(0))));

    let open Always in
    let%hw.Always.State_machine sm = State_machine.create (module States) spec in 
    let%hw_var ram_clear_addr = Variable.reg spec ~width:hash_ram_addr_width in 
    let%hw_var pass_ram_clear_addr = Variable.reg spec ~width:pass_ram_addr_width in 

    let%hw_var src_index  = Variable.reg spec ~width:map_ram_addr_width in
    let%hw_var dest_index = Variable.reg spec ~width:(num_bits_to_represent num_dests) in
    let%hw     dest       = mux dest_index.value (split_lsb ~part_width:hash_ram_data_width map_ram_rdata.(0)) in

    (* Which memory to use for the current pass *)
    let%hw_var flip       = Variable.reg spec ~width:1 in

    (* Search stops when there is no count update for any source node in current pass *)
    let%hw_var need_more_pass = Variable.reg spec ~width:1 in

    let%hw count = reg_fb spec ~width:pass_ram_data_width 
                               ~f:(fun x -> mux2 (sm.is Write_pass_ram) (zero pass_ram_data_width) @@
                                            mux2 (sm.is Process_dests &: (dest_index.value >:. 0)) (x +: (uresize ~width:pass_ram_data_width @@ mux ~:(flip.value) (Array.to_list pass_ram_rdata))) x) in

    let start_node_hash = of_unsigned_int ~width:hash_ram_addr_width @@ get_node_hash "you" in
    let end_node_hash = of_unsigned_int ~width:hash_ram_addr_width @@ get_node_hash "out" in

    let%hw start_node_index = reg spec ~enable:(sm.is Init_search3) hash_ram_rdata.(0) in
    let%hw end_node_index = reg spec ~enable:(sm.is Init_search2) hash_ram_rdata.(0) in

    (*
        Init_search1: Read hash_ram to get the index for end node (e.g. "out")
        Init_search2: Latch end node index. Read hash_ram to get the index for start node (e.g. "you")
        Init_search3: Latch start node index. Write 1 to the dest pass ram for end node index. 
    *)

    compile [ sm.switch
        [ (Clear_ram, [
            ram_clear_addr <-- ram_clear_addr.value +:. 1;
            when_ (ram_clear_addr.value ==: (ones hash_ram_addr_width)) [
              sm.set_next Configure_ram;
              ram_clear_addr <--. 0
            ]
          ]); (Configure_ram, [
            when_ (eof) [
              sm.set_next Init_search1
            ]
          ]); (Init_search1, [
            sm.set_next Init_search2
          ]); (Init_search2, [
            sm.set_next Init_search3
          ]); (Init_search3, [
            sm.set_next Read_map_ram
          ]); (Read_map_ram, [
            sm.set_next Process_dests
          ]); (Process_dests, [
            dest_index <-- dest_index.value +:. 1;
            when_ (dest ==:. 0) [
              sm.set_next Write_pass_ram;
              dest_index <--. 0
            ]
          ]); (Write_pass_ram, [
            sm.set_next Read_map_ram;
            src_index <-- src_index.value +:. 1;
            need_more_pass <-- (mux2 (count >:. 0) vdd need_more_pass.value);
            when_ (src_index.value ==: (next_index -:. 1)) [
              sm.set_next Clear_pass_ram;
              src_index <--. 0
            ]
          ]); (Clear_pass_ram, [
            pass_ram_clear_addr <-- pass_ram_clear_addr.value +:. 1;
            when_ (pass_ram_clear_addr.value ==: (ones pass_ram_addr_width)) [
              sm.set_next Read_map_ram;
              pass_ram_clear_addr <--. 0;
              flip <-- ~:(flip.value);
              need_more_pass <-- gnd;
              when_ ~:(need_more_pass.value) [
                sm.set_next Output_result
              ] 
            ]
          ]); (Output_result, [
            sm.set_next Done
          ]); (Done, [
          ])
        ]
    ];


    Signal.(hash_ram_port.address       <-- (mux2 (sm.is Clear_ram) ram_clear_addr.value @@
                                             mux2 (sm.is Configure_ram) input_hash_val @@ 
                                             mux2 (sm.is Init_search1) end_node_hash @@
                                             mux2 (sm.is Init_search2) start_node_hash (zero hash_ram_addr_width)));
    Signal.(hash_ram_port.write_data    <-- (mux2 (sm.is Clear_ram) (zero hash_ram_data_width) next_index));
    Signal.(hash_ram_port.write_enable  <-- (sm.is Clear_ram |: hash_ram_wr));

    Signal.(map_ram_port.address       <-- (mux2 (sm.is Clear_ram) (uresize ~width:map_ram_addr_width ram_clear_addr.value) @@
                                            mux2 (sm.is Configure_ram) map_hash_key src_index.value));
    Signal.(map_ram_port.write_data    <-- (mux2 (sm.is Clear_ram) (zero map_ram_data_width) map_ram_wdata));
    Signal.(map_ram_port.write_enable  <-- (sm.is Clear_ram |: eol |: eof));   

    Array.iteri pass_ram_port ~f:(fun i x -> (
      Signal.(x.address               <-- (mux2 (flip.value ==:. i) src_index.value (mux2 (sm.is Clear_pass_ram) pass_ram_clear_addr.value @@ mux2 (sm.is Init_search3) end_node_index dest)));
      Signal.(x.write_data            <-- (mux2 (flip.value ==:. i) count (mux2 (sm.is Init_search3) (one pass_ram_data_width) (zero pass_ram_data_width))));
      Signal.(x.write_enable          <-- (mux2 (flip.value ==:. i) (sm.is Write_pass_ram) (sm.is Clear_pass_ram |: sm.is Init_search3)))
    ));

    let%hw_list _map_ram_rdata = split_lsb ~part_width:map_ram_addr_width map_ram_rdata.(0) in
    let%hw _hash_ram_rdata = hash_ram_rdata.(0) in
    let%hw result = reg_fb spec ~width:result_width ~enable:(sm.is Write_pass_ram &: (src_index.value ==: start_node_index)) ~f:(fun x -> x +: uresize ~width:result_width count) in
    let%hw valid_out = sm.is Output_result in
    { valid_out; result }
  ;;    

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day11" create
  ;;
end