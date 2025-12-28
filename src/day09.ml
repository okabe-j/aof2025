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
  let number_width = 17
  let point_ram_addr_width = 9
  let point_ram_depth = Int.pow 2 point_ram_addr_width

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
      | Ram_load
      | Ram_read_1
      | Ram_read_2
      | Compute_area
      | Output_result
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Ram_Port = struct
    type 'a t =
      { address : 'a [@bits point_ram_addr_width]
      ; write_data : 'a [@bits result_width]
      ; write_enable : 'a
      }
    [@@deriving hardcaml ~rtlmangle:"$"]
  end

  let create scope ({clock; clear; uart_in} : _ I.t) : _ O.t
    =
    let spec  = Reg_spec.create ~clock ~clear () in
    let%hw eol    = uart_in.valid &: (uart_in.value ==: of_char '\n') in
    let%hw eof    = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in
    let%hw comma  = uart_in.valid &: (uart_in.value ==: of_char ',') in
    let%hw number = uart_in.valid &: ((uart_in.value >=: of_char '0') &: (uart_in.value <=: of_char '9')) in 
    let%hw bcd    = reg_fb spec ~width:number_width ~enable:uart_in.valid 
          ~f:(fun x -> mux2 (comma |: eol) (zero number_width) @@
                       mux2 number ((Util.mul_10 x) +: (uresize ~width:number_width uart_in.value.:+[0, Some 4])) x) in
    let%hw point_count = reg_fb spec ~width:point_ram_addr_width ~enable:(eof |: eol)
          ~f:(fun x -> x +:. 1) in
    let%hw p_x    = reg spec ~enable:comma bcd in
    let%hw p_y    = bcd in
    let%hw ram_wr = (eol |: eof) in
    let%hw ram_load_done = reg spec ~enable:eof vdd in

    let ram_port_x = { Ram_Port.address = wire point_ram_addr_width; write_data = p_x; write_enable = ram_wr } in
    let ram_port_y = { Ram_Port.address = wire point_ram_addr_width; write_data = p_y; write_enable = ram_wr } in

    let ram_rdata_x =
        Ram.create
          ~name:"ram_x"
          ~collision_mode:Read_before_write
          ~size:point_ram_depth
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = ram_port_x.address
               ; write_enable  = ram_port_x.write_enable
               ; write_data    = ram_port_x.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = ram_port_x.address
              ; read_enable    = ~:(ram_port_x.write_enable)
              } |]
          ()
    in
    let ram_rdata_y =
        Ram.create
          ~name:"ram_y"
          ~collision_mode:Read_before_write
          ~size:point_ram_depth
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = ram_port_y.address
               ; write_enable  = ram_port_y.write_enable
               ; write_data    = ram_port_y.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = ram_port_y.address
              ; read_enable    = ~:(ram_port_y.write_enable)
              } |]
          ()
    in  

    let open Always in
    let%hw.Always.State_machine sm = State_machine.create (module States) spec in

    let%hw_var ram_addr_1 = Variable.reg spec ~width:point_ram_addr_width in
    let%hw_var ram_addr_2 = Variable.reg spec ~width:point_ram_addr_width in

    compile 
        [ sm.switch
            [ (Ram_load, [
                when_ (ram_load_done) [
                  sm.set_next Ram_read_1;
                  ram_addr_1    <--. 0;
                  ram_addr_2    <--. 1
                ]
              ]); (Ram_read_1, [
                sm.set_next Ram_read_2
              ]); (Ram_read_2, [
                sm.set_next Compute_area
              ]); (Compute_area, [
                sm.set_next Ram_read_1;
                ram_addr_2 <-- ram_addr_2.value +:. 1;
                when_ (ram_addr_2.value ==: (point_count -:. 1)) [
                  ram_addr_1 <-- ram_addr_1.value +:. 1;
                  ram_addr_2 <-- ram_addr_1.value +:. 2;
                  when_ (ram_addr_1.value ==: (point_count -:. 2)) [
                    sm.set_next Output_result
                  ]
                ]
              ]); (Output_result, [
                sm.set_next Done
              ]); (Done, [
              ])
            ]
        ];
  Signal.(ram_port_x.address <-- (mux2 (sm.is Ram_load) point_count @@
                                  mux2 (sm.is Ram_read_1) ram_addr_1.value ram_addr_2.value ));
  Signal.(ram_port_y.address <-- (mux2 (sm.is Ram_load) point_count @@
                                  mux2 (sm.is Ram_read_1) ram_addr_1.value ram_addr_2.value ));
  let%hw point1_x = reg spec ~enable:(sm.is Ram_read_2) ram_rdata_x.(0) in
  let%hw point1_y = reg spec ~enable:(sm.is Ram_read_2) ram_rdata_y.(0) in
  let%hw point2_x = ram_rdata_x.(0) in (* Valid at Compute_area *)
  let%hw point2_y = ram_rdata_y.(0) in

  let%hw max_x = mux2 (point1_x >: point2_x) point1_x point2_x in
  let%hw min_x = mux2 (point1_x <: point2_x) point1_x point2_x in
  let%hw max_y = mux2 (point1_y >: point2_y) point1_y point2_y in
  let%hw min_y = mux2 (point1_y <: point2_y) point1_y point2_y in
  let%hw area = uresize ~width:result_width ((max_x -: min_x +:. 1) *: (max_y -: min_y +:. 1)) in
  let result = reg_fb spec ~width:result_width ~enable:(sm.is Compute_area) 
          ~f:(fun x -> mux2 (x <: area) area x) in
  let valid_out = sm.is Output_result in
  { valid_out ; result }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day09" create
  ;;
end