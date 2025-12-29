open! Core
open! Hardcaml
open! Signal

module Make (Config : sig
    val part : string
    val result_width : int
  end) =
struct
  open Config

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

  module States_part1 = struct
    type t =
      | Ram_load
      | Ram_read_1
      | Ram_read_2
      | Compute_area
      | Output_result
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module States_part2 = struct
    type t =
      | Ram_load
      | Ram_read_p1
      | Ram_read_p2
      | Compute_area
      | Ram_read_l
      | Check_valid
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

    if String.equal part "part1" then
      let open Always in
      let%hw.Always.State_machine sm = State_machine.create (module States_part1) spec in

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

    else (* part2 *)
      let open Always in
      let%hw.Always.State_machine sm = State_machine.create (module States_part2) spec in

      let%hw_var ram_addr_p1    = Variable.reg spec ~width:point_ram_addr_width in
      let%hw_var ram_addr_p2    = Variable.reg spec ~width:point_ram_addr_width in
      let%hw_var ram_addr_l     = Variable.reg spec ~width:point_ram_addr_width in
      let%hw_var loaded_points  = Variable.reg spec ~width:point_ram_addr_width in
      let%hw     check_pass     = wire 1 in

      compile 
          [ sm.switch
              [ (Ram_load, [
                  when_ (ram_load_done) [
                    sm.set_next Ram_read_p1;
                    ram_addr_p1    <--. 0;
                    ram_addr_p2    <--. 1
                  ]
                ]); (Ram_read_p1, [
                  sm.set_next Ram_read_p2
                ]); (Ram_read_p2, [
                  sm.set_next Compute_area
                ]); (Compute_area, [
                  sm.set_next Ram_read_l;
                  ram_addr_l       <--. 0;
                  loaded_points    <--. 0
                ]); (Ram_read_l, [
                  sm.set_next Check_valid;
                  loaded_points    <-- loaded_points.value +:. 1;
                ]); (Check_valid, [
                  if_ (~:check_pass |: (loaded_points.value ==: (point_count +:. 1))) [
                    sm.set_next Ram_read_p1;
                    ram_addr_p2 <-- ram_addr_p2.value +:. 1;
                    when_ (ram_addr_p2.value ==: (point_count -:. 1)) [
                      ram_addr_p1 <-- ram_addr_p1.value +:. 1;
                      ram_addr_p2 <-- ram_addr_p1.value +:. 2;
                      when_ (ram_addr_p1.value ==: (point_count -:. 2)) [
                        sm.set_next Output_result
                      ]
                    ]  
                  ][
                    sm.set_next Ram_read_l;
                    ram_addr_l <-- ram_addr_l.value +:. 1;
                    when_ (ram_addr_l.value ==: (point_count -:. 1)) [
                      ram_addr_l <--. 0
                    ]
                  ]                
                ]); (Output_result, [
                  sm.set_next Done
                ]); (Done, [
                ])
              ]
          ];

      Signal.(ram_port_x.address <-- (mux2 (sm.is Ram_load) point_count @@
                                      mux2 (sm.is Ram_read_p1) ram_addr_p1.value @@
                                      mux2 (sm.is Ram_read_p2) ram_addr_p2.value ram_addr_l.value ));
      Signal.(ram_port_y.address <-- (mux2 (sm.is Ram_load) point_count @@
                                      mux2 (sm.is Ram_read_p1) ram_addr_p1.value @@
                                      mux2 (sm.is Ram_read_p2) ram_addr_p2.value ram_addr_l.value ));

      let%hw point1_x = reg spec ~enable:(sm.is Ram_read_p2) ram_rdata_x.(0) in
      let%hw point1_y = reg spec ~enable:(sm.is Ram_read_p2) ram_rdata_y.(0) in
      let%hw point2_x = reg spec ~enable:(sm.is Compute_area) ram_rdata_x.(0) in
      let%hw point2_y = reg spec ~enable:(sm.is Compute_area) ram_rdata_y.(0) in

      let%hw max_x = mux2 (point1_x >: point2_x) point1_x point2_x in
      let%hw min_x = mux2 (point1_x <: point2_x) point1_x point2_x in
      let%hw max_y = mux2 (point1_y >: point2_y) point1_y point2_y in
      let%hw min_y = mux2 (point1_y <: point2_y) point1_y point2_y in

      let%hw area = uresize ~width:result_width ((max_x -: min_x +:. 1) *: (max_y -: min_y +:. 1)) in

      let%hw loaded_p2_x = reg spec ~enable:(sm.is Check_valid) ram_rdata_x.(0) in
      let%hw loaded_p2_y = reg spec ~enable:(sm.is Check_valid) ram_rdata_y.(0) in
      let%hw loaded_p1_x = ram_rdata_x.(0) in
      let%hw loaded_p1_y = ram_rdata_y.(0) in

      let%hw max_line_x = mux2 (loaded_p1_x >: loaded_p2_x) loaded_p1_x loaded_p2_x in
      let%hw min_line_x = mux2 (loaded_p1_x <: loaded_p2_x) loaded_p1_x loaded_p2_x in
      let%hw max_line_y = mux2 (loaded_p1_y >: loaded_p2_y) loaded_p1_y loaded_p2_y in
      let%hw min_line_y = mux2 (loaded_p1_y <: loaded_p2_y) loaded_p1_y loaded_p2_y in

      (* Want to check 2 things:
         1. Point [point1_x, point2_y] [point2_x, point1_y] is inside the tiles
            -> If the point is already on any green tile lines then we are good
            -> Otherwise we check vertical line [point1_x, 0] <-> [point1_x, point2_y] and [point2_x, 0] <-> [point2_x, point2_y] 
               cross odd number of horizontal green tile lines

         2. No green tile lines can intersect the 4 sides of rectangle.

         To check above we iterate through all the green tile lines between loaded_p1 and loaded_p2.
      *)
      let pipeline_loaded = (loaded_points.value >=:. 2) in
      let is_horizontal_line = (loaded_p1_y ==: loaded_p2_y) in

      let%hw is_on_line_1   = mux2 (is_horizontal_line) 
                              ((point2_y ==: loaded_p1_y) &: (point1_x >=: min_line_x) &: (point1_x <=: max_line_x))
                              ((point1_x ==: loaded_p1_x) &: (point2_y >=: min_line_y) &: (point2_y <=: max_line_y)) in
      let%hw is_on_line_2   = mux2 (is_horizontal_line) 
                              ((point1_y ==: loaded_p1_y) &: (point2_x >=: min_line_x) &: (point2_x <=: max_line_x))
                              ((point2_x ==: loaded_p1_x) &: (point1_y >=: min_line_y) &: (point1_y <=: max_line_y)) in
      let%hw is_on_line_1_r = reg_fb spec ~width:1 
          ~f:(fun x -> mux2 (sm.is Ram_read_p1) gnd @@ mux2 (sm.is Check_valid &: is_on_line_1) vdd x) in
      let%hw is_on_line_2_r = reg_fb spec ~width:1 
          ~f:(fun x -> mux2 (sm.is Ram_read_p1) gnd @@ mux2 (sm.is Check_valid &: is_on_line_2) vdd x) in


      let%hw is_cross_line_1 = is_horizontal_line &: (point2_y >: loaded_p1_y) &: (point1_x >=: min_line_x) &: (point1_x <: max_line_x) in
      let%hw is_cross_line_2 = is_horizontal_line &: (point1_y >: loaded_p1_y) &: (point2_x >=: min_line_x) &: (point2_x <: max_line_x) in
      let%hw cross_line_count_1 = reg_fb spec ~width:point_ram_addr_width 
          ~f:(fun x -> mux2 (sm.is Ram_read_p1) (zero @@ width x) @@ mux2 (sm.is Check_valid &: is_cross_line_1) (x +:. 1) x) in
      let%hw cross_line_count_2 = reg_fb spec ~width:point_ram_addr_width 
          ~f:(fun x -> mux2 (sm.is Ram_read_p1) (zero @@ width x) @@ mux2 (sm.is Check_valid &: is_cross_line_2) (x +:. 1) x) in

      let%hw p_okay_1 = is_on_line_1 |: is_on_line_1_r |: (is_cross_line_1 ^: cross_line_count_1.:(0)) in
      let%hw p_okay_2 = is_on_line_2 |: is_on_line_2_r |: (is_cross_line_2 ^: cross_line_count_2.:(0)) in

      let%hw no_intersect = mux2 (is_horizontal_line) 
                          ((loaded_p1_y <=: min_y) |: (loaded_p1_y >=: max_y) |: ((loaded_p1_x <=: min_x) &: (loaded_p2_x <=: min_x)) |: ((loaded_p1_x >=: max_x) &: (loaded_p2_x >=: max_x)))
                          ((loaded_p1_x <=: min_x) |: (loaded_p1_x >=: max_x) |: ((loaded_p1_y <=: min_y) &: (loaded_p2_y <=: min_y)) |: ((loaded_p1_y >=: max_y) &: (loaded_p2_y >=: max_y))) in

      Signal.(check_pass <-- (   
          mux2 (~:pipeline_loaded) vdd @@
          mux2 (~:no_intersect) gnd @@
          mux2 (loaded_points.value ==: point_count +:. 1) (p_okay_1 &: p_okay_2) vdd
      ));

      let result = reg_fb spec ~width:result_width 
              ~enable:(sm.is Check_valid &: check_pass &: (loaded_points.value ==: point_count +:. 1)) 
              ~f:(fun x -> mux2 (x <: area) area x) in
      let valid_out = sm.is Output_result in
      { valid_out ; result }

  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day09" create
  ;;
end