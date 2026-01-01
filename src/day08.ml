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

  let num_width =  17
  let distance_width = num_width * 2 + 2
  let ram_addr_width = 10
  let ram_depth = Int.pow 2 ram_addr_width
  let num_sorted_distances = 100

  module Point = struct
    type 'a t =
      { x : 'a [@bits num_width]
      ; y : 'a [@bits num_width]
      ; z : 'a [@bits num_width]
      }
    [@@deriving hardcaml]
  end

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
        { point : 'a Point.t
        ; valid : 'a
        ; last : 'a
        }
      [@@deriving hardcaml]
    end
    let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
      = 
      let spec = Reg_spec.create ~clock ~clear () in

      let%hw eof    = uart_in.valid &: (uart_in.value ==: of_unsigned_int ~width:8 4) in
      let%hw eol    = uart_in.valid &: (uart_in.value ==: of_char '\n') in
      let%hw comma  = uart_in.valid &: (uart_in.value ==: of_char ',') in

      let shreg = reg_fb spec ~width:num_width ~enable:uart_in.valid 
            ~f:(fun x -> mux2 (comma |: eol) 
                      (zero @@ width x) 
                      ((Util.mul_10 x) +: (uresize ~width:(width x) @@ sel_bottom ~width:4 uart_in.value))
            ) in  

      let dim = reg_fb spec ~width:2 ~enable:uart_in.valid
            ~f:(fun x -> mux2 eol (zero @@ width x) @@ mux2 comma (x +:. 1) x) in

      let x = reg spec ~enable:(comma &: (dim ==:. 0)) shreg in
      let y = reg spec ~enable:(comma &: (dim ==:. 1)) shreg in
      let z = reg spec ~enable:(eol |: eof) shreg in
      let point = {Point.x; y; z} in
      let valid = reg spec (eol |: eof) in

      let last = reg spec eof in

      { point; valid; last }
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
      | Ram_load
      | Ram_read_p1
      | Ram_read_p2
      | Compute_distance
      | Sort_distance
      | Ram_read_r1
      | Check_r1
      | Ram_read_r2
      | Check_r2
      | Update_r1
      | Update_r2
      | Compute_count
      | Output_result
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

  module Point_Distance = struct
    type 'a t =
      { addra : 'a [@bits ram_addr_width]
      ; addrb : 'a [@bits ram_addr_width]
      ; distance : 'a [@bits distance_width]
      }
    [@@deriving hardcaml]

    let width = ram_addr_width * 2 + distance_width
  end

  let create scope ({clock; clear; uart_in } : _ I.t) : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let%tydi { point; valid = valid_in; last } = 
      Loader.hierarchical scope { clock; clear; uart_in }
    in
    let%hw point_count = reg_fb spec ~width:ram_addr_width ~enable:valid_in ~f:(fun x -> x +:. 1) in
    let%hw ram_load_done  = reg spec ~enable:last vdd in

    let point_ram_port = { Ram_Port.address = wire ram_addr_width; 
                           write_data = Point.Of_signal.pack point; 
                           write_enable = valid_in } in 
    let root_ram_port  = { Ram_Port.address = wire ram_addr_width; 
                           write_data = wire ram_addr_width; 
                           write_enable = wire 1 } in 
    let count_ram_port = { Ram_Port.address = wire ram_addr_width; 
                           write_data = wire ram_addr_width; 
                           write_enable = wire 1 } in 
    let point_ram_rdata =
        Ram.create
          ~name:"ram_point"
          ~collision_mode:Read_before_write
          ~size:ram_depth
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = point_ram_port.address
               ; write_enable  = point_ram_port.write_enable
               ; write_data    = point_ram_port.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = point_ram_port.address
              ; read_enable    = ~:(point_ram_port.write_enable)
              } |]
          ()
    in
    let root_ram_rdata =
        Ram.create
          ~name:"ram_root"
          ~collision_mode:Read_before_write
          ~size:ram_depth
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = root_ram_port.address
               ; write_enable  = root_ram_port.write_enable
               ; write_data    = root_ram_port.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = root_ram_port.address
              ; read_enable    = ~:(root_ram_port.write_enable)
              } |]
          ()
    in
    let count_ram_rdata =
        Ram.create
          ~name:"ram_count"
          ~collision_mode:Read_before_write
          ~size:ram_depth
          ~write_ports:
              [| { Hardcaml.Write_port.write_clock = clock
               ; write_address = count_ram_port.address
               ; write_enable  = count_ram_port.write_enable
               ; write_data    = count_ram_port.write_data
               }|]    
          ~read_ports: 
             [| { Hardcaml.Read_port.read_clock = clock
              ; read_address   = count_ram_port.address
              ; read_enable    = ~:(count_ram_port.write_enable)
              } |]
          ()
    in

    let open Always in
    let%hw.Always.State_machine sm = State_machine.create (module States) spec in

    let%hw_var ram_addr_1 = Variable.reg spec ~width:ram_addr_width in
    let%hw_var ram_addr_2 = Variable.reg spec ~width:ram_addr_width in

    let%hw_var processed_distances = Variable.reg spec ~width:(num_bits_to_represent num_sorted_distances) in
    let%hw_var processed_counts = Variable.reg spec ~width:ram_addr_width in

    let%hw distance_packed = wire Point_Distance.width in
    let distance_record = Point_Distance.Of_signal.unpack distance_packed in

    compile 
        [ sm.switch
            [ (Ram_load, [
                when_ (ram_load_done) [
                  sm.set_next Ram_read_p1;
                  ram_addr_1    <--. 0;
                  ram_addr_2    <--. 1
                ]
              ]); (Ram_read_p1, [
                sm.set_next Ram_read_p2
              ]); (Ram_read_p2, [
                sm.set_next Compute_distance
              ]); (Compute_distance, [
                sm.set_next Sort_distance
              ]); (Sort_distance, [
                sm.set_next Ram_read_p1;
                ram_addr_2 <-- ram_addr_2.value +:. 1;
                when_ (ram_addr_2.value ==: (point_count -:. 1)) [
                  ram_addr_1 <-- ram_addr_1.value +:. 1;
                  ram_addr_2 <-- ram_addr_1.value +:. 2;
                  when_ (ram_addr_1.value ==: (point_count -:. 2)) [
                    sm.set_next Ram_read_r1;
                    processed_distances <--. 0;
                    ram_addr_1 <-- distance_record.addra
                  ]
                ]
              ]); (Ram_read_r1, [
                sm.set_next Check_r1
              ]); (Check_r1, [
                sm.set_next Ram_read_r1;
                ram_addr_1 <-- root_ram_rdata.(0);
                when_ (root_ram_rdata.(0) ==: (ones ram_addr_width)) [
                  sm.set_next Ram_read_r2;
                  ram_addr_1 <-- distance_record.addrb
                ]
              ]); (Ram_read_r2, [
                sm.set_next Check_r2
              ]); (Check_r2, [
                sm.set_next Ram_read_r2;
                ram_addr_1 <-- root_ram_rdata.(0);
                when_ (root_ram_rdata.(0) ==: (ones ram_addr_width)) [
                  sm.set_next Update_r1;
                  processed_distances <-- processed_distances.value +:. 1;
                ]                
              ]); (Update_r1, [
                sm.set_next Update_r2
              ]); (Update_r2, [
                sm.set_next Ram_read_r1;
                ram_addr_1 <-- distance_record.addra;
                when_ (processed_distances.value ==:. num_sorted_distances) [
                  sm.set_next Compute_count;
                  processed_counts <--. 0
                ]
              ]); (Compute_count, [
                processed_counts <-- processed_counts.value +:. 1;
                when_ (processed_counts.value ==: point_count) [
                  sm.set_next Output_result
                ]
              ]); (Output_result, [
                sm.set_next Done
              ]); (Done, [
              ])
            ]
        ];

    let  p1 = Point.Of_signal.unpack @@ reg spec ~enable:(sm.is Ram_read_p2) point_ram_rdata.(0) in
    let  p2 = Point.Of_signal.unpack point_ram_rdata.(0) in

    let%hw _p1_x = p1.x in
    let%hw _p1_y = p1.y in
    let%hw _p1_z = p1.z in
    let%hw _p2_x = p2.x in
    let%hw _p2_y = p2.y in
    let%hw _p2_z = p2.z in


    (* Distance sorting *)
    let%hw distance = List.map2_exn ~f:(fun x y -> mux2 (x >: y) (x -: y) (y -: x)) (Point.to_list p1) (Point.to_list p2) |>
                      List.map ~f:(fun x -> uresize ~width:distance_width (x *: x)) |> 
                      reduce ~f:(+:) in

    let distance_r = reg spec ~enable:(sm.is Compute_distance) distance in

    let indices = List.init num_sorted_distances ~f:(fun x -> x) in
    let next_sorted_distance = List.map indices ~f:(fun _ -> wire Point_Distance.width) in
    let%hw_list sorted_distance = List.map indices ~f:(fun x -> 
        reg spec ~enable:(sm.is Sort_distance) 
                 ~clear_to:(Point_Distance.Of_signal.pack { Point_Distance.addra = zero ram_addr_width; addrb = zero ram_addr_width; distance = ones distance_width })
                (List.nth_exn next_sorted_distance x)
    ) in
    let%hw_list comp_result  = List.map indices ~f:(fun x -> distance_r <=: (Point_Distance.Of_signal.unpack @@ List.nth_exn sorted_distance x).distance) in
    let%hw insert_index = trailing_zeros @@ concat_lsb comp_result in
    List.iter indices ~f:(fun x -> Signal.(List.nth_exn next_sorted_distance x <--
        mux2 (insert_index ==:. x) (Point_Distance.Of_signal.pack { Point_Distance.addra = ram_addr_1.value; addrb = ram_addr_2.value; distance = distance_r })
        (if x > 0 
         then mux2 (insert_index <:. x) (List.nth_exn sorted_distance (x - 1)) (List.nth_exn sorted_distance x)
         else (List.nth_exn sorted_distance x))
    ));

    (* Root update *)
    let%hw root1 = reg spec ~enable:(sm.is Check_r1 &: (root_ram_rdata.(0) ==: ones ram_addr_width)) ram_addr_1.value in
    let%hw root2 = reg spec ~enable:(sm.is Check_r2 &: (root_ram_rdata.(0) ==: ones ram_addr_width)) ram_addr_1.value in
    let%hw count1 = reg spec ~enable:(sm.is Check_r1 &: (root_ram_rdata.(0) ==: ones ram_addr_width)) count_ram_rdata.(0) in
    let%hw count2 = reg spec ~enable:(sm.is Check_r2 &: (root_ram_rdata.(0) ==: ones ram_addr_width)) count_ram_rdata.(0) in

    (* Count sorting *)
    let%hw count_largest_1 = reg_fb spec ~width:ram_addr_width ~enable:(sm.is Compute_count &: (processed_counts.value >:. 0))
                                         ~f:(fun x -> mux2 (count_ram_rdata.(0) >: x) count_ram_rdata.(0) x) in 
    let%hw count_largest_2 = reg_fb spec ~width:ram_addr_width ~enable:(sm.is Compute_count &: (processed_counts.value >:. 0))
                                         ~f:(fun x -> mux2 (count_ram_rdata.(0) >: count_largest_1) count_largest_1 @@ 
                                                      mux2 (count_ram_rdata.(0) >: x) count_ram_rdata.(0) x ) in
    let%hw count_largest_3 = reg_fb spec ~width:ram_addr_width ~enable:(sm.is Compute_count &: (processed_counts.value >:. 0))
                                         ~f:(fun x -> mux2 (count_ram_rdata.(0) >: count_largest_2) count_largest_2 @@ 
                                                      mux2 (count_ram_rdata.(0) >: x) count_ram_rdata.(0) x ) in

    Signal.(distance_packed <-- mux processed_distances.value sorted_distance);

    Signal.(point_ram_port.address <-- (mux2 (sm.is Ram_load) point_count @@
                                        mux2 (sm.is Ram_read_p1) ram_addr_1.value ram_addr_2.value));

    Signal.(root_ram_port.address  <-- (mux2 (sm.is Ram_load) point_count @@ 
                                        mux2 (sm.is Update_r1) root1 @@
                                        mux2 (sm.is Update_r2) root2 @@
                                        ram_addr_1.value ));
    Signal.(count_ram_port.address <-- (mux2 (sm.is Ram_load) point_count @@ 
                                        mux2 (sm.is Update_r1) root1 @@
                                        mux2 (sm.is Update_r2) root2 @@
                                        mux2 (sm.is Compute_count) processed_counts.value @@
                                        ram_addr_1.value ));

    Signal.(root_ram_port.write_data  <-- (mux2 (sm.is Ram_load) (ones ram_addr_width) @@
                                           mux2 (sm.is Update_r1 &: (root1 <>: root2)) root2 @@ root1 ));
    Signal.(count_ram_port.write_data <-- (mux2 (sm.is Ram_load) (one ram_addr_width) @@
                                           mux2 (sm.is Update_r1 &: (root1 <>: root2)) (zero ram_addr_width) @@
                                           mux2 (sm.is Update_r2 &: (root1 <>: root2)) (count1 +: count2) count_ram_rdata.(0)));

    Signal.(root_ram_port.write_enable  <-- (mux2 (sm.is Ram_load) valid_in @@ 
                                             mux2 (sm.is Update_r1 &: (root1 <>: root2)) vdd gnd ));
    Signal.(count_ram_port.write_enable <-- (mux2 (sm.is Ram_load) valid_in @@ 
                                             mux2 ((sm.is Update_r1 |: sm.is Update_r2) &: (root1 <>: root2)) vdd gnd )); 

    let%hw _root = root_ram_rdata.(0) in
    let%hw _count = count_ram_rdata.(0) in
    let%hw _root_addr = root_ram_port.address in
    let%hw _count_addr = count_ram_port.address in
    let%hw _root_write_data = root_ram_port.write_data in
    let%hw _count_write_data = count_ram_port.write_data in
    let%hw _root_write_enable = root_ram_port.write_enable in
    let%hw _count_write_enable = count_ram_port.write_enable in
    let%hw _distance_addra = distance_record.addra in
    let%hw _distance_addrb = distance_record.addrb in

    let valid_out = (sm.is Output_result) in
    let result = uresize ~width:result_width (count_largest_1 *: count_largest_2 *: count_largest_3) in
    { valid_out; result }

  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day08" create
  ;;
end

