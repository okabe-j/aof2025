open! Core
open! Hardcaml
open! Signal

let result_width = 64
let bcd_width = 4
let ram_addr_width = 8
let ram_depth = Int.pow 2 ram_addr_width

module Ram = Loadable_pseudo_dual_port_ram.Make (struct
    let width = result_width
    let depth = ram_depth
    let zero_on_startup = false
    let num_ports = 2
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

let create scope ({clock; clear; range_begin; range_end; range_valid; id; id_valid; last;} : _ I.t) : _ O.t
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


let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day05" create
;;

