open! Core
open! Hardcaml
open! Signal

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
      uart_out : 'a Uart.Byte_with_valid.t 
    }
  [@@deriving hardcaml]
end

let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
  =
  let%tydi { valid_out = loader_valid; range_begin; range_end; last } = 
    Day02.Loader.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { valid_out = algo_valid; result } = 
    Day02.hierarchical scope { clock; clear; valid_in = loader_valid; range_begin; range_end; last }
  in    
  (* Day07
  let%tydi { valid_out = loader_valid; row; last } = 
    Day07.Loader.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { valid_out = algo_valid; result } = 
    Day07.hierarchical scope { clock; clear; valid_in = loader_valid; row; last }
  in  
  *)
  (* Day06
  let%tydi { valid_out = algo_valid; result } = 
    Day06.hierarchical scope { clock; clear; uart_in }
  in
  *)  
  (* Day04
  let%tydi { valid_out = loader_valid; row; count; last } = 
    Day04.Loader.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { valid_out = algo_valid; result } = 
    Day04.hierarchical scope { clock; clear; valid_in = loader_valid; row; count; last }
  in
  *)
  (* Day03
  let loader = 
    let module M = Structural_inst.Make (Day03.Loader) in
    M.create scope    
  in
  loader.i.clock <-- clock;
  loader.i.clear <-- clear;
  loader.i.uart_in.valid <-- uart_in.valid;
  loader.i.uart_in.value <-- uart_in.value;
  (*let%tydi { valid_out = loader_valid; bcd; count; last } = 
    Day02.Loader.hierarchical scope { clock; clear; uart_in }
  in*)
  let%tydi { valid_out = algo_valid; result; ready } = 
    Day03.hierarchical scope { 
      clock; 
      clear; 
      valid_in = loader.o.valid_out; 
      bcd = loader.o.bcd; 
      count = loader.o.count; 
      last = loader.o.last }
  in
  loader.i.ready <-- ready;
*)
(* Day01
  let%tydi { valid_out = loader_valid; direction; circles; offset; last } = 
    Day01.Loader.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { valid_out = algo_valid; result } = 
    Day01.hierarchical scope { clock; clear; valid_in = loader_valid; direction; circles; offset; last }
  in
*)
  let uart_out = Util.shift_out ~clock ~clear { valid = algo_valid; value = result } in
  { uart_out }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"algo_top" create
;;