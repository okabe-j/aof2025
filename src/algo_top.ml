open! Core
open! Hardcaml
open! Signal

module Byte_with_valid = With_valid.Vector (struct
    let width = 8
end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; uart_in : 'a Byte_with_valid.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { 
      uart_out : 'a Byte_with_valid.t 
    }
  [@@deriving hardcaml]
end

let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
  =
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
(*
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