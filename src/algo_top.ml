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
  let%tydi { valid_out = loader_valid; direction; number; last } = 
    Day01.Loader.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { valid_out = algo_valid; count } = 
    Day01.hierarchical scope { clock; clear; valid_in = loader_valid; direction; number; last }
  in
  let uart_out = Util.shift_out ~clock ~clear { valid = algo_valid; value = count } in
  { uart_out }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"algo_top" create
;;