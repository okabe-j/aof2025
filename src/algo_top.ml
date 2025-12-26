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

module Make (Config : sig
    val day : string
    val part : string
    val result_width : int
  end) =
struct
  open Config

  module Day01 = Day01.Make (struct
      let result_width = result_width
      let part = part
  end)

  module Day02 = Day02.Make (struct
      let result_width = result_width
      let part = part
  end)

  module Day03 = Day03.Make (struct
      let result_width = result_width
      let part = part
  end)

  module Day04 = Day04.Make (struct
      let result_width = result_width
      let part = part
  end)

  let create scope ({ clock; clear; uart_in } : _ I.t) : _ O.t
    =
    (*
    let%tydi {range_begin; range_end; range_valid; id; id_valid; last } = 
      Day05.Loader.hierarchical scope { clock; clear; uart_in }
    in
    let%tydi { valid_out = algo_valid; result } = 
      Day05.hierarchical scope { clock; clear; range_begin; range_end; range_valid; id; id_valid; last }
    in   
    *)
    (*
    let%tydi { valid_out = loader_valid; range_begin; range_end; last } = 
      Day02.Loader.hierarchical scope { clock; clear; uart_in }
    in
    let%tydi { valid_out = algo_valid; result } = 
      Day02.hierarchical scope { clock; clear; valid_in = loader_valid; range_begin; range_end; last }
    in 
    *)   
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
  (* Day01 *)
  (*
    let%tydi { valid_out = loader_valid; direction; circles; offset; last } = 
      Day01_Inst.Loader.hierarchical scope { clock; clear; uart_in }
    in
    let%tydi { valid_out; result } = 
      Day01_Inst.hierarchical scope { clock; clear; valid_in = loader_valid; direction; circles; offset; last }
    in
  *)
    let algo_valid = wire 1 in
    let algo_result = wire result_width in
    if String.equal day "day01" then (
      let%tydi { valid_out ; result } = 
        Day01.hierarchical scope { clock; clear; uart_in }
      in
      algo_valid  <-- valid_out;
      algo_result <-- result;
    )
    else if String.equal day "day02" then (
      let%tydi { valid_out ; result } = 
        Day02.hierarchical scope { clock; clear; uart_in }
      in
      algo_valid  <-- valid_out;
      algo_result <-- result;
    )
    else if String.equal day "day03" then (
      let%tydi { valid_out ; result } = 
        Day03.hierarchical scope { clock; clear; uart_in }
      in
      algo_valid  <-- valid_out;
      algo_result <-- result;
    )
    else if String.equal day "day04" then (
      let%tydi { valid_out ; result } = 
        Day04.hierarchical scope { clock; clear; uart_in }
      in
      algo_valid  <-- valid_out;
      algo_result <-- result;
    )
    else (
      let%tydi { valid_out ; result } = 
        Day02.hierarchical scope { clock; clear; uart_in }
      in
      algo_valid  <-- valid_out;
      algo_result <-- result; 
    );
      
    let uart_out = Util.shift_out ~clock ~clear { valid = algo_valid; value = algo_result } in
    { uart_out }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"algo_top" create
  ;;
end