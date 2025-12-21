open! Core
open! Hardcaml
open! Signal

module Uart = Uart.Make (struct
    let baud = 100000
    let clock_freq_hz = 25000000
    let rx_fifo_depth = 32
    let tx_fifo_depth = 32
end)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; uart_rx : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { 
      uart_tx : 'a 
    }
  [@@deriving hardcaml]
end

let create scope ({ clock; clear; uart_rx } : _ I.t) : _ O.t
  =
  let%tydi { byte_out = uart_in; _ } = 
    Uart.Rx.create scope { clock; clear; ready = vdd; rx = uart_rx }
  in
  let%tydi { uart_out } = 
    Algo_top.hierarchical scope { clock; clear; uart_in }
  in
  let%tydi { tx = uart_tx; _ } = 
    Uart.Tx.create scope { clock; clear; byte_in = uart_out }
  in
  { uart_tx }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"design_top" create
;;