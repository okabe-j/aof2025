open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
open! Aof2025
module Harness = Cyclesim_harness.Make (Algo_top.I) (Algo_top.O)

let ( <--. ) = Bits.( <--. )
(*let sample_input_values = String.to_list "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82\nL200\nR200\x04"*)
(*let sample_input_values = String.to_list (In_channel.read_all "/Users/jiamingzhao/Documents/hardcaml/aof2025/test/testcase/day01.txt") @ ['\x04']*)
(*let sample_input_values = String.to_list "987654321111111\n811111111111119\n234234234234278\n818181911112111\x04"*)
let sample_input_values = String.to_list (In_channel.read_all "/Users/jiamingzhao/Documents/hardcaml/aof2025/test/testcase/day03.txt") @ ['\x04']

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in

  let feed_byte_input x =
    cycle ~n:1 ();
    inputs.uart_in.valid := Bits.vdd;
    inputs.uart_in.value <--. (Char.to_int x);
    cycle ();
    inputs.uart_in.valid := Bits.gnd;
    cycle ()
  in


  let rec get_output n = 
    if (n = 0) 
    then [] 
    else (
      while not (Bits.to_bool !(outputs.uart_out.valid)) do
        cycle ()
      done;
      let output_byte = Bits.to_unsigned_int !(outputs.uart_out.value) in
        cycle();
        [output_byte] @ get_output(n - 1);
    )
  in

  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();

  List.iter sample_input_values ~f:(fun x -> feed_byte_input x);
  let result_list = get_output 8 in 
  let result_number = List.fold_left result_list ~f:(fun acc b -> (acc lsl 8) lor b) ~init:0 in
  print_s [%message "Result" (result_number : int)];
  cycle ~n:100 ()
;;

let waves_config =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;

let%expect_test "Simple test" =
  Harness.run_advanced ~waves_config ~create:Algo_top.hierarchical simple_testbench;
  [%expect {|
    (Result (result_number 1165))
    |}]
;;
