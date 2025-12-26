open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
open! Aof2025

let result_width = 64
module Harness = Cyclesim_harness.Make (Algo_top.I) (Algo_top.O)

let ( <--. ) = Bits.( <--. )

let simple_testbench (sim : Harness.Sim.t) testfile =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* TODO: Use relative path to project end *)
  let sample_input_values = String.to_list (In_channel.read_all ("/Users/jiamingzhao/Documents/hardcaml/aof2025/test/testcase/" ^ testfile)) @ ['\x04'] in

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

let waves_config = Waves_config.no_waves

(*
let waves_config =
  Waves_config.to_directory "/tmp/"
  |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform
;;
*)

let%expect_test "Day01_Part1" =
  let module Algo_top_day01_part1 = Algo_top.Make (struct
    let day           = "day01"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day01_part1.hierarchical simple_testbench "day01.txt";
  [%expect {| (Result (result_number 1165)) |}]
;;

let%expect_test "Day01_Part2" =
  let module Algo_top_day01_part2 = Algo_top.Make (struct
    let day           = "day01"
    let part          = "part2"
    let result_width  = result_width
  end) in
  Harness.run_advanced ~waves_config ~create:Algo_top_day01_part2.hierarchical simple_testbench "day01.txt";
  [%expect {| (Result (result_number 6496)) |}]
;;

let%expect_test "Day02_Part1" =
  let module Algo_top_day02_part1 = Algo_top.Make (struct
    let day           = "day02"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day02_part1.hierarchical simple_testbench "day02.txt";
  [%expect {| (Result (result_number 38158151648)) |}]
;;

let%expect_test "Day02_Part2" =
  let module Algo_top_day02_part2 = Algo_top.Make (struct
    let day           = "day02"
    let part          = "part2"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day02_part2.hierarchical simple_testbench "day02.txt";
  [%expect {| (Result (result_number 45283684555)) |}]
;;

let%expect_test "Day03_Part1" =
  let module Algo_top_day03_part1 = Algo_top.Make (struct
    let day           = "day03"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day03_part1.hierarchical simple_testbench "day03.txt";
  [%expect {| (Result (result_number 17158)) |}]
;;

let%expect_test "Day03_Part2" =
  let module Algo_top_day03_part2 = Algo_top.Make (struct
    let day           = "day03"
    let part          = "part2"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day03_part2.hierarchical simple_testbench "day03.txt";
  [%expect {| (Result (result_number 170449335646486)) |}]
;;

let%expect_test "Day04_Part1" =
  let module Algo_top_day04_part1 = Algo_top.Make (struct
    let day           = "day04"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day04_part1.hierarchical simple_testbench "day04.txt";
  [%expect {| (Result (result_number 1537)) |}]
;;

let%expect_test "Day04_Part2" =
  let module Algo_top_day04_part2 = Algo_top.Make (struct
    let day           = "day04"
    let part          = "part2"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day04_part2.hierarchical simple_testbench "day04.txt";
  [%expect {| (Result (result_number 8707)) |}]
;;

let%expect_test "Day06_Part1" =
  let module Algo_top_day06_part1 = Algo_top.Make (struct
    let day           = "day06"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day06_part1.hierarchical simple_testbench "day06.txt";
  [%expect {| (Result (result_number 6299564383938)) |}]
;;

let%expect_test "Day06_Part2" =
  let module Algo_top_day06_part2 = Algo_top.Make (struct
    let day           = "day06"
    let part          = "part2"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day06_part2.hierarchical simple_testbench "day06.txt";
  [%expect {| (Result (result_number 11950004808442)) |}]
;;

let%expect_test "Day07_Part1" =
  let module Algo_top_day07_part1 = Algo_top.Make (struct
    let day           = "day07"
    let part          = "part1"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day07_part1.hierarchical simple_testbench "day07.txt";
  [%expect {| (Result (result_number 1566)) |}]
;;

let%expect_test "Day07_Part2" =
  let module Algo_top_day07_part2 = Algo_top.Make (struct
    let day           = "day07"
    let part          = "part2"
    let result_width  = result_width
  end) in 
  Harness.run_advanced ~waves_config ~create:Algo_top_day07_part2.hierarchical simple_testbench "day07.txt";
  [%expect {| (Result (result_number 5921061943075)) |}]
;;
