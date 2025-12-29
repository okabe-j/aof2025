## Advent of FPGA 2025

My attempts to solve AOC2025 puzzle with Hardcaml

All solutions are passing simulation testbench, the generated Verilog file is synthesizable via yosys and routable with nextpnr, targeting a iCESugar-Pro development board with a Lattice ECP5-LEF5U-25F FPGA (Just costs 258CNY ~ 37USD on Taobao)

## Setup

To trigger simulation testbench, run below command from root directory, it runs all testcase for all implemented puzzles.
```
dune test
```

To generate verilog RTL, do:
```
dune build
bin/generate.exe aof2025 > rtl/design_top.v
```
Then for the design implementation, type `make` under rtl directory, it triggers the synthesize & PNR procedure and generates a `design_top.bit` bitstream.
To program the bitstream on to development board, do:
```
icesprog design_top.bit
```
The python script I use to communicate with FPGA from host side is `pyserial/serial_listener.py`
You'll need to install below dependencies to run above commands:

- Ocaml/Hardcaml: https://www.janestreet.com/web-app/hardcaml-docs/introduction/installing_with_opam
- oss-cad-suite: https://github.com/YosysHQ/oss-cad-suite-build This includes yosys / nextpnr with ECP5 support and icesprog command for programming FPGA
- iCESugar-Pro development board: https://github.com/wuxx/icesugar-pro

## Overview
