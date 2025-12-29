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

- [Ocaml/Hardcaml](https://www.janestreet.com/web-app/hardcaml-docs/introduction/installing_with_opam)
- [oss-cad-suite](https://github.com/YosysHQ/oss-cad-suite-build) This includes yosys / nextpnr with ECP5 support and icesprog command for programming FPGA
- [iCESugar-Pro development board](https://github.com/wuxx/icesugar-pro)

## Overview

- My goal is to implement everything on FPGA without any host side preprocessing - i.e. the ASCII input text file is pumped into FPGA as-is (however need to append a byte of EOT - ASCII \0x04) on an UART interface, and FPGA returns a signal number which is the answer of the puzzle.
- `design_top.ml` is the top sheet, it just has a pair of UART TX/RX pin for host commuicating and a clock pin connecting to a 25MHz onboard clock source. The UART TX/RX module is pretty much borrowed from [Anish's repo](https://github.com/asinghani/advent-of-hardcaml-2024/blob/main/fpga/src/uart.ml).
- `algo_top.ml` has a byte UART RX input interface, it instantiates the selected design based on the day/part parameter and shift out the result on a UART TX byte output interface. All design outputs a 8 byte result (uint64_t big endian). This level is also the DUT for my testbench `test_algo_top.ml`.

## Puzzles
### Day01
### Day02
### Day03
### Day04
### Day05
### Day06
### Day07
### Day08
### Day09
### Day10
