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
- `dayxx.ml` is where the solution for each puzzle implemented. Most of the solution has a Loader module that parses the input and handles stuff like BCD to binary conversion, which makes the input easier for the actual module to process. 

## Puzzles
### Day01
FPGA maintains a dial number initialized to 50. Every clock cycle FPGA processes an input line, left rotation would turn into a subtraction and right rotation becomes an addition. At the end of every clock cycle, the dial number is adjust back to the range [0, 100).

**Part 1:** FPGA only process upto 2 least significant BCD digits, everything larger than 100 is ignored as it turns the dial exactly one circle back to the same number.

**Part 2:** The 3rd BCD digit would add to the result directly.

### Day02
For each input range, FPGA iterate through all numbers between the range_begin and range_end, one number per clock cycle. The number remains in BCD format - this makes it easier for FPGA to do pattern matching on its digits. The number passes pattern matching is converted into binary and added to the final result.

**Part 1:** I was able to come up with a recursive function `check_repeat_twice` which handles the pattern matching part, super excited that it compiles into RTL!

**Part 2:** I had to use a bunch of functions to implement the pattern matching. Think there might be a cleaner solution? 

One potential enhancement I could think of is to build multiple "kernels" the handles range iteration and pattern matching in parallel, as each input range is independent. (And the FPGA still having enough resource!)

### Day03
My solution is to use a shift register of BCD digits, it behaves pretty much like a stack. For the next digit from input, it compares with the top of the stack and smaller digits would pop / shift out from the shreg before it could shift in. However it also needs to make sure that there'll be enough digits in shreg when approaching the end of the input line.

For part 1 the shreg holds 2 digits and for part 2 it holds 12 digits, algorithm is the same otherwise.

### Day04
### Day05
### Day06
### Day07
### Day08
### Day09
### Day10
