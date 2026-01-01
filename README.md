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
FPGA encodes the grid into a bitmap ('.' -> GND, '@' -> VDD) and pad an extra row/column of GND around the grid to make processing easier. Then it runs a 3 level pipeline that caches the adjecent 3 grid rows and checks the 8 bits around each bit in the middle row. 

**Part 1:** Just need to count the paper rolls for each bit position. I found out the popcount/reduce function fit this purpose nicely.

**Part 2:** The grid needs to be processed with multiple passes. For each pass the rows are updated with the new value and feed into a FIFO. The algo stops when there is no update in a certain pass.

### Day05
**Part 1:** The idea is to store the ranges into a memory and for each id, it checks all memory entries to see if there is a match.

**Part 2:** The memory entries are sorted using bubble sort algorithm and then adjacent memory entries are "merged" if they are overlapping with each other. 

I found the RTL inference of TDP memory on yosys ECP5 flow is extremely picky - for part 2 I struggled quite a bit but still cannot get the TDP memory reliably inferred. The algo could be re-writed with single port memory but I decided to not spend more time here - for all the later puzzles I use a single port memory whenever memory inference is needed.

### Day06
**Part 1:** The input numbers are converted into binary and partial compute results are pushed into FIFOs as each row coming into FPGA. Given we only know the operator in the last row, both multiplication and addtition are computed beforehand so we can simply mux between them based on the operator.

**Part 2:** A FIFO entry is created for each digit/ASCII character in a row and the partial number (multiply by 10 and add) are computed as each row coming into FPGA. The last row that has the operator triggers the final multiplication / addition computation.

### Day07
**Part 1:** Somewhat similar to Day4 - the input is encoded into bitmap and processed row by row with some kernel function. 

**Part 2:** We need a list of registers to record the "weight" of the light at each column index. This solution is synthesizble with real resource usage, but unfortuately the 64bit * 150column register array doesn't fit nicely on my tiny FPGA. I can think of other solutions which use less resource by introducing a memory/FIFO, but that'd make the design rather ugly and I prefer not to implement that way. 

### Day08
### Day09
**Part 1:** Quite straightforward, store the points in a memory and iterate through all pairs of points to find the largest rectangle area.

**Part 2:** Need an additional validation stage to make sure the rectangle falls within the green tiles boundary. There are 2 things we want to validate: 1. The other 2 corners of the rectangle are also within the boundary. 2. No lines between red tile can intersect with the 4 edges.

### Day10
**Part 1:** Note that press the same button twice cancels the effect, so we only need to try press bottons at most once. It also means we want to try out all the possible combinations of the buttons. The combinations of n numbers can be represented by bitmap (0 ... 2^n-1). I have a signal wire_comb loops through these value and the result is checked/updated on the fly.

**Part 2:** This is hard. I've adopted the method discussed in https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/. In short, all the possible press combinations that can reach a certain pattern is pre-computed and stored into a memory, after that we search all the possible cases to reach the joltages level. The method itself involves recursive function so I had to design a "stack" in FPGA to save the temporary state when we are searching through the child cases. Tooked me a while to debug and make sure the algo handles all the edge cases, but glad that it worked in the end :)
