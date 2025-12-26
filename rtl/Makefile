TARGET=design_top
TOP=design_top

OBJS+=design_top.v

TRELLIS=/usr/local/share/trellis

all: ${TARGET}.bit

$(TARGET).json: $(OBJS)
	yosys -g -p "synth_ecp5 -json $@" $(OBJS)

$(TARGET)_out.config: $(TARGET).json
	nextpnr-ecp5 --25k --package CABGA256 --speed 6 --json $< --textcfg $@ --lpf design_top.lpf --freq 65

$(TARGET).bit: $(TARGET)_out.config
	ecppack --svf ${TARGET}.svf $< $@

${TARGET}.svf : ${TARGET}.bit

prog: ${TARGET}.svf
	openFPGALoader -c digilent_hs2 $(TARGET).bit

clean:
	rm -f *.svf *.bit *.config *.ys *.json

.PHONY: prog clean
