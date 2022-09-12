.DEFAULT_GOAL := all

all: z80rbmon.hex

monitor.asm:

z80rbmon.hex: monitor.asm
	zmac monitor.asm
	cp zout/monitor.hex z80rbmon.hex

clean:
	rm -rf zout
	rm -f z80rbmon.hex