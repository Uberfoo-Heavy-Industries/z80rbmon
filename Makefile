.DEFAULT_GOAL := all
.PHONY: version.asm

all: z80rbmon.hex

*.asm:

version.asm:
	echo VERSION text \"v1-`date -u +%Y%m%d%H%M`\",0 > version.asm

z80rbmon.hex: *.asm version.asm
	zmac monitor.asm
	cp zout/monitor.hex z80rbmon.hex

clean:
	rm -rf zout
	rm -f z80rbmon.hex
	rm -f version.asm