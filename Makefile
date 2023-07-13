.DEFAULT_GOAL := all
.PHONY: version.asm

ROM_SRCS := $(filter-out cbios128.asm version.asm, $(wildcard *.asm))

all: z80rbmon.hex cpmboot.img

cbios128.asm:

ROM_SRCS:

version.asm: 
	echo VERSION text \"v1.2-DC31-`date -u +%Y%m%d%H%M`\",0 > version.asm

z80rbmon.hex: ROM_SRCS version.asm
	zmac monitor.asm
	cp zout/monitor.hex z80rbmon.hex

cbios128.hex: cbios128.asm
	zmac cbios128.asm

cpmboot.img: cbios128.hex
	srec_cat -output ./cpmboot.hex -Intel \
		./CPM22.HEX -Intel -exclude 0xE600 0xE633 \
		./zout/cbios128.hex -Intel
	srec_cat -output ./cpmboot.img -binary \
		./cpmboot.hex -Intel -offset -0xD000

clean:
	rm -rf zout
	rm -f z80rbmon.hex
	rm -f version.asm
	rm -f cpmboot.*
