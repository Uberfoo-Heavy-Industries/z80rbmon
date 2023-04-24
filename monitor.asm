;------------------------------------------------------------------------------
;
; Z80 Retro Badge Monitor firmware
;
;------------------------------------------------------------------------------
; General Equates
;------------------------------------------------------------------------------

CR			equ	0x0D
LF			equ	0x0A
ESC			equ	0x1B
CTRLC		equ	0x03
CLRS		equ	0x0C

		INCLUDE "board.asm"		; Board specific definitions

SER_BUFSIZE		equ	0x40
SER_FULLSIZE	equ	0x30
SER_EMPTYSIZE	equ	5
SERBUF_START	equ	CBOOT_BASE	; Place serial buffer at the top of RAM

loadAddr	equ	0xD000	; CP/M load address
numSecs		equ	24		; Number of 512 sectors to be loaded


;------------------------------------------------------------------------------
; Variables in RAM
;------------------------------------------------------------------------------
			org	SERBUF_START
serBuf		ds	SER_BUFSIZE
serInPtr	ds	2
serRdPtr	ds	2
serBufUsed	ds	1
SERABITS	ds	1
TMPKEYBFR   ds	1

secNo		ds	1
dmaAddr		ds	2
rndSeed1	ds	2
rndSeed2	ds	2

decryptBuf  ds  255
stackSpace	ds	255
STACK		equ	$

;------------------------------------------------------------------------------
;                         START OF MONITOR ROM
;------------------------------------------------------------------------------
MON		org		0x0000		; MONITOR ROM RESET VECTOR
		di					; Disable Interrupts
		jp		MON_INIT	; Initialize Hardware and go

;------------------------------------------------------------------------------
; interrupt vector when SIO ch.A has a char available in its buffer
;------------------------------------------------------------------------------
		org     0x000C
		defw    rx_avail

;------------------------------------------------------------------------------
; interrupt vector for SIO ch.A special conditions (i.e. buf overrun)
;------------------------------------------------------------------------------
		org     0x000E
		defw    rx_spec_cond


		INCLUDE "version.asm"	; Version number definition

;------------------------------------------------------------------------------
; Initialise hardware and start main loop
;------------------------------------------------------------------------------
MON_INIT:
	
	ld		sp, STACK	; Set the Stack Pointer

	ld		hl, serBuf      ; set beginning of input buffer
	ld		(serInPtr), hl  ; for incoming chars to store into buffer
	ld		(serRdPtr), hl  ; and for chars to be read from buffer

	xor		a				; 0 to accumulator
	ld		(serBufUsed), a
	ld		(TMPKEYBFR),a

	call	initialize_port

	; Display the start message
	ld   	hl, INITTXT
	call 	write_string

	call 	LED_RED
	call 	INIT_RND

	xor		a		; 0 to accumulator
	ld		I,a     ; set high byte of interrupt vectors to point to page 0
	im		2       ; interrupt mode 2
	ei

cycle:
	call	RND_FLASH_CYCLE

	call	rx

	cp		' '
	jr		nz, cycle
	
	; Clear message on console
	ld		a, 0x0C
	call	write_char

	call 	LED_CLEAR_ALL

	call 	write_newline

	jp 		monitor_cold_start
	
delay:
	push 	bc
delay1:
	ld		bc, 0xFFFF
delay2:
	dec		bc
	ld		a, b
	or		c
	jr		nz, delay2
	dec		de
	ld		a, d
	or		e
	jr		nz, delay1
	pop		bc
	ret

delay_short:
	dec		bc
	ld		a, b
	or		c
	jr		nz, delay_short
	ret

;
;Simple monitor program for the Z80 Retro Badge
monitor_cold_start:	
	di						; disable interrupts
	
	call	LED_RED
	call	CF_INIT
	call	LED_BLUE

	ld		hl,monitor_message
	call	write_string
	ld		hl,VERSION
	call	write_string

	call	write_newline
	call	write_newline
	
	ei                      ; enable interrupts

monitor_warm_start:
	call	LED_GREEN
	
	call	write_newline	;routine program return here to avoid re-initialization of port
	ld		a,03eh			;cursor symbol
	call	write_char
	ld		hl,buffer
	call	rts_on
	call	get_line		;get monitor input string (command)
	call	write_newline
	call	parse			;interprets command, returns with address to jump to in hl
	jp		(hl)
;
;Parses an input line stored in buffer for available commands as described in parse table.
;Returns with address of jump to action for the command in hl
parse:			
	ld		de,buffer
	call	ENCRYPT
	ld		bc,parse_table		;bc is pointer to parse_table
parse_start:
	ld		a, (bc)			;get pointer to match string from parse table
	ld		e, a
	inc		bc
	ld		a, (bc)			
	ld		d, a			;de will is pointer to strings for matching
	ld		a, (de)			;get first char from match string
	or		0x00			;zero?
	jp		z, parser_exit		;yes, exit no_match
	ld		hl, decryptBuf		;no, parse input string 
match_loop:
	cp		(hl)			;compare buffer char with match string char
	jp		nz,no_match		;no match, go to next match string
	or		0x00			;end of strings (zero)?
	jp		z,parser_exit		;yes, matching string found
	inc		de			;match so far, point to next char in match string
	ld		a, (de)			;get next character from match string
	inc		hl			;and point to next char in input string
	jp		match_loop		;check for match
no_match:
	inc		bc			;skip over jump target to
	inc		bc
	inc		bc			;get address of next matching string
	jp		parse_start
parser_exit:
	inc		bc			;skip to address of jump for match
	ld		a, (bc)
	ld		l, a
	inc		bc
	ld		a, (bc)
	ld		h, a			;returns with jump address in hl
	ret

;Memory dump program
;Input 4-digit hexadecimal address
;Calls memory_dump subroutine
dump_jump:
	ld		hl, dump_message		;Display greeting
	call	write_string
	ld		hl, address_entry_msg	;get ready to get address
	call	write_string
	call	address_entry		;returns with address in hl
	call	write_newline
	call	memory_dump
	jp		monitor_warm_start
;
;Hex loader, displays formatted input
load_jump:
	ld		hl, load_message		;Display greeting
	call	write_string		;get address to load
	ld		hl, address_entry_msg	;get ready to get address
	call	write_string
	call	address_entry
	call	write_newline
	call	memory_load
	jp		monitor_warm_start
;
;
;Jump and run do the same thing: get an address and jump to it.
run_jump:
	ld		hl, run_message		;Display greeting
	call	write_string
	ld		hl, address_entry_msg	;get ready to get address
	call	write_string
	call	address_entry
	jp		(hl)
;
;Help and ? do the same thing, display the available commands
help_jump:
	ld		hl, help_message
	call	write_string
	ld		bc, parse_table		;table with pointers to command strings
	ld		d,12
help_loop:
	ld		a, (bc)			;displays the strings for matching commands,
	ld		l, a			;getting the string addresses from the
	inc		bc			;parse table
	ld		a, (bc)			;pass address of string to hl through a reg
	ld		h, a
	dec		d				; exit after we reach the last listed command
	jp		z, help_done
	push	hl
	push	bc			;write_char uses b register
	push	d
	ld		d,h
	ld		e,l
	call	ENCRYPT
	pop		d
	ld		a, 0x20			;space char
	call	write_char
	pop		bc
	ld		hl,decryptBuf
	call	write_string		;writes match string
	pop		hl
	inc		bc			;pass over jump address in table
	inc		bc
	inc		bc
	jp		help_loop
help_done:
	jp		monitor_warm_start

;Disk read. Need memory address to place data, LBA of sector to read
diskrd_jump:
	ld		hl, diskrd_message
	call	write_string
	ld		hl, address_entry_msg
	call	write_string
	call	address_entry
	call	write_newline
	push	hl
	ld		hl, LBA_entry_string
	call	write_string
	call	decimal_entry
	ld		b,h
	ld		c,l
	ld		e, 0x00
	pop		hl
	call	disk_read
	jp		monitor_warm_start
diskwr_jump:
	ld		hl, diskwr_message
	call	write_string
	ld		hl, address_entry_msg
	call	write_string
	call	address_entry
	call	write_newline
	push	hl
	ld		hl, LBA_entry_string
	call	write_string
	call	decimal_entry
	ld		b, h
	ld		c, l
	ld		e, 0x00
	pop		hl
	call	disk_write
	jp		monitor_warm_start
;Prints message for no match to entered command
no_match_jump:
	ld		hl, no_match_message
	call	write_string
	ld		hl, buffer
	call	write_string
	jp		monitor_warm_start

;------------------------------------------------------------------------------
; CP/M load command
;------------------------------------------------------------------------------
boot_jump:
	ld		hl, BOOTTXT
	call	write_string
	call	write_newline
	call	rx
	ret		z		; Cancel if CTRL-C
	and		0x5F 	; uppercase
	cp 		'Y'
	jp		z, boot_jump2
	ret
boot_jump2:
	ld 		hl, BOOTTXT2
	call	write_string
	call	write_newline

	ld		hl, 0x3000
	ld		a, (secNo)
	ld		c, a
	ld		a, 0
	ld	 	b, a
	ld		e, a

	call	disk_read

	ld		a, 0
	push	af
	ld		hl,(0x3000)
	jp		(hl)

;------------------------------------------------------------------------------
; CP/M load command
;------------------------------------------------------------------------------
cpm_jump:
	ld		hl, CPMTXT
	call	write_string
	call	write_newline
	call	rx
	ret		z		; Cancel if CTRL-C
	and		0x5F 	; uppercase
	cp 		'Y'
	jp		z, cpm_jump2
	ret
cpm_jump2:
	ld 		hl, CPMTXT2
	call	write_string
	call	write_newline

	ld		b,numSecs

	ld		a, 0
	ld		(secNo), a
	ld		hl, loadAddr
	ld		(dmaAddr), hl
processSectors:
	push	b
	
	ld		a, (secNo)
	ld		c, a		; Sector number into c
	ld		a, 0
	ld		b,a			; set b and e to 0
	ld		e,a

	call	disk_read
	pop		b

	ld		de, 0x0200
	ld		hl, (dmaAddr)
	add		hl, de
	ld		(dmaAddr), hl
	ld		a, (secNo)
	inc		a
	ld		(secNo), a

	djnz	processSectors

; Start CP/M using entry at top of BIOS
; The current active console stream ID is pushed onto the sta ck
; to allow the CBIOS to pick it up
; 0 = SIO A, 1 = SIO B
		
	ld		a, 0
	push	af
	ld		hl,($FFFE)
	jp		(hl)

led_jump:
	; Clear message on console
	ld		a, 0x0C
	call	write_char

	; Display the "Press space to start" message
	ld   	hl, INITTXT
	call 	write_string

	call 	RND_FLASH_CYCLE

	call	rx				; Check for spacebar press
	cp		' '
	jr		nz, led_jump

	jp		monitor_warm_start

about_jump:
	ld		hl,about_txt
	call	write_string

	jp		monitor_warm_start

xyzzy_jump:
	ld		de,secret_txt
	call	ENCRYPT
	ld		hl,decryptBuf
	call	write_string
	call	write_newline
	call	write_newline

	jp		monitor_warm_start

	INCLUDE "led.asm"
	INCLUDE "math.asm"
	INCLUDE "sio.asm"
	INCLUDE "stdout.asm"
	INCLUDE "cf_card.asm"
	INCLUDE "utils.asm"

;------------------------------------------------------------------------------

CKSUMERR:
		defm	"Checksum error"
		defm	CR,LF,0

INITTXT:
		defm	0x0C
		defm	"Press [SPACE] to activate console"
		defm	CR,LF,0

LDETXT:
		defm	"Load complete."
		defm	CR,LF,0

BOOTTXT:
		defm	CR,LF
		defm	"Boot from CF card?",0

BOOTTXT2:
		defm	CR,LF
		defm	"Booting from CF card..."
		defm	CR,LF,0
CPMTXT:
		defm	CR,LF
		defm	"Boot CP/M?",0

CPMTXT2:
		defm	CR,LF
		defm	"Loading CP/M..."
		defm	CR,LF,0

;
;Monitor data structures:
;
monitor_message:
	defm	CR,LF
	defm	ESC,"[32m"
	defm	" _____   ____  ____     ____       __                ____            __         ",CR,LF
	defm	"/__  /  ( __ )/ __ \   / __ \___  / /__________     / __ )____ _____/ /___ ____ ",CR,LF
	defm	"  / /  / __  / / / /  / /_/ / _ \/ __/ ___/ __ \   / __  / __ `/ __  / __ `/ _ \",CR,LF
	defm	" / /__/ /_/ / /_/ /  / _, _/  __/ /_/ /  / /_/ /  / /_/ / /_/ / /_/ / /_/ /  __/",CR,LF
	defm	"/____/\____/\____/  /_/ |_|\___/\__/_/   \____/  /_____/\__,_/\__,_/\__, /\___/ ",CR,LF
	defm	"                                                                   /____/       ",CR,LF
	defm	CR,LF
	defm	ESC,"[97m"
	defm	"                      by Uberfoo Heavy Industries",CR,LF
	defm	"                       Version: ",ESC,"[0m",0

about_txt:
	defm	CR,LF
	defm	ESC,"[97m"
	defm	"                      - Z80 Retro Badge Monitor -",CR,LF
	defm    ESC,"[0m"
	defm	CR,LF
	defm	"This firmware was custom made for the Z80 Retro Badge",CR,LF
	defm    "by Uberfoo Heavy Industries",CR,LF
	defm	CR,LF
	defm	"The contents of this firmware includes work by Grant Searle and Joel Owens.",CR,LF
	defm	"Grant Searle can be found at: http://searle.wales/",CR,LF
	defm    "or home.micros01@btinternet.com",CR,LF
	defm	CR,LF
	defm	"Created for DEF CON 31. More information at: https://heavy.uberfoo.net/z80rb",CR,LF
	defm	CR,LF
	defm	"Special thanks to madamorr and flux.",CR,LF,0
	defm    "flag{iFd1zlNrldH2By8JEQLVTOzD}",CR,LF,0

secret_txt:
	defm 0x4D, 0x75, 0x51, 0x71, 0x1F, 0x2E, 0x0A, 0x69, 0x40, 0x60, 0x53, 0x78
	defm 0x0B, 0x74, 0x47, 0x64, 0x57, 0x67, 0x4A, 0x6E, 0x0B, 0x6F, 0x40, 0x75
	defm 0x0A, 0x7B, 0x1D, 0x31, 0x57, 0x63, 0x0A, 0x67, 0x49, 0x60, 0x42, 0x2E
	defm 0x40, 0x42, 0x49, 0x46, 0x67, 0x67, 0x4C, 0x59, 0x43, 0x53, 0x68, 0x68
	defm 0x6C, 0x32, 0x67, 0x43, 0x1C, 0x4E, 0x73, 0x66, 0x4F, 0x67, 0x17, 0x55
	defm 0

no_match_message:	defm	"? ",0
help_message:		defm	"Commands implemented:",CR,LF,0
dump_message:		defm	"Displays a 256-byte block of memory.",CR,LF,0
load_message:		defm	"Enter hex bytes starting at memory location.",CR,LF,0
run_message:		defm	"Will jump to (execute) program at address entered.",CR,LF,0
diskrd_message:		defm	"Reads one sector from disk to memory.",CR,LF,0
diskwr_message:		defm	"Writes one sector from memory to disk.",CR,LF,0

;Strings for matching:
dump_string:		defm	"AtHq",0
jump_string:		defm	"OtHq",0
run_string:			defm	"WtK",0
question_string:	defm	0x1A,0
help_string:		defm	"MdIq",0
diskrd_string:		defm	"AhVjWe",0
diskwr_string:		defm	"AhVjRs",0
cpm_string:			defm	"FqH",0
boot_string:		defm	"GnJu",0
led_string:			defm	"IdA",0
about_string:		defm	"DcJtQ",0
xyzzy_string:		defm	"]x_{\",0
no_match_string:	defm	0,0

;Table for matching strings to jumps
parse_table:	defw	dump_string,dump_jump
				defw	jump_string,run_jump,run_string,run_jump
				defw	question_string,help_jump,help_string,help_jump
				defw	diskrd_string,diskrd_jump,diskwr_string,diskwr_jump
				defw	cpm_string,cpm_jump,boot_string,boot_jump
				defw	led_string,led_jump,about_string,about_jump
				defw    xyzzy_string,xyzzy_jump,no_match_string,no_match_jump

;------------------------------------------------------------------------------



FINIS:		.end	


