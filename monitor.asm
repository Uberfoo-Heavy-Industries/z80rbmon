;==================================================================================
; Contents of this file are copyright Grant Searle
; HEX routines from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.wales/
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================

;------------------------------------------------------------------------------
;
; Z80 Monitor Rom
;
;------------------------------------------------------------------------------
; General Equates
;------------------------------------------------------------------------------

CR			equ	0x0D
LF			equ	0x0A
ESC			equ	0x1B
CTRLC		equ	0x03
CLRS		equ	0x0C

;BASIC cold and warm entry points
BASCLD		equ	0x2000
BASWRM		equ	0x2003

		INCLUDE "board.asm"		; Board specific definitions

; CF registers
CF_DATA		equ	CFBASE
CF_FEATURES	equ	0x31
CF_ERROR	equ	0x31
CF_SECCOUNT	equ	0x32
CF_SECTOR	equ	0x33
CF_CYL_LOW	equ	0x34
CF_CYL_HI	equ	0x35
CF_HEAD		equ	0x36
CF_STATUS	equ	0x37
CF_COMMAND	equ	0x37
CF_LBA0		equ	0x33
CF_LBA1		equ	0x34
CF_LBA2		equ	0x35
CF_LBA3		equ	0x36

;CF Features
CF_8BIT			equ	1
CF_NOCACHE		equ	0x82
;CF Commands
CF_READ_SEC		equ	0x20
CF_WRITE_SEC	equ	0x30
CF_SET_FEAT		equ	0xEF

SER_BUFSIZE		equ	0x40
SER_FULLSIZE	equ	0x30
SER_EMPTYSIZE	equ	5
SERBUF_START	equ	0x2000

loadAddr	equ	0xD000	; CP/M load address
numSecs		equ	24		; Number of 512 sectors to be loaded


			org	0x2000
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

stackSpace	ds	32
STACK		equ	$

;------------------------------------------------------------------------------
;                         START OF MONITOR ROM
;------------------------------------------------------------------------------
MON		org		0x0000		; MONITOR ROM RESET VECTOR

;------------------------------------------------------------------------------
; Reset
;------------------------------------------------------------------------------
RST00	di					;Disable INTerrupts
		jp		MON_INIT	;Initialize Hardware and go

;------------------------------------------------------------------------------
; TX a character over RS232 wait for TXDONE first.
;------------------------------------------------------------------------------
RST08	org		0x0008
		jp		write_char

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

;------------------------------------------------------------------------------
; RX a character from buffer wait until char ready.
;------------------------------------------------------------------------------
RST10	org		0x0010
		jp		rx

;------------------------------------------------------------------------------
; Check input buffer status
;------------------------------------------------------------------------------
RST18	org		0x0018
		jp		ckinchar


		INCLUDE "version.asm"	; Version number definition

;------------------------------------------------------------------------------
; Initialise hardware and start main loop
;------------------------------------------------------------------------------
MON_INIT:
	ld		sp, STACK	; Set the Stack Pointer

	ld		hl, serBuf      ; set beginning of input buffer
	ld		(serInPtr), hl  ; for incoming chars to store into buffer
	ld		(serRdPtr), hl  ; and for chars to be read from buffer

	xor		a				;0 to accumulator
	ld		(serBufUsed), a

	call	initialize_port

	; Interrupt vector in page 0
	xor		a
	ld		i, a
	im		2
	ei

	; Display the "Press space to start" message
	ld   	hl, INITTXT
	call 	write_string

	call 	LED_RED
	call 	INIT_RND

cycle:
	call	RND_FLASH_CYCLE

	ld		hl, question_string
	call	write_string

	call	rx
	cp		' '
	jr		nz, cycle


	ld		hl, question_string
	call	write_string

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
	di
	call	LED_RED
	call	CF_INIT
	call	LED_BLUE
	ld		hl,monitor_message
	call	write_string
	ld		hl,VERSION
	call	write_string
	call	write_newline
	call	write_newline
	xor		a
	ld		I,a             ; set high byte of interrupt vectors to point to page 0
	im		2               ; interrupt mode 2
	ei                      ; enable interrupts

monitor_warm_start:
	call	LED_GREEN
	call	write_newline	;routine program return here to avoid re-initialization of port
	ld		a,03eh			;cursor symbol
	call	write_char
	ld		hl,buffer
	call	get_line		;get monitor input string (command)
	call	write_newline
	call	parse			;interprets command, returns with address to jump to in hl
	jp		(hl)
;
;Parses an input line stored in buffer for available commands as described in parse table.
;Returns with address of jump to action for the command in hl
parse:			
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
	ld		hl, buffer		;no, parse input string 
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
help_loop:
	ld		a, (bc)			;displays the strings for matching commands,
	ld		l, a			;getting the string addresses from the
	inc		bc			;parse table
	ld		a, (bc)			;pass address of string to hl through a reg
	ld		h, a
	ld		a, (hl)			;hl now points to start of match string
	or		000h			;exit if no_match string
	jp		z, help_done
	push	bc			;write_char uses b register
	ld		a, 0x20			;space char
	call	write_char
	pop		bc
	call	write_string		;writes match string
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
; LOAD Intel Hex format file from the console.
; [Intel Hex Format is:
; 1) Colon (Frame 0)
; 2) Record Length Field (Frames 1 and 2)
; 3) Load Address Field (Frames 3,4,5,6)
; 4) Record Type Field (Frames 7 and 8)
; 5) Data Field (Frames 9 to 9+2*(Record Length)-1
; 6) Checksum Field - Sum of all byte values from Record Length to and 
;   including Checksum Field = 0 ]
;------------------------------------------------------------------------------	
; LOAD		ld   E,0	; First two Characters is the Record Length Field
; 		call GET2	; Get us two characters into bc, convert it to a byte <A>
; 		ld   D,a	; Load Record Length count into D
; 		call GET2	; Get next two characters, Memory Load Address <H>
; 		ld   H,a	; put value in H register.
; 		call GET2	; Get next two characters, Memory Load Address <L>
; 		ld   L,a	; put value in L register.
; 		call GET2	; Get next two characters, Record Field Type
; 		cp   $01	; Record Field Type 00 is Data, 01 is End of File
; 		jr   NZ,LOAD2	; Must be the end of that file
; 		call GET2	; Get next two characters, assemble into byte
; 		ld   a,E	; Recall the Checksum byte
; 		and  a		; Is it Zero?
; 		jr   Z,LOAD00	; Print footer reached message
; 		jr   LOADERR	; Checksums don't add up, Error out
		
; LOAD2		ld   a,D	; Retrieve line character counter	
; 		and  a		; Are we done with this line?
; 		jr   Z,LOAD3	; Get two more ascii characters, build a byte and checksum
; 		call GET2	; Get next two chars, convert to byte in a, checksum it
; 		ld   (hl),a	; Move converted byte in a to memory location
; 		inc  hl		; Increment pointer to next memory location	
; 		ld   a,'.'	; Print out a "." for every byte loaded
; 		RST  08H	;
; 		DEC  D		; Decrement line character counter
; 		jr   LOAD2	; and keep loading into memory until line is complete
		
; LOAD3		call GET2	; Get two chars, build byte and checksum
; 		ld   a,E	; Check the checksum value
; 		and  a		; Is it zero?
; 		RET  Z

; LOADERR		ld   hl,CKSUMERR  ; Get "Checksum Error" message
; 		call write_string	; Print Message from (hl) and terminate the load
; 		call	write_newline
; 		RET

; LOAD00  	ld   hl,LDETXT	; Print load complete message
; 		call write_string
; 		call	write_newline
; 		RET

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


	INCLUDE "led.asm"
	INCLUDE "random.asm"
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
	defm	" _____   ____  ____     ____       __                ____            __         ",CR,LF
	defm	"/__  /  ( __ )/ __ \   / __ \___  / /__________     / __ )____ _____/ /___ ____ ",CR,LF
	defm	"  / /  / __  / / / /  / /_/ / _ \/ __/ ___/ __ \   / __  / __ `/ __  / __ `/ _ \",CR,LF
	defm	" / /__/ /_/ / /_/ /  / _, _/  __/ /_/ /  / /_/ /  / /_/ / /_/ / /_/ / /_/ /  __/",CR,LF
	defm	"/____/\____/\____/  /_/ |_|\___/\__/_/   \____/  /_____/\__,_/\__,_/\__, /\___/ ",CR,LF
	defm	"                                                                   /____/       ",CR,LF
	defm	CR,LF
	defm	"                      by Uberfoo Heavy Industries",CR,LF
	defm	"                       Version: ",0


                                                                            
no_match_message:	defm	"? ",0
help_message:		defm	"Commands implemented:",CR,LF,0
dump_message:		defm	"Displays a 256-byte block of memory.",CR,LF,0
load_message:		defm	"Enter hex bytes starting at memory location.",CR,LF,0
run_message:		defm	"Will jump to (execute) program at address entered.",CR,LF,0
diskrd_message:		defm	"Reads one sector from disk to memory.",CR,LF,0
diskwr_message:		defm	"Writes one sector from memory to disk.",CR,LF,0
boot_message:		defm	"Boots from the first sector of the disk.",CR,LF,0
;Strings for matching:
dump_string:		defm	"dump",0
load_string:		defm	"load",0
jump_string:		defm	"jump",0
run_string:			defm	"run",0
question_string:	defm	"?",0
help_string:		defm	"help",0
diskrd_string:		defm	"diskrd",0
diskwr_string:		defm	"diskwr",0
cpm_string:			defm	"cpm",0
boot_string:		defm	"boot",0
no_match_string:	defm	0,0
;Table for matching strings to jumps
parse_table:	defw	dump_string,dump_jump,load_string,load_jump
				defw	jump_string,run_jump,run_string,run_jump
				defw	question_string,help_jump,help_string,help_jump
				defw	diskrd_string,diskrd_jump,diskwr_string,diskwr_jump
				defw	cpm_string,cpm_jump,boot_string,boot_jump
				defw	no_match_string,no_match_jump

;------------------------------------------------------------------------------

FINIS:		.end	


