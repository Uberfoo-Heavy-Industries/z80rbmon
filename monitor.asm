;==================================================================================
; Contents of this file are copyright Grant Searle
; HEX routines from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
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
CF_FEATURES	equ	$31
CF_ERROR	equ	$31
CF_SECCOUNT	equ	$32
CF_SECTOR	equ	$33
CF_CYL_LOW	equ	$34
CF_CYL_HI	equ	$35
CF_HEAD		equ	$36
CF_STATUS	equ	$37
CF_COMMAND	equ	$37
CF_LBA0		equ	$33
CF_LBA1		equ	$34
CF_LBA2		equ	$35
CF_LBA3		equ	$36

;CF Features
CF_8BIT			equ	1
CF_NOCACHE		equ	082H
;CF Commands
CF_READ_SEC		equ	020H
CF_WRITE_SEC	equ	030H
CF_SET_FEAT		equ	0EFH

SER_BUFSIZE		equ	40H
SER_FULLSIZE	equ	30H
SER_EMPTYSIZE	equ	5

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
MONSTACK	ds	$
;Need to have stack in upper RAM, but not in area of CP/M or RAM monitor.
ROM_monitor_stack:	equ	0xdbff		;upper TPA in RAM, below RAM monitor
TEMPSTACK	equ	0x20ED	; Top of BASIC line input buffer so is "free ram" when BASIC resets

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

;------------------------------------------------------------------------------
; Initialise hardware and start main loop
;------------------------------------------------------------------------------
MON_INIT
		ld		SP,MONSTACK	; Set the Stack Pointer

		ld		HL,serBuf
		ld		(serInPtr),HL

		xor		a		;0 to accumulator
		ld		(serBufUsed),A

		call	initialize_port

		ld		sp,ROM_monitor_stack
		
		; Interrupt vector in page 0
		xor		a
		ld		i,a
		im		2
		ei

		; Display the "Press space to start" message
    	ld   	HL,INITTXT
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
		call 	LED_BLUE

		CALL 	write_newline

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
;Simple monitor program for CPUville Z80 computer with serial interface.
monitor_cold_start:	
	di
	ld		hl, TEMPSTACK   ; load temp stack pointer
	ld		sp, hl          ; set stack to temp stack pointer
	ld		hl, serBuf      ; set beginning of input buffer
	ld		(serInPtr), hl  ; for incoming chars to store into buffer
	ld		(serRdPtr), hl  ; and for chars to be read from buffer
	xor		a               ; reset a
	ld		(serBufUsed),a  ; actual buffer size is 0
	; call	initialize_port
	call	LED_RED
	call	CF_INIT
	call	LED_BLUE
	ld		sp,ROM_monitor_stack
	ld		hl,monitor_message
	call	write_string
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
; LOAD		LD   E,0	; First two Characters is the Record Length Field
; 		CALL GET2	; Get us two characters into BC, convert it to a byte <A>
; 		LD   D,A	; Load Record Length count into D
; 		CALL GET2	; Get next two characters, Memory Load Address <H>
; 		LD   H,A	; put value in H register.
; 		CALL GET2	; Get next two characters, Memory Load Address <L>
; 		LD   L,A	; put value in L register.
; 		CALL GET2	; Get next two characters, Record Field Type
; 		CP   $01	; Record Field Type 00 is Data, 01 is End of File
; 		JR   NZ,LOAD2	; Must be the end of that file
; 		CALL GET2	; Get next two characters, assemble into byte
; 		LD   A,E	; Recall the Checksum byte
; 		AND  A		; Is it Zero?
; 		JR   Z,LOAD00	; Print footer reached message
; 		JR   LOADERR	; Checksums don't add up, Error out
		
; LOAD2		LD   A,D	; Retrieve line character counter	
; 		AND  A		; Are we done with this line?
; 		JR   Z,LOAD3	; Get two more ascii characters, build a byte and checksum
; 		CALL GET2	; Get next two chars, convert to byte in A, checksum it
; 		LD   (HL),A	; Move converted byte in A to memory location
; 		INC  HL		; Increment pointer to next memory location	
; 		LD   A,'.'	; Print out a "." for every byte loaded
; 		RST  08H	;
; 		DEC  D		; Decrement line character counter
; 		JR   LOAD2	; and keep loading into memory until line is complete
		
; LOAD3		CALL GET2	; Get two chars, build byte and checksum
; 		LD   A,E	; Check the checksum value
; 		AND  A		; Is it zero?
; 		RET  Z

; LOADERR		LD   HL,CKSUMERR  ; Get "Checksum Error" message
; 		CALL write_string	; Print Message from (HL) and terminate the load
; 		CALL	write_newline
; 		RET

; LOAD00  	LD   HL,LDETXT	; Print load complete message
; 		CALL write_string
; 		CALL	write_newline
; 		RET

;------------------------------------------------------------------------------
; Start BASIC command
;------------------------------------------------------------------------------
BASIC
    		LD HL,C_OR_W
		CALL write_string
		CALL	write_newline
		CALL u_get_char
		RET Z	; Cancel if CTRL-C
		AND  $5F ; uppercase
		CP 'C'
		JP  Z,BASCLD
		CP 'W'
		JP  Z,BASWRM
		RET

	
;------------------------------------------------------------------------------
; CP/M load command
;------------------------------------------------------------------------------
cpm_jump
    	LD		HL,CPMTXT
		CALL	write_string
		CALL	write_newline
		CALL	u_get_char
		RET		Z	; Cancel if CTRL-C
		AND		$5F ; uppercase
		CP 		'Y'
		JP		Z,CPMLOAD2
		RET
CPMTXT
		.BYTE	$0D,$0A
		.TEXT	"Boot CP/M?"
		.BYTE	$00

CPMTXT2
		.BYTE	$0D,$0A
		.TEXT	"Loading CP/M..."
		.BYTE	$0D,$0A,$00

CPMLOAD2
    		LD HL,CPMTXT2
		CALL write_string
		CALL	write_newline


		CALL	cfWait
		LD	A,$04
		OUT	(CF_COMMAND),A

		CALL	cfWait
		LD 	A,CF_8BIT	; Set IDE to be 8bit
		OUT	(CF_FEATURES),A
		LD	A,CF_SET_FEAT
		OUT	(CF_COMMAND),A


		CALL	cfWait
		LD 	A,CF_NOCACHE	; No write cache
		OUT	(CF_FEATURES),A
		LD	A,CF_SET_FEAT
		OUT	(CF_COMMAND),A

		LD	B,numSecs

		LD	A,0
		LD	(secNo),A
		LD	HL,loadAddr
		LD	(dmaAddr),HL
processSectors:

		CALL	cfWait

		LD	A,(secNo)
		OUT 	(CF_LBA0),A
		LD	A,0
		OUT 	(CF_LBA1),A
		OUT 	(CF_LBA2),A
		LD	a,0E0H
		OUT 	(CF_LBA3),A
		LD 	A,1
		OUT 	(CF_SECCOUNT),A

		call	sec_read

		LD	DE,0200H
		LD	HL,(dmaAddr)
		ADD	HL,DE
		LD	(dmaAddr),HL
		LD	A,(secNo)
		INC	A
		LD	(secNo),A

		djnz	processSectors

		;call LED_GREEN

; Start CP/M using entry at top of BIOS
; The current active console stream ID is pushed onto the stack
; to allow the CBIOS to pick it up
; 0 = SIO A, 1 = SIO B
		
		ld		A,0
		push	AF
		ld		HL,($FFFE)
		jp		(HL)


;------------------------------------------------------------------------------

; Read physical sector from host

sec_read:
		PUSH 	AF
		PUSH 	BC
		PUSH 	HL

		CALL 	cfWait

		LD 	A,CF_READ_SEC
		OUT 	(CF_COMMAND),A

		CALL 	cfWait

		LD 	c,4
		LD 	HL,(dmaAddr)
rd4secs:
		LD 	b,128
rdByte:
		nop
		nop
		in 	A,(CF_DATA)
		LD 	(HL),A
		iNC 	HL
		dec 	b
		JR 	NZ, rdByte
		dec 	c
		JR 	NZ,rd4secs

		POP 	HL
		POP 	BC
		POP 	AF

		RET


; Wait for disk to be ready (busy=0,ready=1)
cfWait:
		PUSH 	AF
cfWait1:
		in 	A,(CF_STATUS)
		AND 	080H
		cp 	080H
		JR	Z,cfWait1
		POP 	AF
		RET

	INCLUDE "led.asm"
	INCLUDE "random.asm"
	INCLUDE "sio.asm"
	INCLUDE "stdout.asm"
	INCLUDE "cf_card.asm"
	INCLUDE "utils.asm"

;------------------------------------------------------------------------------

BLUE		.BYTE	"BLUE"
		.BYTE	$0D,$0A,$00
WHITE		.BYTE	"WHITE"
		.BYTE	$0D,$0A,$00

C_OR_W
		.BYTE	$0D,$0A
		.TEXT	"Cold or Warm ?"
		.BYTE	$0D,$0A,$00

CKSUMERR	.BYTE	"Checksum error"
		.BYTE	$0D,$0A,$00

INITTXT  
		.BYTE	$0C
		.TEXT	"Press [SPACE] to activate console"
		.BYTE	$0D,$0A, $00

LDETXT  
		.TEXT	"Load complete."
		.BYTE	$0D,$0A, $00


;
;Monitor data structures:
;
monitor_message: 	defm	CR,LF,"  Z80 Retro Board Monitor",CR,LF
					defm	" by Uberfoo Heavy Industries",CR,LF,CR,LF,0
no_match_message:	defm	"? ",0
help_message:		defm	"Commands implemented:",CR,LF,0
dump_message:		defm	"Displays a 256-byte block of memory.",CR,LF,0
load_message:		defm	"Enter hex bytes starting at memory location.",CR,LF,0
run_message:		defm	"Will jump to (execute) program at address entered.",CR,LF,0
diskrd_message:		defm	"Reads one sector from disk to memory.",CR,LF,0
diskwr_message:		defm	"Writes one sector from memory to disk.",CR,LF,0
;Strings for matching:
dump_string:		defm	"dump",0
load_string:		defm	"load",0
jump_string:		defm	"jump",0
run_string:		defm	"run",0
question_string:	defm	"?",0
help_string:		defm	"help",0
diskrd_string:		defm	"diskrd",0
diskwr_string:		defm	"diskwr",0
cpm_string:		defm	"cpm",0
no_match_string:	defm	0,0
;Table for matching strings to jumps
parse_table:		defw	dump_string,dump_jump,load_string,load_jump
			defw	jump_string,run_jump,run_string,run_jump
			defw	question_string,help_jump,help_string,help_jump
			defw	diskrd_string,diskrd_jump,diskwr_string,diskwr_jump
			defw	cpm_string,cpm_jump
			defw	no_match_string,no_match_jump

;------------------------------------------------------------------------------

	INCLUDE "basic.asm"

FINIS		.END	


