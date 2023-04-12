
num_colors	equ	8

all_colors:
color_green	defb	0xE8,0x00,0xFF,0x00
color_red	defb	0xE8,0x00,0x00,0xFF
color_blue	defb	0xE8,0xFF,0x00,0x00
color_purp	defb	0xE8,0xFF,0x00,0xFF
color_orng  defb    0xE8,0xFF,0xA5,0x00
color_yell  defb    0xE8,0xFF,0xFF,0x00
color_cyan  defb    0xE8,0x00,0xFF,0xFF
color_white defb 	0xE8,0xFE,0xFE,0xFE
color_blank defb 	0xE0,0x00,0x00,0x00
tmp_buff	equ		0xdb08
color_buff  equ		0xdb0f
;***************************************************************************
; LED Functions
;***************************************************************************

SEND_COLOR:
	push	b
	push	a
	ld		b, 4		; Loop 4 times
send_color_loop:
	ld		a, (hl)
    call	SEND_BYTE
	inc		hl
	djnz	send_color_loop
	pop 	a
	pop		b
	ret

LED_GREEN:
	call 	START_FRAME
	ld		hl, color_green
	call 	SEND_COLOR
	call 	START_FRAME
	ret

LED_RED:
	call 	START_FRAME
	ld		hl, color_red
	call 	SEND_COLOR
	call 	START_FRAME
	ret

LED_BLUE:
	call 	START_FRAME
	ld		hl, color_blue
	call 	SEND_COLOR
	call 	START_FRAME
	ret

ONE_COLOR:
	push	hl
	ld		b, a
	call	START_FRAME
	cp		0
	jr		z, oc_color
oc_loop:
	ld		hl, color_blank
	call 	SEND_COLOR
	dec		a
	jr		nz, oc_loop
oc_color:
	pop		hl
	call 	SEND_COLOR
	ld		a, 3
	sub		b
	jr		z, oc_end
oc_loop2:
	ld		hl, color_blank
	call 	SEND_COLOR
	dec		a
	jr		nz, oc_loop2
oc_end:
	call	START_FRAME
	ret

LED_CLEAR_ALL:
	call	START_FRAME
	ld		hl, color_blank
	call	SEND_COLOR
	ld		hl, color_blank
	call	SEND_COLOR
	ld		hl, color_blank
	call	SEND_COLOR
	ld		hl, color_blank
	call	SEND_COLOR
	call	START_FRAME

SEND_BYTE:
	push	bc
	push	a
	ld		b, 8
SEND_BYTE1:
	rlca
	call	SEND_BIT
	djnz	SEND_BYTE1
	pop		a
	pop		bc
	ret

SEND_BIT:
	push	a
	ld		a, 0x05
	out		(SIO_CB), a
	pop		a
	push	a
	bit		0, a
	jr		z, SEND_0
	ld		a, 11101000b
	jr		BIT_OUT
SEND_0:
	ld		a, 11101010b
BIT_OUT:
	push	a
	out		(SIO_CB), a
	ld		a, 0x05
	out		(SIO_CB), a
	pop		a
	and		01111111b
	out		(SIO_CB), a
	pop		a
	ret

START_FRAME:
	push	b
	push	a
    ld		b, 4
START_FRAME1:
    ld		a, 0x00
    call	SEND_BYTE
    djnz	START_FRAME1
	pop		a
	pop		b
    ret

RND_FLASH_CYCLE:
	call	RAND			; Generate random number
	ld		de, (rndSeed1)	; Put random number in de
	ld		e, num_colors			
	call	DIV_D_E			; Divide by number of colors
	ld		c,a				; Put remainder in c
	xor		b				; 0 out high byte in b
	sla		c				; Multiply remainder by 4				
	sla		c
	push	bc				; Save color offset on the stack
	call	RAND			; Generate random number
	ld		de, (rndSeed1)	; Put random number in de
	ld		e, 4
	call	DIV_D_E			; Divide by 4
	pop		bc				; Retrieve color offset from the stack
	ld		hl, all_colors	; Load base address of colors
	add		hl, bc			; Add color offsetto hl
	
	; a now contains the led number and hl the color address
	call	ONE_COLOR		

	call 	ckinchar		; Check for any input
	jr 		nz, rfc_end			; stop if we find any

	ld		bc, 0x3000
	call	delay_short

	call	LED_CLEAR_ALL

	ld		bc, 0xFFFF
	call	delay_short

	jr		RND_FLASH_CYCLE
rfc_end:
	ret

