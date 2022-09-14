
num_colors	equ	6

color_green	byte	0xFF,0x00,0xFF,0x00
color_red	byte	0xFF,0x00,0x00,0xFF
color_blue	byte	0xFF,0xFF,0x00,0x00
color_purp	byte	0xFF,0xFF,0x00,0xFF
color_white byte 	0xEF,0xFE,0xFE,0xFE
color_blank byte 	0xE0,0x00,0x00,0x00

all_colors  dw		color_green, color_red, color_blue, color_purp, color_white

;***************************************************************************
; LED Functions
;***************************************************************************

SEND_COLOR:
	push	bc
	push	a
	ld		b, 4		; Loop 4 times
send_color_loop:
	ld		a, (hl)
    call	SEND_BYTE
	inc		hl
	djnz	send_color_loop
	pop 	a
	pop		bc
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

ONE_WHITE:
	push	bc
	ld		b, a
	call	START_FRAME
	cp		0
	jr		z, ow_white
ow_loop:
	ld		hl, color_blank
	call 	SEND_COLOR
	dec		a
	jr		nz, ow_loop
ow_white:
	ld		hl, color_white
	call 	SEND_COLOR
	ld		a, 3
	sub		b
	jr		z, ow_end
ow_loop2:
	ld		hl, color_blank
	call 	SEND_COLOR
	dec		a
	jr		nz, ow_loop2
ow_end:
	call	START_FRAME
	pop		bc
	ret

ONE_COLOR:
	push	bc
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
	pop		bc
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
	ld		b, 0x08
SEND_BYTE1:
	call	SEND_BIT
	rra
	djnz	SEND_BYTE1
	pop		bc
	ret

SEND_BIT:
	push	af
	ld		a, 0x05
	out		(SIO_CB), a
	pop		af
	push	af
	and		00000001b
	jr		z, SEND_0
	ld		a, 11101000b
	jr		BIT_OUT
SEND_0:
	ld		a, 11101010b
BIT_OUT:
	push	af
	out		(SIO_CB), a
	ld		a, 0x05
	out		(SIO_CB), a
	pop		af
	and		01111111b
	out		(SIO_CB), a
	pop		af
	ret

START_FRAME:
	push	bc
	push	a
    ld		b, 4
START_FRAME1:
    ld		a, 0x00
    call	SEND_BYTE
    djnz	START_FRAME1
	pop		a
	pop		bc
    ret

RND_FLASH_CYCLE:
		call	RAND
		ld		de, (rndSeed1)
		ld		e, 5
		call	DIV_D_E
		add		a
		add		a
		call	RAND
		ld		de, (rndSeed1)
		ld		e, 4
		call	DIV_D_E		
		call	ONE_WHITE

		CALL 	ckinchar
		jr 		nz, end

		push	bc
		ld		bc, 0x0FFF
		call	delay_short

		call	LED_CLEAR_ALL

		ld		bc, 0xFFFF
		call	delay_short
		pop		bc

		djnz	RND_FLASH_CYCLE
end:
		ret
