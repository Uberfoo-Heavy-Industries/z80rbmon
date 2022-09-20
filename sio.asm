bufWrap				equ     (SERBUF_START + SER_BUFSIZE) & $FF

RTS_HIGH	.EQU	0E8H
RTS_LOW		.EQU	0EAH


;
;Subroutine to initialize serial port UART
;16x = 9600 baud
initialize_port:
	; CH0 provides SIO A RX/TX clock

	ld      a, 00000111b    ; int off, timer on, prescaler=16, don't care ext. TRG edge,
							; start timer on loading constant, time constant follows
							; sw-­rst active, this is a ctrl cmd
	out     (CTC_CH0), a
	ld      a, 40           ; Time constant 40
	out     (CTC_CH0), a    ; load into channel 0

	ld      a, 01000111b    ; int off, counter on, don't care prescaler, don't care ext. TRG edge,
							; don't care timer TRG, time constant follows
							; sw-­rst active, this is a ctrl cmd
	out     (CTC_CH1), a
	ld      a, 3            ; Time constant 3
	out     (CTC_CH1), a    ; load into channel 0

	ld      a,0xE8
	ld      (SERABITS),a  ; set serial ports status to ON

	;set up TX and RX:
	ld      a, 00110000b    ;write into WR0: error reset, select WR0
	out     (SIO_CA), a
	ld      a, 0x18         ;write into WR0: channel reset
	out     (SIO_CA), a
	ld      a, 0x04         ;write into WR0: select WR4
	out     (SIO_CA), a
	ld      a, 0x04          ;04h write into WR4: clkx1,1 stop bit, no parity
	out     (SIO_CA), a
	ld      a, 0x05         ;write into WR0: select WR5
	out     (SIO_CA), a
	ld      a, (SERABITS)   ;DTR active, TX 8bit, BREAK off, TX on, RTS inactive
	out     (SIO_CA), a

	ld      a, 0x01          ;write into WR0: select WR1
	out     (SIO_CB), a
	ld      a, 00000100b    ;no interrupt in CH B, special RX condition affects vect
	out     (SIO_CB), a

	ld      a, 0x01         ;write into WR0: select WR1
	out     (SIO_CA), a
	ld      a, 00011000b    ;interrupt on all RX characters, parity is not a spec RX condition
							;buffer overrun is a spec RX condition
	out     (SIO_CA), a

	call    sio_a_ei
	call	rts_on

	ret

;-------------------------------------------------------------------------------
; interrupt driven routine to get chars from Z80 SIO ch.A
rx_avail:   
	push    af              ; store a
	push    hl              ; and hl
	call    rts_off       ; disable RTS line
	in      a, (SIO_DA)     ; read char from RX buffer into a
	ld      (TMPKEYBFR), a  ; store it into the temp key buffer
	call    char_into_buffer     ; sub-routine to put the char into the input buffer
	jp      NC, lvrx     ; if buffer is full, then leave without doing anything else
	ld      a, (TMPKEYBFR)  ; retrieve char
lvrx:
	pop		hl              ; retrieve hl
	pop		af              ; and a
	ei                      ; re-enable interrupts
	reti                    ; and exit

;-------------------------------------------------------------------------------
; special SIO ch.a condition (i.e., buffer overrun)
; if buffer overruns then show an error, empty the RX buffer and send
; a break char
rx_spec_cond:  
	push    af              ; store af
	call    rts_off       ; disable RTS
	call    sio_a_di        ; disable RX on ch. A
	out     (SIO_CA), a     ; send command to SIO
empty_buffer:
	xor		a
	out		(SIO_CA), a     ; write to WR0, select RR0
	in		a, (SIO_CA)     ; read RR0 register
	and		0x01            ; check if input buffer if empty
	jp		z, buffer_empty  ; if yes (bit 0 = 0) then leave
	in		a, (SIO_DA)     ; read chars
	jr		empty_buffer     ; repeat
buffer_empty:
	pop		af              ; retrieve af
	ld		hl, 0x00        ; return point set to start of memory
	ex      (sp), hl        ; store onto stack
	ei                      ; re-enable interrupts
	reti                    ; return from interrupt and execute code at start of memory

;-------------------------------------------------------------------------------
;               Z80 SIO MANAGEMENT
;-------------------------------------------------------------------------------
; disable RTS:
; by resetting RTS bit (set to 0), the RTS line is disabled (HIGH)
rts_off: 
	push    bc              ; store bc
	ld      a, (SERABITS)   ; load data serial bits for ch.A
	srl     a               ; position data bits in bits #5&6
	and     01100000b       ; get only bits #5&6
	ld      b, a            ; store data bits
	ld      a, 0x05         ; write into WR0: select WR5
	out     (SIO_CA), a
	ld      a, 10001000b    ; enable DTR (b7) and TX (b4), disable RTS (b1)
	or      b               ; set data bits
	out     (SIO_CA), a     ; send setting
	pop     bc              ; retrieve bc
	ret                     ; exit

;-------------------------------------------------------------------------------
; enable RTS
; by setting RTS bit (set to 1), the RTS line is enabled (LOW)
rts_on:
	push    bc              ; store bc
	ld      a, (SERABITS)   ; load data serial bits for ch.A
	srl     a               ; position data bits in bits #5&6
	and     01100000b       ; get only bits #5&6
	ld      b, a            ; store data bits
	ld      a, 0x05         ; write into WR0: select WR5
	out     (SIO_CA), a
	ld      a, 10001010b    ; enable DTR (b7), TX (b4), and RTS (b1)
	or      b               ; set data bits
	out     (SIO_CA), a     ; send setting
	pop     bc              ; retrieve bc
	ret                     ; return

;-------------------------------------------------------------------------------
; disable SIO RX channel
sio_a_di:
	ld      a, 0x03          ; write into WR0: select WR3
	out     (SIO_CA), a
	ld      a, 0xC0          ; RX 8bit, auto enable off, RX off
	out     (SIO_CA), a
	ret                     ; return

;-------------------------------------------------------------------------------
; enable SIO RX channel
sio_a_ei:
	ld      a, 0x03          ; write into WR0: select WR3
	out     (SIO_CA), a           ; select register
	ld      a, 0xC1          ; RX 8bit, auto enable off, RX on
	out     (SIO_CA), a           ; send settings to SIO
	ret

;------------------------------------------------------------------------------
; put a char into the input buffer, char is into a
; this sub is called both from the ISR "rx_avail" and when
; the RETURN key is pressed on the keyboard
char_into_buffer:
	push    af              ; store it
	ld      a,(serBufUsed)  ; load buffer size
	cp      SER_BUFSIZE     ; if buffer is not full
	jp      c,notfull       ; then store the char
	pop     af              ; else drop it
	ret                     ; and exit
notfull:
	ld      hl,(serInPtr)   ; buffer is not full, can store the char
	inc     hl              ; load pointer to find first free cell
	ld      a,l             ; only check low byte because buffer<256
	cp      bufWrap         ; check if the pointer is at the last cell
	jr      NZ,notwrap      ; if not then continue
	ld      hl,serBuf       ; else load the address of the first cell
notwrap:
	ld      (serInPtr),hl   ; store the new pointer
	pop     af              ; then retrieve the char...
	ld      (hl),a          ; ...and store it in the appropriate cell
	ld      hl,serBufUsed   ; size of the input buffer
	inc     (hl)            ; increment it
	ld      a,SER_FULLSIZE  ; input buffer capacity
	cp      (hl)            ; check if input buffer is full
	ret     c               ; exit if buffer is not full
	call    rts_off     ; ...receiving further chars must be stopped
	scf                     ; set Carry flag, because  we must inform that the char has been added before to disable RTS
	ret

;-------------------------------------------------------------------------------
; retrieve a char from the input buffer
rx:
	ld      a, (serBufUsed) ; load the buffer size
	and     a               ; check if it's 0 (empty)
	jp      z, rx          ; if it's empty, wait for a char
	di                      ; disable interrupts
	push    hl              ; store hl
	ld      hl, (serRdPtr)  ; load pointer to first available char
	inc     hl              ; increment it (go to the next char)
	ld      a, l            ; check if the end of the buffer has been reached
	cp      bufWrap         ; (only check low byte because buffer<256)
	jr      NZ, notrdwrap   ; if not, jump straight
	ld      hl, serBuf      ; else reload the starting address of the buffer
notrdwrap:
	ld      (serRdPtr), hl  ; store new pointer to the next char to read
	ld      a, (serBufUsed) ; load buffer size
	dec     a               ; decrement it
	ld      (serBufUsed), a ; and store the new size
	cp      SER_EMPTYSIZE   ; check if input buffer can be considered empty
	jr      NC, rx_exit    ; if not empty yet, then exit
	call    rts_on        ; set RTS on
rx_exit:
	ld      a, (hl)         ; recover the char and return it into a
	pop     hl              ; retrieve hl
	ei                      ; re-enable interrupts
	ret                     ; return
