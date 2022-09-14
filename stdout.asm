;
;Puts a single char (byte value) on serial output
;call with char to send in a register. Uses B register
write_char:
	push	af              ; store af
	push	bc              ; store bc
	out		(SIO_DA), a      ; send char to the SIO
	call	tx_emp          ; wait for outgoing char to be sent
txa_exit:
	pop		bc              ; retrieve bc
	pop		af              ; retrieve af
	ret                     ; return

; wait until outgoing serial has been sent
tx_emp:
	sub		a               ; set a to 0
	inc		a               ; set a to 1
	out		(SIO_CA), a      ; write to WR0, select RR1
	in		a, (SIO_CA)      ; read RR1 register
	bit		0, a             ; check if all chars have been sent
	jp		z, tx_emp        ; if not (bit 0 = 0) then retrieve
	ret                     ; else exit

;
;Subroutine to write a zero-terminated string to serial output
;Pass address of string in hl register
;No error checking
write_string:
	ld      a,(hl)          ; load character from memory cell pointed by hl
	or      a               ; is it $00 (end string)?
	ret     z               ; Yes, then return
	call    write_char      ; Transmit char to SIO Ch.A
	inc     hl              ; and select the next one
	jr		write_string
