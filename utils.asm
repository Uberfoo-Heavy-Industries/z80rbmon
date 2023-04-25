;The subroutines for the serial port use these variables in RAM:
current_location	equ		0xdb00		;word variable in RAM
line_count			equ		0xdb02		;byte variable in RAM
byte_count			equ		0xdb03		;byte variable in RAM
value_pointer		equ		0xdb04		;word variable in RAM
current_value		equ		0xdb06		;word variable in RAM
buffer				equ		0xdb08		;buffer in RAM -- up to stack area

;
;Subroutine to get a string from serial input, place in buffer.
;Buffer address passed in hl reg.
;Uses a,bc,DE,hl registers (including calls to other subroutines).
;Line entry ends by hitting return key. Return char not included in string (replaced by zero).
;Backspace editing OK. No error checking.
;
get_line:
	push	hl
	ld		c, 0x00	;line position
	ld		a, h	;put original buffer address in de
	ld		d, a	;after this don't need to preserve hl
	ld		a, l	;subroutines called don't use de
	ld		e, a
get_line_next_char:			
	call 	rx
	cp		00dh		;check if return
	jp		z, get_line_end		;yes, normal exit
	cp		07fh		;check if backspace (VT102 keys)
	jp		z, get_line_backspace	;yes, jump to backspace routine
	cp		008h		;check if backspace (ANSI keys)
	jp		z, get_line_backspace	;yes, jump to backspace
	cp		CTRLC		;check if CTRL-C
	jp		z, get_line_ctrlc
	call	write_char	;put char on screen
	ld		(de), a		;store char in buffer
	inc		de			;point to next space in buffer
	inc		c			;inc counter
	ld		a, 0x00
	ld		(de),a		;leaves a zero-terminated string in buffer
	jp		get_line_next_char
get_line_backspace:
	ld		a, c		;check current position in line
	cp		0x00		;at beginning of line?
	jp		z, get_line_next_char	;yes, ignore backspace, get next char
	dec		de			;no, erase char from buffer
	dec		c			;back up one
	ld		a, 0x00		;put a zero in buffer where the last char was
	ld		(de), a
	ld		hl, erase_char_string	;ANSI sequence to delete one char from line
	call	write_string		;transmits sequence to backspace and erase char
	jp		get_line_next_char
get_line_ctrlc:
	pop		de		;put saved hl into de
	ld		a,CTRLC
	ld		(de),a	 ;put a CTRL-C at the beginning of the string
	ret
get_line_end:
	pop		hl
	ret

;
;Creates a two-char hex string from the byte value passed in register a
;Location to place string passed in hl
;String is zero-terminated, stored in 3 locations starting at hl
;Also uses registers b,d, and e
byte_to_hex_string:
			ld	b,a			;store original byte
			srl	a			;shift right 4 times, putting
			srl	a			;high nybble in low-nybble spot
			srl	a			;and zeros in high-nybble spot
			srl	a
			ld	d,000h			;prepare for 16-bit addition
			ld	e,a			;de contains offset
			push	hl			;temporarily store string target address
			ld	hl,hex_char_table	;use char table to get high-nybble character
			add	hl,de			;add offset to start of table
			ld	a,(hl)			;get char
			pop	hl			;get string target address
			ld	(hl),a			;store first char of string
			inc	hl			;point to next string target address
			ld	a,b			;get original byte back from reg b
			and	00fh			;mask off high-nybble
			ld	e,a			;d still has 000h, now de has offset
			push	hl			;temp store string target address
			ld	hl,hex_char_table	;start of table
			add	hl,de			;add offset
			ld	a,(hl)			;get char
			pop	hl			;get string target address
			ld	(hl),a			;store second char of string
			inc	hl			;point to third location
			ld	a,000h			;zero to terminate string
			ld	(hl),a			;store the zero
			ret				;done
;
;Converts a single ASCII hex char to a nybble value
;Pass char in reg a. Letter numerals must be upper case.
;Return nybble value in low-order reg a with zeros in high-order nybble if no error.
;Return 0ffh in reg a if error (char not a valid hex numeral).
;Also uses b, c, and hl registers.
hex_char_to_nybble:	ld	hl,hex_char_table
			ld	b,00fh			;no. of valid characters in table - 1.
			ld	c,000h			;will be nybble value
hex_to_nybble_loop:	cp	(hl)			;character match here?
			jp	z,hex_to_nybble_ok	;match found, exit
			dec	b			;no match, check if at end of table
			jp	m,hex_to_nybble_err	;table limit exceded, exit with error
			inc	c			;still inside table, continue search
			inc	hl
			jp	hex_to_nybble_loop
hex_to_nybble_ok:	ld	a,c			;put nybble value in a
			ret
hex_to_nybble_err:	ld	a,0ffh			;error value
			ret
;
;Converts a hex character pair to a byte value
;Called with location of high-order char in hl
;If no error carry flag clear, returns with byte value in register a, and
;hl pointing to next mem location after char pair.
;If error (non-hex char) carry flag set, hl pointing to invalid char
hex_to_byte:		ld	a,(hl)			;location of character pair
			push	hl			;store hl (hex_char_to_nybble uses it)
			call	hex_char_to_nybble
			pop	hl			;returns with nybble value in a reg, or 0ffh if error
			cp	0ffh			;non-hex character?
			jp	z,hex_to_byte_err	;yes, exit with error
			sla	a			;no, move low order nybble to high side
			sla	a
			sla	a
			sla	a
			ld	d,a			;store high-nybble
			inc	hl			;get next character of the pair
			ld	a,(hl)
			push	hl			;store hl
			call	hex_char_to_nybble
			pop	hl
			cp	0ffh			;non-hex character?
			jp	z,hex_to_byte_err	;yes, exit with error
			or	d			;no, combine with high-nybble
			inc	hl			;point to next memory location after char pair
			scf
			ccf				;no-error exit (carry = 0)
			ret
hex_to_byte_err:	scf				;error, carry flag set
			ret
hex_char_table:		defm	"0123456789ABCDEF"	;ASCII hex table
;
;Subroutine to get a two-byte address from serial input.
;Returns with address value in hl
;Uses locations in RAM for buffer and variables
address_entry:		ld	hl,buffer		;location for entered string
			call	get_line		;returns with address string in buffer
			ld 	a,(buffer)
			cp	CTRLC
			ret	z
			ld	hl,buffer		;location of stored address entry string
			call	hex_to_byte		;will get high-order byte first
			jp	c, address_entry_error	;if error, jump
			ld	(current_location+1),a	;store high-order byte, little-endian
			ld	hl,buffer+2		;point to low-order hex char pair
			call	hex_to_byte		;get low-order byte
			jp	c, address_entry_error	;jump if error
			ld	(current_location),a	;store low-order byte in lower memory
			ld	hl,(current_location)	;put memory address in hl
			ret
address_entry_error:	ld	hl,address_error_msg
			call	write_string
			jp	address_entry
;
;Subroutine to get a decimal string, return a word value
;Calls decimal_string_to_word subroutine
decimal_entry:		ld	hl,buffer
			call	get_line		;returns with DE pointing to terminating zero
			ld 	a,(buffer)
			cp	CTRLC
			ret	z
			ld	hl,buffer
			call	decimal_string_to_word
			ret	nc			;no error, return with word in hl
			ld	hl,decimal_error_msg	;error, try again
			call	write_string
			jp	decimal_entry
;
;Subroutine to convert a decimal string to a word value
;call with address of string in hl, pointer to end of string in DE
;Carry flag set if error (non-decimal char)
;Carry flag clear, word value in hl if no error.
decimal_string_to_word:	ld	b,d
			ld	c,e			;use bc as string pointer
			ld	(current_location),hl	;store addr. of start of buffer in RAM word variable
			ld	hl,000h			;starting value zero
			ld	(current_value),hl
			ld	hl,decimal_place_value	;pointer to values
			ld	(value_pointer),hl
decimal_next_char:	dec	bc			;next char in string (moving right to left)
			ld	hl,(current_location)	;check if at end of decimal string
			scf				;get ready to subtract de from buffer addr.
			ccf				;set carry to zero (clear)
			sbc	hl,bc			;keep going if bc > or = hl (buffer address)
			jp	c,decimal_continue	;borrow means bc > hl
			jp	z,decimal_continue	;z means bc = hl
			ld	hl,(current_value)	;return if de < buffer address (no borrow)
			scf				;get value back from RAM variable
			ccf
			ret				;return with carry clear, value in hl
decimal_continue:	ld	a,(bc)			;next char in string (right to left)
			sub	030h			;ASCII value of zero char
			jp	m,decimal_error		;error if char value less than 030h
			cp	00ah			;error if byte value > or = 10 decimal
			jp	p,decimal_error		;a reg now has value of decimal numeral
			ld	hl,(value_pointer)	;get value to add an put in de
			ld	e,(hl)			;little-endian (low byte in low memory)
			inc	hl
			ld	d,(hl)
			inc	hl			;hl now points to next value
			ld	(value_pointer),hl
			ld	hl,(current_value)	;get back current value
decimal_add:		dec	a			;add loop to increase total value
			jp	m,decimal_add_done	;end of multiplication
			add	hl,de
			jp	decimal_add
decimal_add_done:	ld	(current_value),hl
			jp	decimal_next_char
decimal_error:		scf
			ret
			jp	decimal_add
decimal_place_value:	defw	1,10,100,1000,10000
;
;Memory dump
;Displays a 256-byte block of memory in 16-byte rows.
;Called with address of start of block in hl
memory_dump:		ld	(current_location),hl	;store address of block to be displayed
			ld	a,000h
			ld	(byte_count),a		;initialize byte count
			ld	(line_count),a		;initialize line count
			jp	dump_new_line
dump_next_byte:		ld	hl,(current_location)	;get byte address from storage,
			ld	a,(hl)			;get byte to be converted to string
			inc	hl			;increment address and
			ld	(current_location),hl	;store back
			ld	hl,buffer		;location to store string
			call	byte_to_hex_string	;convert
			ld	hl,buffer		;display string
			call	write_string
			ld	a,(byte_count)		;next byte
			inc	a
			jp	z,dump_done		;stop when 256 bytes displayed
			ld	(byte_count),a		;not finished yet, store
			ld	a,(line_count)		;end of line (16 characters)?
			cp	00fh			;yes, start new line
			jp	z,dump_new_line
			inc	a			;no, increment line count
			ld	(line_count),a
			ld	a,020h			;print space
			call	write_char
			jp	dump_next_byte		;continue
dump_new_line:		ld	a,000h			;reset line count to zero
			ld	(line_count),a			
			call	write_newline
			ld	hl,(current_location)	;location of start of line
			ld	a,h			;high byte of address
			ld	hl, buffer
			call	byte_to_hex_string	;convert
			ld	hl,buffer
			call	write_string		;write high byte
			ld	hl,(current_location)
			ld	a,l			;low byte of address
			ld	hl, buffer
			call	byte_to_hex_string	;convert
			ld	hl,buffer
			call	write_string		;write low byte
			ld	a,020h			;space
			call	write_char
			jp	dump_next_byte		;now write 16 bytes
dump_done:		ld	a,000h
			ld	hl,buffer
			ld	(hl),a			;clear buffer of last string
			call	write_newline
			ret
;
;Memory load
;Loads RAM memory with bytes entered as hex characters
;Called with address to start loading in hl
;Displays entered data in 16-byte rows.
memory_load:		ld	(current_location),hl
			ld	hl,data_entry_msg
			call	write_string
			jp	load_new_line
load_next_char:		call	u_get_char
			cp	00dh			;return?
			jp	z,load_done		;yes, quit
			ld	(buffer),a
			call	u_get_char
			cp	00dh			;return?
			jp	z,load_done		;yes, quit
			ld	(buffer+1),a
			ld	hl,buffer
			call	hex_to_byte
			jp	c,load_data_entry_error	;non-hex character
			ld	hl,(current_location)	;get byte address from storage,
			ld	(hl),a			;store byte
			inc	hl			;increment address and
			ld	(current_location),hl	;store back
			ld	a,(buffer)
			call	write_char
			ld	a,(buffer+1)
			call	write_char
			ld	a,(line_count)		;end of line (16 characters)?
			cp	00fh			;yes, start new line
			jp	z,load_new_line
			inc	a			;no, increment line count
			ld	(line_count),a
			ld	a,020h			;print space
			call	write_char
			jp	load_next_char		;continue
load_new_line:		ld	a,000h			;reset line count to zero
			ld	(line_count),a
			call	write_newline
			jp	load_next_char		;continue
load_data_entry_error:	call	write_newline
			ld	hl,data_error_msg
			call	write_string
			ret
load_done:		call	write_newline
			ret
;
; Check for input character in buffer
ckinchar:

		ld	a,(serBufUsed)
		cp	$0
		ret

;
;Get one ASCII character from the serial port.
;Returns with char in a reg. No error checking.
u_get_char:
	in 		a, (3)			;get status
	and 	0x02			;check RxRDY bit
	jp 		z, u_get_char		;not ready, loop
	in 		a, (2)			;get char
	ret
;
;Subroutine to start a new line
write_newline:
	ld		a, 00dh			;ASCII carriage return character
	call	write_char
	ld		a, 00ah			;new line (line feed) character
	call	write_char
	ret

;
;Strings used in subroutines
length_entry_string:	defm	"Enter length of file to load (decimal): ",0
dump_entry_string:	defm	"Enter no. of bytes to dump (decimal): ",0
LBA_entry_string:	defm	"Enter LBA (decimal, 0 to 65535): ",0
erase_char_string:	defm	008h,01bh,"[K",000h	;ANSI sequence for backspace, erase to end of line.
address_entry_msg:	defm	"Enter 4-digit hex address (use upper-case A through F): ",0
address_error_msg:	defm	CR,LF,"Error: invalid hex character, try again: ",0
data_entry_msg:		defm	"Enter hex bytes, hit return when finished.",CR,LF,0
data_error_msg:		defm	"Error: invalid hex byte.",CR,LF,0
decimal_error_msg:	defm	CR,LF,"Error: invalid decimal number, try again: ",0
