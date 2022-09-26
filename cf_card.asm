;The addresses that the CF Card resides in I/O space.
;Change to suit hardware.
CFDATA				equ		CFBASE + $00		; Data (R/W)
CFERR				equ		CFBASE + $01		; Error register (R)
CFFEAT				equ		CFBASE + $01		; Features (W)
CFSECCO				equ		CFBASE + $02		; Sector count (R/W)
CFLBA0				equ		CFBASE + $03		; LBA bits 0-7 (R/W, LBA mode)
CFLBA1				equ		CFBASE + $04		; LBA bits 8-15 (R/W, LBA mode)
CFLBA2				equ		CFBASE + $05		; LBA bits 16-23 (R/W, LBA mode)
CFLBA3				equ		CFBASE + $06		; LBA bits 24-27 (R/W, LBA mode)
CFSTAT				equ		CFBASE + $07		; Status (R)
CFCMD				equ		CFBASE + $07		; Command (W)	

;***************************************************************************
;CF_INIT
;Function: Initialize CF to 8 bit data transfer mode
;***************************************************************************	
CF_INIT:
	call	cfWait
    ;call    CF_CHK_ERR
	ld		a, 0x04						;ld features register to enable 8 bit
	out		(CFCMD), a
    ;call    CF_CHK_ERR
	call	cfWait
	ld		a, 0x01						;ld features register to enable 8 bit
	out		(CFFEAT), a
    ;call    CF_CHK_ERR
	call	cfWait
	ld		a, 0xEF						;Send set features command
	out		(CFCMD), a
    ;call    CF_CHK_ERR
	call	cfWait
	ret

;***************************************************************************
;cfWait
;Function: Loops until status register bit 7 (busy) is 0
;***************************************************************************	
cfWait:
		push 	af
cfWait1:
		in 		a,(CF_STATUS)
		and 	0x80
		cp 		0x80
		jr		z,cfWait1
		pop 	af
		ret

; CF_CHK_ERR:
;     push	hl
;     ld      hl, CF_MSG_8
; 	call	write_string
;     call	byte_to_hex_string
; 	call	write_string
; 	call    write_newline
; 	and		00000001b					;mask off error bit
; 	jr		z, CE_END			        ;jump if no error
;     ld      hl, CF_MSG_9
; 	call	write_string
;     call	byte_to_hex_string
; 	call	write_string
; 	call    write_newline
; 	in      a, (CFERR)
;     call	byte_to_hex_string
; 	call	write_string
; 	call    write_newline
; CE_END:
;     pop     hl
;     ret

;
;Subroutine to read one disk sector (512 bytes)
;Address to place data passed in hl
;LBA bits 0 to 7 passed in c, bits 8 to 15 passed in B
;LBA bits 16 to 23 passed in E
disk_read:
rd_status_loop_1:
	in		a, (CFSTAT)		;check status
	and		0x80		;check BSY bit
	jp		nz, rd_status_loop_1	;loop until not busy
rd_status_loop_2:
	in		a, (CFSTAT)		;check	status
	and		0x40		;check DRDY bit
	jp		z, rd_status_loop_2	;loop until ready
	ld		a, 0x01		;number of sectors = 1
	out		(CFSECCO), a		;sector count register
	ld		a, c
	out		(CFLBA0), a		;lba bits 0 - 7
	ld		a, b
	out		(CFLBA1), a		;lba bits 8 - 15
	ld		a, e
	out		(CFLBA2), a		;lba bits 16 - 23
	ld		a, 11100000b	;LBA mode, select drive 0
	out		(CFLBA3), a		;drive/head register
	ld		a, 0x20		;Read sector command
	out		(CFCMD),a
rd_wait_for_DRQ_set:
	in		a, (CFSTAT)		;read status
	and		0x08		;DRQ bit
	jp		z, rd_wait_for_DRQ_set	;loop until bit set
rd_wait_for_BSY_clear:
	in		a, (CFSTAT)
	and		0x80
	jp		nz, rd_wait_for_BSY_clear
	in		a, (CFSTAT)		;clear INTRQ
read_loop:
	in		a, (CFDATA)		;get data
	ld		(hl), a
	inc		hl
	in		a, (CFSTAT)		;check status
	and		0x08		;DRQ bit
	jp		nz, read_loop	;loop until cleared
	ret
;
;Subroutine to write one disk sector (256 bytes)
;Address of data to write to disk passed in hl
;LBA bits 0 to 7 passed in c, bits 8 to 15 passed in B
;LBA bits 16 to 23 passed in E
disk_write:
wr_status_loop_1:
	in		a, (CFSTAT)		;check status
	and		0x80		;check BSY bit
	jp		nz, wr_status_loop_1	;loop until not busy
wr_status_loop_2:
	in		a, (CFSTAT)		;check	status
	and		0x40		;check DRDY bit
	jp		z, wr_status_loop_2	;loop until ready
	ld		a, 0x01		;number of sectors = 1
	out		(CFSECCO), a		;sector count register
	ld		a, c
	out		(CFLBA0), a		;lba bits 0 - 7
	ld		a, b
	out		(CFLBA1), a		;lba bits 8 - 15
	ld		a, e
	out		(CFLBA2), a		;lba bits 16 - 23
	ld		a, 11100000b	;LBA mode, select drive 0
	out		(CFLBA3), a		;drive/head register
	ld		a, 0x30		;Write sector command
	out		(CFCMD), a
wr_wait_for_DRQ_set:
	in		a, (CFSTAT)		;read status
	and		08h		;DRQ bit
	jp		z,wr_wait_for_DRQ_set	;loop until bit set			
write_loop:
	ld		a, (hl)
	out		(CFDATA), a		;write data
	inc		hl
	in		a, (CFSTAT)		;read status
	and		0x08		;check DRQ bit
	jp		nz,write_loop	;write until bit cleared
wr_wait_for_BSY_clear:
	in		a, (CFSTAT)
	and		0x80
	jp		nz, wr_wait_for_BSY_clear
	in		a, (CFSTAT)		;clear INTRQ
	ret