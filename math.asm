SEED1		equ	0xA280
SEED2		equ	0xC0DE

KEY_WORD        equ     0x2501

; ----------------------------------------------------
; Intializes the PRNG
; ----------------------------------------------------
; The seeds are copied into thier RAM locations
; Registers:
;   hl
; ----------------------------------------------------
INIT_RND:
        ld  hl,SEED1
        ld  (rndSeed1),hl
        ld  hl,SEED2
        ld  (rndSeed2),hl
        ret

; ----------------------------------------------------
; PRNG function
; ----------------------------------------------------
; Generates a psuedo random number. The number is 
; store in rndSeed1.
; Registers:
;   a, de, hl
; ----------------------------------------------------
RAND:   
        ld  hl,(rndSeed1)   ; yw -> zt
        ld  de,(rndSeed2)   ; xz -> yw
        ld  (rndSeed2),hl  ; x = y, z = w
        ld  a,l         ; w = w ^ ( w << 3 )
        add a,a
        add a,a
        add a,a
        xor l
        ld  l,a
        ld  a,d         ; t = x ^ (x << 1)
        add a,a
        xor d
        ld  h,a
        rra             ; t = t ^ (t >> 1) ^ w
        xor h
        xor l
        ld  h,e         ; y = z
        ld  l,a         ; w = t
        ld  (rndSeed1),hl
        ret

; ----------------------------------------------------
; Divide d by e
; ----------------------------------------------------
; divides d by e and places the quotient in d and the 
; remainder in a
; Input:
;   d = dividend
;   e = devisor
; Output:
;   d = quotient
;   a = remainder
; Registers:
;   a, b, d, e
; ----------------------------------------------------
DIV_D_E:
        xor	a       ; 0 out a
        ld	b,8

div_loop:
        sla	d
        rla
        cp	e
        jr	c, $+4
        sub	e
        inc	d  
        djnz	div_loop

        ret

; ----------------------------------------------------
; XOR encrypts or decrypts a string using a 16-bit key
; ----------------------------------------------------
; Result is a null-terminated string in the decrytBuf
; Input:
;   de = Address of the string to encrypt/decrypt
; Registers:
;   a, bc, de, hl
; ----------------------------------------------------

ENCRYPT:
        ld bc,KEY_WORD     ; Load the 16bit key into bc
        ld hl,decryptBuf   ; Copy decrpyt buffer address to hl

enc_loop:
        ld a,(de)       ; Load the next character of the string into a
        or a            ; Test if a is zero (end of string)
        jr z,enc_end    ; Jump to end if end of string reached
        xor b           ; XOR the high byte of the key with a
        ld (hl),a       ; Store the encrypted/decrypted character in buffer
        inc hl          ; Increment hl to point to the next byte of the buffer
        inc de          ; Increment de to point to the next ciphertext byte
        ld a,(de)       ; Load the next character of the string into a
        or a            ; Test if a is zero (end of string)
        jr z,enc_end    ; Jump to end if end of string reached
        xor c           ; XOR the low byte of the key with a
        ld (hl),a       ; Store the encrypted/decrypted character in buffer
        inc hl          ; Increment hl to point to the next byte of the buffer
        inc de          ; Increment de to point to the next ciphertext byte
        jr enc_loop     ; Loop

enc_end:
        ld (hl),0x00    ; Terminate string with NULL character
        ret
