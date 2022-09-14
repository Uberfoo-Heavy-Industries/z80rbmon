SEED1		.EQU	$A280
SEED2		.EQU	$C0DE

INIT_RND:
    ld  hl, SEED1
    ld  (rndSeed1), hl
    ld  hl, SEED2
    ld  (rndSeed2), hl
    ret

RAND:   ld  hl,(rndSeed1)   ; yw -> zt
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

DIV_D_E:
   xor	a
   ld	b, 8

div_loop:
   sla	d
   rla
   cp	e
   jr	c, $+4
   sub	e
   inc	d
   
   djnz	div_loop
   
   ret
