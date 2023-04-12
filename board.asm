; I/O addresses for SIO
SIO_DA		equ	0x20    ; SIO A data
SIO_CA		equ	0x22    ; SIO A control
SIO_DB		equ	0x21    ; SIO B data
SIO_CB		equ	0x23    ; SIO B control

; I/O addresses for CTC
CTC_CH0		equ	0x10    ; CTC channel 0
CTC_CH1		equ	0x11    ; CTC channel 1

; Base I/O address for CF card
CFBASE		equ	0x30

; Base address of RAM when ROM is paged in
CBOOT_BASE  equ 0x2000  ; (8k)