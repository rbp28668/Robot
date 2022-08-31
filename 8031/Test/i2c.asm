;======================================================================
; I2C test program
; Reads CMPS03 compass module and sends byte bearing back.
;======================================================================

;======================================================================
; Define interrupt vectors 
;======================================================================
            .equ rambase,4000h      ;start of dual mapped ram
            .equ intIE0,rambase+32
            .equ intTF0,rambase+64
            .equ intIE1,rambase+96
            .equ intTF1,rambase+128
            .equ intCom,rambase+160
            .equ intTF2,rambase+192
            .equ user,rambase+256

;======================================================================
; Monitor jump table
; Include in code that uses the robot monitor
; Gives isolation between monitor code & other routines
;======================================================================
;        .equ monjtab,0800h - 100h       ; allow 256 bytes at top of bottom 2k of EPROM
;        .equ stop,monjtab + 0           ; stop execution
;        .equ scominit,monjtab + 3       ; intitialise serial port
;        .equ scomtxa,monjtab + 6        ; transmit ascii char in A
;        .equ scomtxc,monjtab + 9        ; transmit string from code mem ptr to by dptr
;        .equ scomtxx,monjtab + 12       ; transmit string from ext mem ptr to by dptr
;        .equ scomtx8,monjtab + 15       ; transmit byte in A as 2 hex digits
;        .equ scomrxa,monjtab + 18       ; receive an ascii char in A
;        .equ scomrhx8,monjtab + 21      ; receive a hex encoded byte into A
;        .equ scomrh16,monjtab + 24      ; receive a hex encoded word into dptr
;        .equ ihexin,monjtab + 27        ; read intelhex file into data memory

;======================================================================
; Bit addresses of i2c lines
;======================================================================
		.equ sda,P3.4					; bit address of P3.4 - T0 - SDA
		.equ scl,P3.5					; bit address of p3.5 - T1 - SCL

;======================================================================
; I2C addresses
;======================================================================
		.equ compas,0C0h
		


;======================================================================
;======================================================================
            .org user

			lcall scominit
			mov a,65
			lcall scomtxa

			mov dptr,#startmsg
			lcall scomtxc

loop:		lcall i2cstr	; start
			mov a,#compas
			lcall i2ctx
			mov a,#1		; register 1, bearing as byte
			lcall i2ctx
			lcall i2cstr	; restart
			mov a,#compas+1	; read from compass
			lcall i2ctx
			setb c			; want to send NACK as only read 1 byte
			lcall i2crx		; read bearing as byte
			lcall scomtx8	; transmit
            mov dptr,#crlf
            lcall scomtxc
            sjmp loop        

startmsg:	.db "CMPS03 compass module test\r\n\0" 
crlf:       .db "\r\n\0"
;======================================================================
; scominit
; Initialises the serial port to transmit/receive data at 2400 baud
; Parameters:   NONE
; Returns:      NOTHING
;======================================================================
scominit:   anl     tmod,#0fh   ; clear t1 control bits
            orl     tmod,#20h   ; set t1 for mode 2 (auto reload)
            mov     th1,#243    ; 2400 baud with 12Mhz clock (actually 2403.8)
            setb    tr1         ; TCON.6 - run timer 1
            ;orl     pcon,#80h   ; set SMOD - 2x baud ->4800
            mov     scon,#50h   ; 8 bit UART, receive enabled
            clr     ti          
            clr     ri
            ret

;======================================================================
; scomtxa
; Transmits the character in A
; Parameters:   char to transmit in A
; Returns:      NOTHING
;======================================================================
scomtxa:    mov     sbuf,a      ; transmit a
scomtxw:    jnb     scon.1, scomtxw ; wait until char transmitted
            clr     scon.1      ; reset TI before next char
            ret

;======================================================================
; scomtxc
; Transmits the zero terminated string, from code memory, pointed
; to by DPTR
; Parameters:   Pointer to code string in DPTR
; Returns:      NOTHING
; Modifies:     A, DPTR
;======================================================================
scomtxc:    clr     a           ; zero offset
            movc    a,@a+dptr
            jz      scomcz      ; 0 byte for end of string
            acall    scomtxa    ; transmit char
            inc     dptr
            sjmp    scomtxc     ; next char
scomcz:     ret


;======================================================================
; scomtx8
; Transmits A as 2 hex digits
; Parameters:   byte to transmit in A
; Returns:      NOTHING
; Modifies:     A,B,DPTR
;======================================================================
scomtx8:    mov     b,a         ; save for low byte
            swap    a           ; get ms nibble -> ls
            anl     a,#15       ; zero high nibble
            acall   scomhcon    ; convert to hex
            acall   scomtxa     ; and send it
            mov     a,b         ; get ls nibble saved earlier
            anl     a,#15       ; zero high nibble
            acall   scomhcon    ; convert to hex
            acall   scomtxa     ; and send it
            ret
scomhcon:   inc     a           ; need to allow 1 byte for ret
            movc    a,@a+pc     ; look up to convert binary to hex digit
            ret
scomhtab:   .db "0123456789ABCDEF"


;======================================================================

; I2C Start routine
i2cstr:		setb sda
			nop
			nop
			nop
			nop
			setb scl
			nop
			nop
			nop
			nop
			clr sda
			nop
			nop
			nop
			nop
			clr scl
			nop
			nop
			nop
			ret


; I2C Stop routine
i2cstp:		clr sda
			nop
			nop
			nop
			nop
			setb scl
			nop
			nop
			nop
			nop
			setb sda
			nop
			nop
			nop
			ret

; I2C transmit.  Byte in A
; returns ~ACK in C
; uses R0
i2ctx:		mov r0,#8
i2ctx1:		rlc a				; msb -> A
			mov sda,c
			setb scl
			nop
			nop
			nop
			nop
			clr scl
			nop
			djnz r0,i2ctx1
			nop
			nop
			setb sda
			setb scl
			nop
			nop
			nop
			mov c,sda			; get ack bit, low is ACK, high NACK
			clr scl
			ret

; I2C receive.  Ack flag in C, returns byte in A
; uses R0
i2crx:		setb sda			; make sure data released
			mov r0, #8
i2crx1:		rlc a
			setb scl
i2crx2:		jnb scl,i2crx2		; clock stretch from slave?
			nop
			nop
			mov c,sda
			clr scl
			nop
			nop
			djnz r0,i2crx1
			rlc a				; last bit to acc & ack to C
			mov sda,c
			setb scl
			nop
			nop
			nop
			nop
			clr scl
			setb sda
			ret






