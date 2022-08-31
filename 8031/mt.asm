;======================================================================
; MONITOR.ASM
; Machine code monitor main program
;
; Resources:
; T0 - main timing interrupt.
; T1 - baud rate generator.
; P3.2:P3.5 - Dual I2C master.
;======================================================================

;======================================================================
; Define interrupt vectors at start of dual mapped ram
;======================================================================
           .equ rambase,0-    ;start of dual mapped ram, below this EPROM or Data RAM
;            .equ intIE0,rambase+32
;            .equ intTF0,rambase+64
;            .equ intIE1,rambase+96
;            .equ intTF1,rambase+128
;            .equ intCom,rambase+160
;            .equ intTF2,rambase+192
;            .equ user,rambase+256

;======================================================================
;-- Reset & interrupt vectors
;======================================================================
            .org    0000h
            sjmp    monitor     ;jump over interrupt vectors
            .org    0003h       ;IE0 external interrupt 0 vector
            ljmp    intIE0
            .org    000Bh       ;TF0 timer/counter 0 interrupt vector
            ljmp    timeint     ;direct to main timing interrupt
            .org    0013h       ;IE1 external interrupt 1 vector
            ljmp    intIE1
            .org    001Bh       ;TF1 timer/counter 1 interrupt vector
            ljmp    intTF1
            .org    0023h       ;serial interrupt vector.
            ljmp    intCom
            .org    002Bh       ;T2
            ljmp    intTF2



;======================================================================
;-- Hardware equates
;======================================================================

;======================================================================
; Bit addresses of i2c lines, 2 software I2C masters, I2C-A and I2C
;======================================================================
 ;               .equ sda_a, P3.3         ; bit address of I2C-A SDA
 ;               .equ scl_a, P3.2         ; bit address of I2C-A SCL
 ;               .equ sda_b, P3.5         ; bit address of I2C-B SDA
 ;               .equ scl_b, P3.4         ; bit address of I2C-B SCL


;======================================================================
			mov a,65
		sjmp $;
.end


