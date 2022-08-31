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
; Gives isolation between robot monitor code & other routines
;======================================================================

        .equ monjtab,1000h - 100h       ; allow 256 bytes at top of bottom 4k of EPROM
        .equ stop,monjtab + 0           ; stop execution
        .equ scominit,monjtab + 3       ; intitialise serial port
        .equ scomtxa,monjtab + 6        ; transmit ascii char in A
        .equ scomtxc,monjtab + 9        ; transmit string from code mem ptr to by dptr
        .equ scomtxx,monjtab + 12       ; transmit string from ext mem ptr to by dptr
        .equ scomtx8,monjtab + 15       ; transmit byte in A as 2 hex digits
        .equ scomrxa,monjtab + 18       ; receive an ascii char in A
        .equ scomrhx8,monjtab + 21      ; receive a hex encoded byte into A
        .equ scomrh16,monjtab + 24      ; receive a hex encoded word into dptr
        .equ ihexin,monjtab + 27        ; read intelhex file into data memory
        .equ psgInit,monjtab + 30       ; initialise the AY-3-8910 psg
        .equ psgwr,monjtab + 33         ; write r0 to psg register A
        .equ psgwr16,monjtab + 36       ; write dptr to psg register A, A+1
        .equ psgwrc,monjtab + 39        ; write data from code memory @dptr to psg
        .equ adcrd,monjtab + 42         ; read from TLC548 ADC.
        .equ pioInit,monjtab + 45       ; init the PIO port A,B outut, C input
        .equ pioWrtA,monjtab + 48       ; write to PIO port A
        .equ pioWrtB,monjtab + 51       ; write to PIO port B
        .equ pioRdC,monjtab + 54        ; read from PIO port C
        .equ colDet,monjtab + 57        ; check for collision
        .equ colStat,monjtab + 60       ; check current collision status
        .equ colClear,monjtab + 63      ; clear collision detect bits
        .equ servset,monjtab + 66       ; set servo position
        .equ settim,monjtab + 69        ; setup timer - value in A
        .equ timer,monjtab + 72         ; get timer count - return value in A
        .equ tkchain,monjtab + 75       ; chain tick count interrupt (dptr in, dptr out)

;======================================================================
; Virtual Machine entry points (in EPROM)
;======================================================================
		.equ vminit,1000h				; initialise the virtual machine
		.equ vmreset,100Bh				; Resets the virtual machine
		.equ vmstart,101Bh				; Starts up the virtual machine IP in dptr
		.equ vmrun,	1022h				; Runs the vm until it hits a terminate instruction.
		.equ vmidle,102Ah				; Determines whether the vm is idle (stopped)
		.equ vmsrsp,102Eh				; Sets the return stack pointer (defaults to 0)
		.equ vmdisp,1039h				; Virtual Machine Dispatch routine.


;======================================================================
;-- Motor control bit patterns
;-- Write to P1 (SFR at 90H)
;======================================================================
            .equ    leftrev,    00ch    ; left motor reverse
            .equ    leftfwd,    014h    ; left motor forwards 
            .equ    rightfwd,   060h    ; right motor forwards 
            .equ    rightrev,   0A0h    ; right motor reverse
            .equ    forwards,   074h    ; both motors forwards
            .equ    reverse,    0ACh    ; both motors reverse
            .equ    spinrt,     0B4h    ; left fwd, right rev
            .equ    spinleft,   06Ch    ; right fwd, left rev

;======================================================================
; Register defines for AY-3-8910 psg
;======================================================================
            .equ psgToneA,0             ; 12 bit tone register for channel A
            .equ psgToneB,2             ; 12 bit tone register for channel B
            .equ psgToneC,4             ; 12 bit tone register for channel C
            .equ psgNoise,6             ; noise period (5 bits)
            .equ psgEnabl,7             ; enable register
            .equ psgAmpA,8              ; channel A amplitude
            .equ psgAmpB,9              ; channel B amplitude
            .equ psgAmpC,10             ; channel C amplitude
            .equ psgEnvPd,11            ; envelope period (16 bits)
            .equ psgEnvPr,13            ; envelope profile (4 bit)
            .equ psgPortA,14            ; i/o port A
            .equ psgPortB,15            ; i/o port B

;======================================================================
; Collision detect bits
; cdm* are collision detect masks
; cdb* are collision detect bits
;======================================================================
            .equ cdmFront,0010b         ; FD   1101
            .equ cdmRight,0100b         ; FB   1011
            .equ cdmLeft,0001b          ; FE   1110
            .equ cdmBack,1000b          ; F7   0111
            .equ cdbFront,1             ; FD   1101
            .equ cdbRight,2             ; FB   1011
            .equ cdbLeft,0              ; FE   1110
            .equ cdbBack,3              ; F7   0111

;======================================================================
; ADC Channels
;======================================================================
            .equ adcBat,0               ; battery
            .equ adcWide,1              ; wide angle LDR
            .equ adcNarrow,2            ; narrow angle LDR
            .equ adcPIR,3               ; Passive Infra-Red

;======================================================================
; Collision Avoidance LEDs
;======================================================================
            .equ ledFar,1               ; far field (narrow) LEDs
            .equ ledNear,2              ; near field (wide) LEDs
