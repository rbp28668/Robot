;======================================================================
; Robot Virtual Machine support routines
; Prefix: vx
;======================================================================

;======================================================================
; vxDisp
; vm extension dispatch - runs one of the VM extension routines.
; Enter with the extension code in A (0..127).
;======================================================================
vxDisp:     anl   A,#07Fh         ; make sure top bit is zero
            rl    A               ; 2 bytes per jump table entry
            mov   dptr,#vxjmp     ; jump table base
            jmp   @A+dptr         

;======================================================================
; vxjmp - VM extension jump table
;======================================================================
vxjmp:      ajmp  vxill           ;0  ILLEGAL
            ajmp  vxstop          ;1  Stop processing
            ajmp  vxsini          ;2  intitialise serial port
            ajmp  vxstxa          ;3  transmit character from LS byte of TOS
            ajmp  vxstxc          ;4  Transmit string from code memory pointed to by TOS.
            ajmp  vxstxx          ;5  Transmit string from external data memory pointed to by TOS
            ajmp  vxstx8          ;6  Transmit LS byte of TOS as 2 hex digits
            ajmp  vxst16          ;7  Transmit 16 bit TOS as 4 hex digits
            ajmp  vxsrx           ;8  receive an ascii char & push char code
            ajmp  vxsrx8          ;9  receive a hex encoded byte & push 
            ajmp  vxsr16          ;10  receive a hex encoded 16 bit word & push
            ajmp  vxihex          ;11  read intelhex file in
            ajmp  vxsmtxba        ;12  Transmit SMBus byte on channel A
            ajmp  vxsmtxwa        ;13  Transmit SMBus word on channel A
            ajmp  vxsmrxba        ;14  Receive SMBus byte on channel A
            ajmp  vxsmrxwa        ;15  Receive SMBus word on channel A
            ajmp  vxsmtxbb        ;16  Transmit SMBus byte on channel B
            ajmp  vxsmtxwb        ;17  Transmit SMBus word on channel B
            ajmp  vxsmrxbb        ;18  Receive SMBus byte on channel B
            ajmp  vxsmrxwb        ;19  Receive SMBus word on channel B
            ajmp  vxrdp1          ;20  Read Port 1
            ajmp  vxwrp1          ;21  Write Port 1
            ajmp  vxill           ;22  ILLEGAL
            ajmp  vxill           ;23  ILLEGAL
            ajmp  vxill           ;24  ILLEGAL
            ajmp  vxdump          ;25  dump VM state to serial link
            ajmp  vxtset          ;26  set countdown timer from TOS
            ajmp  vxtim           ;27  get value of countdown timer to TOS
            ajmp  vxill           ;28  TOS (bottom 6 bits) to motor control
            ajmp  vxupb10         ;29  Print TOS in base 10 (unsigned)
            ajmp  vxpb10          ;30  Print TOS in base 10 (signed)
            ajmp  vxill           ;31  ILLEGAL
            ajmp  vxill           ;32  ILLEGAL
            ajmp  vxill           ;33  ILLEGAL
            ajmp  vxill           ;34  ILLEGAL
            ajmp  vxill           ;35  ILLEGAL
            ajmp  vxill           ;36  ILLEGAL
            ajmp  vxill           ;37  ILLEGAL
            ajmp  vxill           ;38  ILLEGAL
            ajmp  vxill           ;39  ILLEGAL
            ajmp  vxill           ;40  ILLEGAL
            ajmp  vxill           ;41  ILLEGAL
            ajmp  vxill           ;42  ILLEGAL
            ajmp  vxill           ;43  ILLEGAL
            ajmp  vxill           ;44  ILLEGAL
            ajmp  vxill           ;45  ILLEGAL
            ajmp  vxill           ;46  ILLEGAL
            ajmp  vxill           ;47  ILLEGAL
            ajmp  vxill           ;48  ILLEGAL
            ajmp  vxill           ;49  ILLEGAL
            ajmp  vxill           ;50  ILLEGAL
            ajmp  vxill           ;51  ILLEGAL
            ajmp  vxill           ;52  ILLEGAL
            ajmp  vxill           ;53  ILLEGAL
            ajmp  vxill           ;54  ILLEGAL
            ajmp  vxill           ;55  ILLEGAL
            ajmp  vxill           ;56  ILLEGAL
            ajmp  vxill           ;57  ILLEGAL
            ajmp  vxill           ;58  ILLEGAL
            ajmp  vxill           ;59  ILLEGAL
            ajmp  vxill           ;60  ILLEGAL
            ajmp  vxill           ;61  ILLEGAL
            ajmp  vxill           ;62  ILLEGAL
            ajmp  vxill           ;63  ILLEGAL
            ajmp  vxill           ;64  ILLEGAL
            ajmp  vxill           ;65  ILLEGAL
            ajmp  vxill           ;66  ILLEGAL
            ajmp  vxill           ;67  ILLEGAL
            ajmp  vxill           ;68  ILLEGAL
            ajmp  vxill           ;69  ILLEGAL
            ajmp  vxill           ;70  ILLEGAL
            ajmp  vxill           ;71  ILLEGAL
            ajmp  vxill           ;72  ILLEGAL
            ajmp  vxill           ;73  ILLEGAL
            ajmp  vxill           ;74  ILLEGAL
            ajmp  vxill           ;75  ILLEGAL
            ajmp  vxill           ;76  ILLEGAL
            ajmp  vxill           ;77  ILLEGAL
            ajmp  vxill           ;78  ILLEGAL
            ajmp  vxill           ;79  ILLEGAL
            ajmp  vxill           ;80  ILLEGAL
            ajmp  vxill           ;81  ILLEGAL
            ajmp  vxill           ;82  ILLEGAL
            ajmp  vxill           ;83  ILLEGAL
            ajmp  vxill           ;84  ILLEGAL
            ajmp  vxill           ;85  ILLEGAL
            ajmp  vxill           ;86  ILLEGAL
            ajmp  vxill           ;87  ILLEGAL
            ajmp  vxill           ;88  ILLEGAL
            ajmp  vxill           ;89  ILLEGAL
            ajmp  vxill           ;90  ILLEGAL
            ajmp  vxill           ;91  ILLEGAL
            ajmp  vxill           ;92  ILLEGAL
            ajmp  vxill           ;93  ILLEGAL
            ajmp  vxill           ;94  ILLEGAL
            ajmp  vxill           ;95  ILLEGAL
            ajmp  vxill           ;96  ILLEGAL
            ajmp  vxill           ;97  ILLEGAL
            ajmp  vxill           ;98  ILLEGAL
            ajmp  vxill           ;99  ILLEGAL
            ajmp  vxill           ;100  ILLEGAL
            ajmp  vxill           ;101  ILLEGAL
            ajmp  vxill           ;102  ILLEGAL
            ajmp  vxill           ;103  ILLEGAL
            ajmp  vxill           ;104  ILLEGAL
            ajmp  vxill           ;105  ILLEGAL
            ajmp  vxill           ;106  ILLEGAL
            ajmp  vxill           ;107  ILLEGAL
            ajmp  vxill           ;108  ILLEGAL
            ajmp  vxill           ;109  ILLEGAL
            ajmp  vxill           ;110  ILLEGAL
            ajmp  vxill           ;111  ILLEGAL
            ajmp  vxill           ;112  ILLEGAL
            ajmp  vxill           ;113  ILLEGAL
            ajmp  vxill           ;114  ILLEGAL
            ajmp  vxill           ;115  ILLEGAL
            ajmp  vxill           ;116  ILLEGAL
            ajmp  vxill           ;117  ILLEGAL
            ajmp  vxill           ;118  ILLEGAL
            ajmp  vxill           ;119  ILLEGAL
            ajmp  vxill           ;120  ILLEGAL
            ajmp  vxill           ;121  ILLEGAL
            ajmp  vxill           ;122  ILLEGAL
            ajmp  vxill           ;123  ILLEGAL
            ajmp  vxill           ;124  ILLEGAL
            ajmp  vxill           ;125  ILLEGAL
            ajmp  vxill           ;126  ILLEGAL
            ajmp  vxill           ;127  ILLEGAL

;======================================================================
; vxill
; Illegal extension (not implemented)
;======================================================================
vxill:      mov dptr, #vxillm
            lcall  scomtxc
            ljmp vmhalt

vxillm:     .db     "VMX Illegal Instruction\r\n\0"

;======================================================================
; vxstop
; stop program execution.
;======================================================================
vxstop:     ljmp stop          ; stop execution

;======================================================================
; vxsini
; initialise serial port
;======================================================================
vxsini:     ljmp scominit      ; intitialise serial port

;======================================================================
; vxstxa
; transmit character from LS byte of TOS
;======================================================================
vxstxa:     mov A,@R1           ; get lsbyte of TOS to transmit
            dec R1
            dec R1
            ljmp scomtxa        ; transmit ascii char in A

;======================================================================
; vxstxc
; Transmit string from code memory pointed to by TOS.
;======================================================================
  vxstxc:   mov A,@R1
            mov dpl,A
            inc R1
            mov A,@R1
            mov dph,A
            dec R1
            dec R1
            dec R1
            ljmp scomtxc       ; transmit string from code mem ptr to by dptr
                                       
;======================================================================
; vxstxx
; Transmit string from external data memory pointed to by TOS
;======================================================================
  vxstxx:   mov A,@R1
            mov dpl,A
            inc R1
            mov A,@R1
            mov dph,A
            dec R1
            dec R1
            dec R1
            ljmp scomtxx       ; transmit string from ext mem ptr to by dptr

;======================================================================
; vxstx8
; Transmit LS byte of TOS as 2 hex digits
;======================================================================
vxstx8:     mov A,@R1           ; get lsbyte of TOS to transmit
            dec R1
            dec R1
            ljmp scomtx8       ; transmit byte in A as 2 hex digits

;======================================================================
; vxst16
; Transmit 16 bit TOS as 4 hex digits
;======================================================================
vxst16:     inc R1
            mov A,@R1           ; get msbyte of TOS to transmit
            lcall scomtx8       ; transmit byte in A as 2 hex digits
            dec R1
            mov A,@R1           ; get lsbyte of TOS to transmit
            dec R1
            dec R1
            ljmp scomtx8        ; transmit byte in A as 2 hex digits

;======================================================================
; vxsrx
; receive an ascii char & push char code
;======================================================================
vxsrx:      lcall scomrxa       ; receive an ascii char in A
            inc R1
            inc R1
            mov @R1,A
            inc R1
            clr A
            mov @R1,A
            dec R1
            ret

;======================================================================
; vxsrx8
; receive a hex encoded byte & push 
;======================================================================
vxsrx8:     lcall scomrhx8      ; receive a hex encoded byte into A
            inc R1
            inc R1
            mov @R1,A
            inc R1
            mov @R1,#0			; zero ms byte
            dec R1
            ret

;======================================================================
; vxsr16
; receive a hex encoded 16 bit word & push
;======================================================================
vxsr16:     lcall scomrh16      ; receive a hex encoded word into dptr
            inc R1
            inc R1
            mov @R1, dpl
            inc R1
            mov @R1, dph
            dec R1
            ret

;======================================================================
; vxihex
; read intelhex file in
;======================================================================
vxihex:     ljmp ihexin        ; read intelhex file into data memory


;======================================================================
; vxtset
; Set timer from TOS
;======================================================================
vxtset:     mov A,@R1
            dec R1
            dec R1
            ljmp settim        ; setup timer - value in A

;======================================================================
; vxtim
; Get timer value to TOS
;======================================================================
vxtim:      inc R1
            inc R1
            lcall timer         ; get timer count - return value in A
            mov @R1,A
            inc R1
            mov @R1,#0          ; zero MS byte of result
            dec R1
            ret


;======================================================================
; vxdump
; dumps state of VM
;======================================================================
vxdump:     mov dptr,#vxdmp0	  ; ESP
		lcall scomtxc
		  mov A,R1
		  lcall scomtx8
		  mov A,#'\n'
		  lcall scomtxa

		  mov dptr,#vxdmp1	  ; RSP
		  lcall scomtxc
		  mov A,R5
		  lcall scomtx8
		  mov A,R4
		  lcall scomtx8
		  mov A,#10 	; linefeed
		  lcall scomtxa

		  mov dptr,#vxdmp2	  ; FP
		  lcall scomtxc
		  mov A,R3
		  lcall scomtx8
		  mov A,R2
		  lcall scomtx8
		  mov A,#10 	; linefeed
		  lcall scomtxa

		  mov dptr,#vxdmp3	  ; RSP
		  lcall scomtxc
		  mov A,R7
		  lcall scomtx8
		  mov A,R6
		  lcall scomtx8
		  mov A,#10 	; linefeed
		  lcall scomtxa

		  ; Write the stack.
		  mov A,R1
		  mov R0,A
vxdmpl:	  cjne R0,#(vmestk-2),vxdmpp
		  mov A,#10 	; linefeed
		  lcall scomtxa
		  ret
vxdmpp:	  inc R0
		  mov A,@R0
		  lcall scomtx8
		  dec R0
		  mov A,@R0
		  lcall scomtx8
		  mov A,#10 	; linefeed
		  lcall scomtxa
		  dec R0
		  dec R0
		  sjmp vxdmpl

vxdmp0:	  .db "ESP: \0"
vxdmp1:   .db "RSP: \0"
vxdmp2:   .db "FP:  \0"
vxdmp3:   .db "IP:  \0"

;======================================================================
; vxupb10
; Prints top of stack in base 10 (unsigned)
; loop version using return stack to reverse the digits
;======================================================================
vxupb10:
		; push an initial 0 to the return stack to mark end of digits
		inc R1
		inc R1
		mov @R1,#0
		
		lcall vmtor

p10in:  
		; push base
		inc R1
		inc R1
		mov @R1,#10
		inc R1
		mov @R1,#0
		dec R1

		; acc -> acc div 10, acc mod 10
		lcall vmudm		; unsigned div-mod
		mov a,@R1
		add a,#'0'
		mov @R1,a	
		lcall vmtor

		mov a,@R1
		inc R1
		orl a,@r1
		dec R1
		jnz p10in

p10out:	        lcall vmfromr
		mov a,@r1
		jz p10end
		acall vxstxa    ; send char
		sjmp p10out

p10end:	ret

;======================================================================
; vxpb10
; Prints top of stack in base 10 
;======================================================================
vxpb10:	        inc R1
		mov A,@R1
		dec R1
		jnb Acc.7,vxupb10	; no sign if bit 7 clear

		; -ve so output a - sign
		mov a,#'-'
		lcall scomtxa

		; negate the accumulator & print it.
		clr C
		mov a,#0
		subb a,@R1
		mov @R1,a
		inc R1
		mov a,#0
		subb a,@R1
		mov @R1,a
		dec R1
		sjmp vxupb10

;======================================================================
; pboot
; Runs vm code at 0400h	& prints fault code on completion.
;======================================================================
pboot: 		mov dptr, #pbmsg
                lcall  scomtxc

                lcall vminit  
                mov dptr,#1024	; start address
                lcall vmstart
                lcall vmrun
                mov dptr, #pbflt
                lcall  scomtxc
                mov a,vmfltc	; fault code
                lcall  scomtx8
                mov a, #13 		; cr
                lcall scomtxa
                mov a, #10 		; lf
                lcall scomtxa

                ret

pbmsg:     .db     "Start VM at 1024\r\n\0"
pbflt:	   .db	   "VM stopped, code \0"


;*********************************************************************
; Virtual machine extensions for SMB routines
;
; Notes around VM & SMB:
; Little endian - least significant byte at lowest numbered address
; R1 points to top WORD of data stack - need to pre-increment for push, post decrement for pop
; @R1 is LS byte
; SMB protocol reads/writes words LS byte first.
;*********************************************************************


;======================================================================
; vxsmtxba
; Virtual machine SMB transmit byte on channel A
; TOS - Bus address
; 2TOS - device internal address
; 3TOS - data to write
; Write byte:
; tx Start
; tx device bus addr
; tx device addr
; tx device data
; txStop
;======================================================================

vxsmtxba:       lcall i2csta    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
                dec R1
                dec R1
                lcall i2ctxa     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                dec R1
                dec R1
                lcall i2ctxa

                mov A,@R1       ; and data byte to send
                dec R1
                dec R1
                lcall i2ctxa
                lcall i2cspa
                ret



;======================================================================
; vxsmtxwa
; Virtual machine SMB transmit word on channel A
; TOS - Bus address
; 2TOS - device internal address
; 3TOS - data to write
; Write word:
; tx Start
; Tx device bus addr
; Tx device addr
; tx device data
; tx device data
; tx Stop
;======================================================================

vxsmtxwa:       lcall i2csta    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
                dec R1
                dec R1
                lcall i2ctxa     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                dec R1
                dec R1
                lcall i2ctxa

                mov A,@R1       ; and ls data byte to send
                lcall i2ctxa
		inc R1
                mov A,@R1       ; and ms data byte to send
                dec R1
                lcall i2ctxa
                dec R1
                dec R1
                lcall i2cspa
                ret


;======================================================================
; vxsmrxba
; Virtual machine SMB recive byte on channel A
; TOS - Bus address
; 2TOS - device internal address
; Returns read data on TOS
; read byte:
; tx start
; tx device bus addr
; tx device addr
; tx (re)start
; tx device bus addr + 1
; rx byte with NACK set
;======================================================================

vxsmrxba:       lcall i2csta    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
		mov B,A		; save it in B for later
                dec R1
                dec R1
                lcall i2ctxa     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                lcall i2ctxa

		lcall i2csta    ; restart
		
                mov A,B		; saved bus addr
                orl A,#1          ; set read bit
                lcall i2ctxa     ; send bus read address of device

                setb c          ; want to send NACK as last byte read
                lcall i2crxa      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1
		mov @R1,#0	; clear MS byte of TOS 
		dec R1
               
                lcall i2cspa
                ret

;======================================================================
; vxsmrxwa
; Virtual machine SMB recive word on channel A
; TOS - Bus address
; 2TOS - device internal address
; Returns read data on TOS
; read word:
; tx start
; tx device bus addr
; tx device addr
; tx (re)start
; tx device bus addr + 1
; rx ls byte with NACK clear
; rx ms byte with NACK set
;======================================================================
vxsmrxwa:       lcall i2csta    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
		mov B,A		; save it in B for later
                dec R1
                dec R1
                lcall i2ctxa     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                lcall i2ctxa

		lcall i2csta    ; restart
		
                mov A,B		; saved bus addr
                orl A,#1          ; set read bit
                lcall i2ctxa     ; send bus read address of device

                clr c           ; want to send ACK as not last byte read
                lcall i2crxa      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1

                setb c          ; want to send NACK as last byte read
                lcall i2crxa      ; read the byte

		mov @R1,A	; save result as MS byte of TOS 
		dec R1
               
                lcall i2cspa
                ret






;======================================================================
; vxsmtxbb
; Virtual machine SMB transmit byte on channel B
; TOS - Bus address
; 2TOS - device internal address
; 3TOS - data to write
; Write byte:
; tx Start
; tx device bus addr
; tx device addr
; tx device data
; txStop
;======================================================================

vxsmtxbb:       lcall i2cstb    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
                dec R1
                dec R1
                lcall i2ctxb     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                dec R1
                dec R1
                lcall i2ctxb

                mov A,@R1       ; and data byte to send
                dec R1
                dec R1
                lcall i2ctxb
                lcall i2cspb
                ret



;======================================================================
; vxsmtxwb
; Virtual machine SMB transmit word on channel B
; TOS - Bus address
; 2TOS - device internal address
; 3TOS - data to write
; Write word:
; tx Start
; Tx device bus addr
; Tx device addr
; tx device data
; tx device data
; tx Stop
;======================================================================

vxsmtxwb:       lcall i2cstb    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
                dec R1
                dec R1
                lcall i2ctxb     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                dec R1
                dec R1
                lcall i2ctxb

                mov A,@R1       ; and ls data byte to send
                lcall i2ctxb
		inc R1
                mov A,@R1       ; and ms data byte to send
                dec R1
                lcall i2ctxb
                dec R1
                dec R1

                lcall i2cspb
                ret


;======================================================================
; vxsmrxbb
; Virtual machine SMB recive byte on channel B
; TOS - Bus address
; 2TOS - device internal address
; Returns read data on TOS
; read byte:
; tx start
; tx device bus addr
; tx device addr
; tx (re)start
; tx device bus addr + 1
; rx byte with NACK set
;======================================================================

vxsmrxbb:       lcall i2cstb    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
		mov B,A		; save it in B for later
                dec R1
                dec R1
                lcall i2ctxb     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                lcall i2ctxb

		lcall i2cstb    ; restart
		
                mov A,B		; saved bus addr
                orl A,#1          ; set read bit
                lcall i2ctxb     ; send bus read address of device

                setb c          ; want to send NACK as last byte read
                lcall i2crxb      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1
		mov @R1,#0	; clear MS byte of TOS 
		dec R1
               
                lcall i2cspb
                ret

;======================================================================
; vxsmrxwb
; Virtual machine SMB recive word on channel B
; TOS - Bus address
; 2TOS - device internal address
; Returns read data on TOS
; read word:
; tx start
; tx device bus addr
; tx device addr
; tx (re)start
; tx device bus addr + 1
; rx ls byte with NACK clear
; rx ms byte with NACK set
;======================================================================
vxsmrxwb:       lcall i2cstb    ; start
                mov A,@R1       ; get lsbyte of tos for bus addr
		mov B,A		; save it in B for later
                dec R1
                dec R1
                lcall i2ctxb     ; send bus address of device
                
                mov A,@R1       ; get lsbyte of tos for device internal addr
                lcall i2ctxb

		lcall i2cstb    ; restart
		
                mov A,B		; saved bus addr
                orl A,#1          ; set read bit
                lcall i2ctxb     ; send bus read address of device

                clr c           ; want to send ACK as not last byte read
                lcall i2crxb      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1

                setb c          ; want to send NACK as last byte read
                lcall i2crxb      ; read the byte

		mov @R1,A	; save result as MS byte of TOS 
		dec R1
               
                lcall i2cspb
                ret

;======================================================================
; vxrdp1
; Read port 1 and return data as LSB of TOS
;======================================================================
vxrdp1:         mov A,p1
                inc R1
                inc R1
                mov @R1,A
                inc R1
                mov @R1,#0
                dec R1
                ret

;======================================================================
; vxwrp1
; Write port 1 with LSB of TOS
;======================================================================
vxwrp1:        mov A,@R1
               mov p1,A
               dec R1
               dec R1
               ret



