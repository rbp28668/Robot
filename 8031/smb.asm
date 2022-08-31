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
                decR1
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
                orl #1          ; set read bit
                lcall i2ctxa     ; send bus read address of device

                setb c          ; want to send NACK as last byte read
                call i2crxa      ; read the byte

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
                orl #1          ; set read bit
                lcall i2ctxa     ; send bus read address of device

                clr c           ; want to send ACK as not last byte read
                call i2crxa      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1

                setb c          ; want to send NACK as last byte read
                call i2crxa      ; read the byte

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
                decR1
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
                orl #1          ; set read bit
                lcall i2ctxb     ; send bus read address of device

                setb c          ; want to send NACK as last byte read
                call i2crxb      ; read the byte

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
                orl #1          ; set read bit
                lcall i2ctxb     ; send bus read address of device

                clr c           ; want to send ACK as not last byte read
                call i2crxb      ; read the byte

		mov @R1,A	; save result as LS byte of TOS
		inc R1

                setb c          ; want to send NACK as last byte read
                call i2crxb      ; read the byte

		mov @R1,A	; save result as MS byte of TOS 
		dec R1
               
                lcall i2cspb
                ret


