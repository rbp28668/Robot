; vmtest.asm
; Robot virtual machine test harness.
; Runs any VM code loaded to 1024h
gotest:     mov dptr, #tstst
            lcall  scomtxc

		  	lcall vminit  
			mov dptr,#1024	; start address
			lcall vmstart
			lcall vmrun


		  	ret

tstst:     .db     "Running VM Test\r\n\0"
tsttxc:    .db  	"String in Code memory\n\0"	; for testing TXSTRC
tstbeep:    .db psgToneA,128
            .db psgToneA+1,2
            .db psgEnvPd,0
            .db psgEnvPd+1,16
            .db psgEnvPr,0b         ; single decay
            .db psgAmpA,010h        ; use envelope
            .db psgEnabl,00111110b   ; enable tone A only
            .db 0ffh                ; end

			.org gotest + 100h  ; should be 4200h
ccall:		mov dptr, #tst1
			lcall  scomtxc
			ret

tst1:	    .db "CCALL - running native code\n\0"

