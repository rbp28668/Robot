; vmstub.asm
; 8051 assembler stub to start up Virtual machine.
; Runs any VM code loaded to 1024h
            mov dptr, #start
            lcall  scomtxc

		  	lcall vminit  
			mov dptr,#1024	; start address
			lcall vmstart
			lcall vmrun
	  	    ret

start:     .db     "Start VM\r\n\0"

