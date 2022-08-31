;---------------------------------------------------------------------
; File: smb.inc
; Definitions for external SMB functions and callbacks.
; Define _SMBSLAVE when including in smbslave.asm
;--------------------------------------------------------------------

;---------------------------------------------------------------------
; Callbacks implemented by calling program
;---------------------------------------------------------------------

#ifdef _SMBSLAVE
    ; Called with byte written to device in W.  Current address in smbaddr.
    extern SMBWrite

    ; Called to get byte from current SMB address (in smbaddr) and return 
    ; it in W
    extern SMBRead

    ; Called once a read from a slave is complete (master has
    ; asserted NACK at the end of the read).
    extern SMBRdEnd

	; Called in polling loop
	extern SMBPoll

#else
    ; Called with byte written to device in W.  Current address in smbaddr.
    global SMBWrite

    ; Called to get byte from current SMB address (in smbaddr) and return 
    ; it in W
    global SMBRead

    ; Called once a read from a slave is complete (master has
    ; asserted NACK at the end of the read).
    global SMBRdEnd

	; Called in polling loop
	global SMBPoll

#endif


;---------------------------------------------------------------------
; Entry points (and variable) for SMB slave code.
;---------------------------------------------------------------------

#ifdef _SMBSLAVE

    ; Call with left shifted slave address in W
	global SMBInit

    ; Goto to handle input
	global SMBHandler

    ; Current SMB (i.e. on device) address.
	global smbaddr

#else     
    ; Call with left shifted slave address in W
	extern SMBInit

    ; Goto to handle input
	extern SMBHandler

    ; Current SMB (i.e. on device) address.
	extern smbaddr



#endif

