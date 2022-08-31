;======================================================================
;-- Robot Variables
;======================================================================

;======================================================================
;-- Internal memory layout
;======================================================================
;-- System variables at start of general purpose memory area
            .equ    varbase,    020h          ; start in bitmapped area
            .equ    vmflags,    varbase + 0   ; 1 byte for flags (bit mapped)
            .equ    vmsflag,    varbase + 1   ; 1 byte spare flags byte
            .equ    countdn,    varbase + 2   ; countdown byte for timing
            .equ    tickvect,   varbase + 3   ; 2 bytes for tick chaining vector (low,high)
            .equ    vpscratc,   varbase + 5   ; 8 bytes for virtual machine workspace					 
            .equ    dramaloc,   varbase + 13  ; 2 bytes for top of allocated data mem ptr
            .equ    state,      varbase + 15  ; 2 bytes for current state (pt to event records)
            .equ    stcnt,      varbase + 17  ; 1 byte for count of event records for this state
            .equ    vmfltc,     varbase + 19  ; 1 byte Virtual machine fault code.
            .equ    vmrstk,     varbase + 20  ; 2 bytes virtual machine return stack base.
            .equ    events,     varbase + 22  ; 2 byte pointer to event table
            .equ    stable,     varbase + 24  ; 2 byte pointer to state table
            .equ    stemap,     varbase + 26  ; 2 byte pointer to the event map for the current state.
            .equ    image,      varbase + 28  ; 2 byte pointer to image being executed.
            
            .equ    vmestk,     040h          ; Virtual machine eval stack base

; Alias the scratch area for arithmetic routines.
            .equ op_0,vpscratc
            .equ op_1,vpscratc+1
            .equ op_2,vpscratc+2
            .equ op_3,vpscratc+3
            .equ tmp_0,vpscratc+4
            .equ tmp_1,vpscratc+5
            .equ tmp_2,vpscratc+6
            .equ tmp_3,vpscratc+7


; -- bit addresses for virtual machine flags - map to vmFlags byte
            .equ    vmfStop,    00h ; - flag set when VM stopped
            .equ    vmChk,      01h ; - turn on extra run-time checks if set
            .equ    vmsign,     02h ; - flag to negate result of arithmetic
            .equ    vmsgn2,     03h ; - second flag for sign of arithmetic results.

;======================================================================
;-- External memory layout
;======================================================================
            .equ    evtabl,     0000h         ; location of event table.
