;======================================================================
; VPTEST.APP
; Creates a test harness for the virtual machine & virtual machine
; extensions.
;======================================================================

;ROM layout:
;0000 - 07ff   monitor page 0
;0800 - 0fff   monitor page 1
;1000 - 17ff   virtual processor
;1800 - 1fff   virtual processor extensions
;2000 - 27ff   finite state machine

:;******************************************************************
:;*                       vmstubgn.asm                             *
:;* Autogenerated file for running ROBOT VIRTUAL MACHINE           *
:;*                                                                *
:;******************************************************************

:;******************************************************************
:; Variables for monitor, fsm & VM processor
:;******************************************************************
include ..\Variables.asm

:;******************************************************************
:; Entry points for monitor
:;******************************************************************
include monjmp.asm

:;******************************************************************
:; Stub code
:            .org  4100h                 ; user space
include vmstub.asm

:
:;******************************  END  *******************************
:.end
output  vmstubgn.asm
run     a51 vmstubgn.asm

