ROM layout:
0000 - 07ff   monitor page 0
0800 - 0fff   monitor page 1
1000 - 17ff   virtual processor
1800 - 1fff   virtual processor extensions

include Variables.asm
include Monitor.asm
:
: ;********************* ROBOT VIRTUAL MACHINE **********************
:            .org  1000h                 ; 3rd 2k page
include Virtproc.asm

:
: ;********************* ROBOT VM EXTENSIONS **********************
:            .org 1800h   ; 4th 2k page
include VMSupport.asm

:
:
: ;******************************  END  *******************************
: .end
output  Robot.asm
run     asm51 robot.asm


