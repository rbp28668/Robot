;======================================================================
; Robot Virtual Machine
;======================================================================

;======================================================================
; Virtual machine fault codes
; These are the predefined ones..
;======================================================================
            .equ vNone,0                ; no fault
            .equ vHalt,1                ; halt fault
            .equ vIlleg,2               ; illegal instruction fault
            .equ vBound,3               ; bounds fault
            .equ vESUnd,4               ; eval stack underflow
            .equ vESOvf,5               ; eval stack overflow
            .equ vRSUnd,6               ; return stack underflow
            .equ vRSOvf,7               ; return stack overflow

;======================================================================
; *** CODE ***
;======================================================================

;======================================================================
; vminit
; Initialises the virtual machine
;   initialise the basic VM
;   set up the event list
;======================================================================
vminit:     mov acc,#0 					; set up basic RSP position in vmrstk
			mov vmrstk, acc				; LSB
			mov vmrstk+1, acc			; MSB
            sjmp vmreset

;======================================================================
; vmreset
; Resets the virtual machine
; Reset ESP
; Reset RSP
; Reset IP to 0 (invalid)
;======================================================================
vmreset:    mov vmFltc,#vNone           ; reset fault code
            clr  vmfStop                ; reset stopped bit
            mov R1,#(vmestk-2)          ; reset evaluation stack (-2 for preinc)
            mov R4,vmrstk               ; reset return stack LSB
            mov R5,vmrstk+1             ; reset return stack MSB
            mov R6,#0                   ; zero IP LSB
            mov R7,#0                   ; zero IP MSB
            ret

;======================================================================
; vmstart
; Starts up the virtual machine - vm is reset and then its IP is set
; to the start address pointed to by DPTR
;======================================================================
vmstart:    acall vmreset
            mov R6,dpl
            mov R7,dph
            ret

;======================================================================
; vmrun
; Runs the vm until it hits a terminate instruction.
; VM must have had its IP set up by call to vmstart before hand
;======================================================================
vmrun:      jb vmfStop,vmrune         ; abort if stopped bit is set
            acall vmdisp              ; single tick
            sjmp vmrun                ; and do next instruction
vmrune:     ret

;======================================================================
; vmidle
; Determines whether the vm is idle (stopped)
; returns C set if so, clear if still running
;======================================================================
vmidle:     mov cy,vmfStop
            ret

;======================================================================
; vmsrsp
; Sets the return stack pointer (defaults to 0)
; input - rsp value in dptr
;======================================================================
vmsrsp:		mov vmrstk,dpl
			mov vmrstk+1,dph
            mov R4,vmrstk               ; reset return stack LSB
            mov R5,vmrstk+1             ; reset return stack MSB
			ret

;======================================================================
; vmdisp
; Virtual Machine Dispatch routine.  Each time this is called it will
; execute one instruction in the virtual machine.
;======================================================================
vmdisp:     mov   dpl,R6          ; load IP into dptr
            mov   dph,R7
            movx  A,@dptr         ; get opcode
            inc   dptr

            mov   B,A
            anl   A,#0C0h         ;mask off top 2 bits
            jz    vmdis0          ;0 - no literal
            cjne  A,#040h,vmdis1
            
            ;1 - single byte, read from instruction stream & push
            movx  A,@dptr
            inc   dptr
            inc   R1
            inc   R1
            mov   @R1,A

			;sign extend
			rlc   A  		;sign bit to C
			mov   A,#255	
			addc  A,#0		; 0 if C was set (-ve), FF if cleared
			cpl   A			; so FF if was -ve, 0 if 0 or +ve
            inc   R1
            mov   @R1,A
            dec   R1
            sjmp  vmdis0

vmdis1:     cjne  A,#080h,vmdis2

            ;2 - word, read from instruction stream & push
            movx  A,@dptr
            inc   dptr
            inc   R1
            inc   R1
            mov   @R1,A
            inc   R1
            movx  A,@dptr
            inc   dptr
            mov   @R1,A
            dec   R1
            sjmp  vmdis0

vmdis2:     ; if we get here, the top 2 bits of the opcode are set
            ; and we have an illegal instruction
            ajmp vmill

            ; vmdis0 - main dispatch point, any literals have already
            ; been pushed, the opcode is in B & the new IP is in dptr
vmdis0:     mov   R6,dpl          ; save new IP
            mov   R7,dph
            mov   A,B
            anl   A,#03Fh         ; zero operand size bits
            rl    A               ; 2 bytes per jump table entry
            mov   dptr,#vmjmp     ; jump table base
            jmp   @A+dptr         

vmjmp:      ajmp  vmnop     ;0   NOP                             ( no operation)
            ajmp  vmadd     ;1   ADD     ia,ib  -  ia + ib       ( integer addition)
            ajmp  vmsub     ;2   SUB     ia,ib  -  ia - ib       ( integer subtraction)
            ajmp  vmmult    ;3   MULT    ia,ib  -  ia * ib       ( integer multiplication)
            ajmp  vmdiv     ;4   DIV     ia,ib  -  ia / ib       ( integer division)
            ajmp  vmmod     ;5   MOD     ia,ib  -  ia MOD ib     ( integer modulus)
            ajmp  vmand     ;6   AND     ia,ib  -  ia AND ib     ( bitwise and)
            ajmp  vmor      ;7   OR      ia,ib  -  ia OR ib      ( bitwise or)
            ajmp  vmnot     ;8   NOT     ia     -  NOT ia        ( 1s complement)
            ajmp  vmumul    ;9   UMUL	ua,ub  -  ua * ub       ( unsigned integer multiplication)
            ajmp  vmudiv    ;10  UDIV	ua,ub  -  ua / ub       ( unsigned integer division)
            ajmp  vmumod    ;11  UMOD	ua,ub  -  ua MOD ub     ( unsigned integer modulus)
            ajmp  vminc     ;12  INC     a		-  a+1		    ( increment )
            ajmp  vmdec     ;13  DEC	 a		-  a-1			( decrement )
            ajmp  vmill     ;14  ILLEGAL
            ajmp  vmequ     ;15  EQU     a,b    -  a = b         ( bitwise comparison )
            ajmp  vmnequ    ;16  NEQU    a,b    -  a <> b        ( bitwise comparsion )
            ajmp  vmlt      ;17  LT      a,b    -  a < b         ( integer comparison )
            ajmp  vmgt      ;18  GT      a,b    -  a > b         ( integer comparsion )
            ajmp  vmle      ;19  LE      a,b    -  a <= b        ( integer comparison )
            ajmp  vmge      ;20  GE      a,b    -  a >= b        ( integer comparsion )
            ajmp  vmdm      ;21  DIVMOD	 ia,ib  -  ia DIV ib, ia MOD ib (combined division/modulus)
            ajmp  vmudm     ;22  UDIVMOD ua,ub  -  ua DIV ub, ua MOD ub (unsigned combinded division/modulus)
            ajmp  vmmuld    ;23  MULDIV	 ia,ib,ic - (ia * ib) / ic (multiply to 32 bit & scale)
            ajmp  vmumd     ;24  UMULDIV ua,ub,uc - (ua * ub) / uc (unsigned multiply to 32 bit & scale)
            ajmp  vmzequ    ;25  ZEQU    bl     -  not-bl        ( boolean not (true if tos=0,else false)
            ajmp  vmbra     ;26  BRA     offset  -               ( relative branch (IP = IP+offset))
            ajmp  vmzbra    ;27  ZBRA    flg,off -               ( rel branch if tos = 0)
            ajmp  vmcall    ;28  CALL    addr    -               ( PUSH(RSP) IP, IP = addr)
            ajmp  vmenter   ;29  ENTER   n       -               ( set up stack frame for n bytes of locals)
            ajmp  vmleave   ;30  LEAVE           -               ( unwind stack frame )
            ajmp  vmret     ;31  RET             -               ( POP(RSP) -> IP )
            ajmp  vmnret    ;32  NRET    n       -               ( POP(RSP)->IP, RSP-=N )
            ajmp  vmccall   ;33  CCALL   addr    -               ( call C fn at addr)
            ajmp  vmdup     ;34  DUP     n     -  n,n            ( duplicate TOS )
            ajmp  vmover    ;35  OVER    a,b   -  a,b,a          ( pick 2nd TOS )
            ajmp  vmdrop    ;36  DROP    a     -                 ( drop TOS )
            ajmp  vmpick    ;37  PICK    (..)n -  (..) n-th item ( pick n-TOS )
            ajmp  vmswap    ;38  SWAP    a,b   -  b,a            ( swap TOS and 2-TOS )
            ajmp  vmrepl    ;39  REPL    a,b   -  b              ( replace 2-TOS with TOS )
            ajmp  vmst      ;40  ST      v,a   -                 ( [a] = v)
            ajmp  vmld      ;41  LD      a     -  v              ( v = [a] )
            ajmp  vmcst     ;42  CST     c,a   -                 ( character store)
            ajmp  vmcld     ;43  CLD     a     -  c              ( character fetch)
            ajmp  vmdata    ;44  DATA                            ( skip n bytes,& push original IP)
            ajmp  vmill     ;45  ILLEGAL
            ajmp  vmtor     ;46  TOR     n    -                  ( tos to return stack )
            ajmp  vmfromr   ;47  FROMR        - n                ( return stack to tos )
            ajmp  vmfpadd   ;48  FPADD   n    - FP+n             ( push FP+n )
            ajmp  vmfpld    ;49  FPLD    n    - val              ( return stack[FP+n] ->tos )
            ajmp  vmfpst    ;50  FPST    v,n  -                  ( 2tos -> return stack[FP+n] )
            ajmp  vmill     ;51  ILLEGAL
            ajmp  vmill     ;52  ILLEGAL
            ajmp  vmfpcld   ;53  FPCLD   n    - c                ( return stack[FP+n] -> tos (char load ))
            ajmp  vmfpcst   ;54  FPCST   c,n  -                  ( 2tos -> (char) return stack[FP+n] )
            ajmp  vmhalt    ;55  HALT            -               ( halt execution )
            ajmp  vmfault   ;56  FAULT   n       -               ( raise fault n & halt execution )
            ajmp  vmbnd     ;57  BND     v,l,u - v               ( check l<=v<=u (integer), fault if not )
            ajmp  vmalloc   ;58  ALLOC   n     - a               ( allocates n bytes on RS, returns its addr )
            ajmp  vmmov     ;59  MOV     s,d,n -                 ( move n bytes from source to dest )
            ajmp  vmill     ;60  ILLEGAL
            ajmp  vmill     ;61  ILLEGAL
            ajmp  vmill     ;62  ILLEGAL
            ajmp  vmesc     ;63  ESC     -                       ( prefix to 2 byte opcodes)

;======================================================================
; Support routines
;======================================================================

;======================================================================
; umul32
; Takes 2 16 bit integers pointed to by R0 & R1 (each points to LSB-
; number stored LSB at lowest numbered address (big-endian)).
; 
; 32 bit product left in vpscratc workspace
; 2 16 bit integers, ia & ib  ial,iah & ibl, ibh
; algorithm as for long multiplication
;           vpscratc +  3 2 1 0
; ial * ibl                 B A
; iah * ibl               B A
; ial * ibh               B A
; iah * ibh             B A
;======================================================================
umul32:   mov   A,R0
          push  Acc
          mov   A,R1
          push  Acc

          ; Clear results accumulator
          clr   A
          mov   vpscratc+2,A
          mov   vpscratc+3,A
          ; note bytes 0 & 1 intitalised by ls multiplication

          ; ial * ibl - R0 & R1 already point to LS bytes
          mov   A,@R1
          mov   B,@R0
          mul   AB
          mov   vpscratc+0,A    ; save LS partial product
          mov   vpscratc+1,B  ; and MS partial product

          ; iah * ibl
          inc   R0            ; R0 --> iah

          mov   A,@R1
          mov   B,@R0
          mul   AB
          add   A,vpscratc+1  ; add LS partial product to existing
          mov   vpscratc+1,A
          mov   A,vpscratc+2
          addc  A,B           ; add carry to B, result -> A
          mov   vpscratc+2,A  ; MS partial product
          mov   A,vpscratc+3    
          addc  A,#0
          mov   vpscratc+3,A

          ; ial * ibh
          dec   R0            ; R0 --> ial
          inc   R1            ; R1 --> ibh

          mov   A,@R1
          mov   B,@R0
          mul   AB
          add   A,vpscratc+1  ; add LS partial product to existing
          mov   vpscratc+1,A
          mov   A,vpscratc+2
          addc  A,B           ; add carry to B, result -> A
          mov   vpscratc+2,A  ; MS partial product
          mov   A,vpscratc+3    
          addc  A,#0
          mov   vpscratc+3,A

          ; iah * ibh
          inc   R0            ; R0 --> iah

          mov   A,@R1
          mov   B,@R0
          mul   AB
          add   A,vpscratc+2  ; save LS partial product
          mov   vpscratc+2,A
          mov   A,B
          addc  A,vpscratc+3
          mov   vpscratc+3,A

          pop   Acc
          mov   R1,A
          pop   Acc
          mov   R0,A

          ret

;======================================================================
; div_16
; 32 bit quotient in op_0 to op_3 (ls in op_0, ms in op_3),
; 16 bit divisor in R1:R0 (ms in R1, ls in R0)
; Result in OP
; Remainder in TMP
; Also uses A, R5, R6, R7
;======================================================================
div_16:   mov R7,#0
          mov R6,#0 ; zero out partial remainder
          mov tmp_0, #0
          mov tmp_1, #0
          mov tmp_2, #0
          mov tmp_3, #0
          
          mov R5,#32    ; loop count
div_lp:   acall shiftd  ; Shift the dividend and return MSB in C
          mov A,R6      ; Shift carry into LSB of partial remainder
          rlc A
          mov R6,A
          mov A,R7
          rlc A
          mov R7,A
          ; now test to see if R7:R6 >= R1:R0
          jc cansub     ; Carry out of R7 shift means R7:R6 > R1:R0
          clr C
          mov A,R7      ; subtract R1 from R7 to see if R1 < R7
          subb A,R1     ; A = R7 - R1, carry set if R7 < R1
          jc cantsub
          ; at this point R7>R1 or R7=R1
          jnz cansub    ; jump if R7>R1
          ; if R7 = R1, test for R6>R0
          clr C
          mov A,R6
          subb A,R0     ; A = R6 - R0, carry set if R6 < R0
          jc cantsub
cansub:
          ; subtract divisor from the partial remainder
          clr C
          mov A,R6
          subb A,R0     ; A = R6 - R0
          mov R6,A
          mov A,R7
          subb A,R1     ; A = R7 - R1 - Borrow
          mov R7,A
          setb C        ; shift one into the quotient
          sjmp quot
cantsub:
          ; shift a 0 into the quotient
          clr C
quot:     
          ;shift the carry bit into the quotient
          acall shiftq
          
          ;test for completion
          djnz R5,div_lp

          ; now all done- move TMP values back into OP
          mov op_0,tmp_0
          mov op_1,tmp_1
          mov op_2,tmp_2
          mov op_3,tmp_3
          ; and remainder into TMP
          mov tmp_0,R6
          mov tmp_1,R7
          mov tmp_2,#0
          mov tmp_3,#0
          ret

shiftd:   ;shift the dividend one bit to the left and return MSB in C
          clr C
          mov A,op_0
          rlc A
          mov op_0,A
          mov A,op_1
          rlc A
          mov op_1,A
          mov A,op_2
          rlc A
          mov op_2,A
          mov A,op_3
          rlc A
          mov op_3,A
          ret

shiftq:   ;shift the quotient one bit to the left and shift C into LSB
          mov A,tmp_0
          rlc A
          mov tmp_0,A
          mov A,tmp_1
          rlc A
          mov tmp_1,A
          mov A,tmp_2
          rlc A
          mov tmp_2,A
          mov A,tmp_3
          rlc A
          mov tmp_3,A
          ret

;======================================================================
; vmcmp
; compare tos with 2 to, stack unchanged, 2tos - tos in vpscratc
; a,b - a,b  (a-b -> scratch)
; a>b scratch +ve & != 0
; a<b scratch -ve
; a>=b scratch +ve or 0
; a<=b  scratch -ve or 0
;======================================================================
vmcmp:    mov   A,R1
          mov   R0,A
          dec   R0
          dec   R0
          mov   A,@R0
          clr   C
          subb  A,@R1
          mov   vpscratc,A
          inc   R0
          inc   R1
          mov   A,@R0
          subb  A,@R1
          mov   vpscratc+1,A
          dec   R1
          ret

;======================================================================
; vmm2p
; Makes the top 2 entries on the evaluation stack +ve.  Used for 
; signed multiplication, division and modulus.  The appropriate
; sign information should be retrieved before calling this routine
; so that the sign of the result can be correctly set
;======================================================================
vmm2p:	  inc R1
		  mov A,@R1
		  dec R1
		  jnb Acc.7,vmm2p0 ; skip if ib is +ve or 0
		  mov A,#0
		  clr C
		  subb A,@R1
		  mov @R1,A
		  inc R1
		  mov A,#0
		  subb A,@R1
		  mov  @R1,A
		  dec R1

vmm2p0:	  dec R1			; to point to ms byte of lower word
		  mov A,@R1
		  jnb Acc.7,vmm2p1 ; skip if ia is +ve or 0
		  dec R1
		  mov A,#0
		  clr C
		  subb A,@R1
		  mov @R1,A
		  inc R1
		  mov A,#0
		  subb A,@R1
		  mov  @R1,A
		
vmm2p1:	  inc R1			; point back to ls byte of upper word
		  ret	


;======================================================================
; vminvn
; negate TOS if sign flag is set.  Used for signed multiplication,
; division and modulus.  These are done as unsigned after the sign
; of the result is worked out and the sign bit set appropriately
;======================================================================
vminvn:	  ; invert sign of result if sign bit set.
		  jnb vmsign,vmninv ; skip if sign bit clear
		  mov A,#0
		  clr C
		  subb A,@R1
		  mov @R1,A
		  inc R1
		  mov A,#0
		  subb A,@R1
		  mov @R1,A
		  dec R1

vmninv:   ret

;======================================================================
; Virtual Machine Instruction Implementations
;======================================================================

;======================================================================
; ILLEGAL
; Illegal instruction.
;======================================================================
vmill:    mov vmfltc,#vIlleg      ; illegal instruction fault
          setb vmfStop             ; set stop flag
          ret

;======================================================================
; NOP
; No-operation
;======================================================================
vmnop:    ret

;======================================================================
; ADD     
; ia,ib  -  ia + ib       ( integer addition)
; enter with R1 pointing to lsb of ib (ibl)
;======================================================================
vmadd:    mov   A,R1    ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          mov   A,@R1
          add   A,@R0
          mov   @R1,A   ; Replace ial
          inc   R1      ; R1 --> iah
          inc   R0      ; R0 --> ibh
          mov   A,@R1
          addc  A,@R0
          mov   @R1,A   ; Replace iah
          dec   R1
          ret

;======================================================================
; SUB     
; ia,ib  -  ia - ib       ( integer subtraction)
; 2tos - tos
; ibh     
; ibl     <-- R1      R0
; iah             ==>
; ial                 R1
;======================================================================
vmsub:    mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          clr   C
          mov   A,@R1
          subb  A,@R0
          mov   @R1,A   ; Replace ial
          inc   R1      ; R1 --> iah
          inc   R0      ; R0 --> ibh
          mov   A,@R1
          subb  A,@R0
          mov   @R1,A   ; Replace iah
          dec   R1      ; point back to lsb of result
          ret
;======================================================================
; MULT    
; ia,ib  -  ia * ib       ( integer multiplication)
; ibh     
; ibl     <-- R1
; iah     
; ial
; 
;======================================================================
vmmult:   ; Capture sign information - xor sign bits of both words
		  inc R1
		  mov A,@R1 ; ibh
		  dec R1
		  dec R1
		  xrl A,@R1	; iah - if same sign bit 7 = 0
		  rlc A		; sign to carry
		  mov vmsign,C
		  inc R1

		  acall vmm2p		; make sure operands +ve
		  acall vmumul		; do unsigned multiplcation

		  ajmp vminvn		; invert result if needed

;======================================================================
; UMUL    
; ua,ub  -  ua * ub       ( unsigned integer multiplication)
;======================================================================
vmumul:   mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          
          acall umul32  ; result in vpscratc

          mov A,vpscratc    ; lsb
          mov @R1,A
          mov A,vpscratc+1  ; msb
          dec R0            ; pt to iah
          mov @R0,A

          ret

;======================================================================
; DIV     
; ia,ib  -  ia / ib       ( integer division)
;======================================================================
vmdiv:    ; Capture sign information - xor sign bits of both words
		  inc R1
		  mov A,@R1 ; ibh
		  dec R1
		  dec R1
		  xrl A,@R1	; iah - if same sign bit 7 = 0
		  rlc A		; sign to carry
		  mov vmsign,C
		  inc R1

		  acall vmm2p		; make sure operands +ve
		  acall vmudiv		; do unsigned division

		  ajmp vminvn		; invert sign bit if needed

;======================================================================
; UDIV     
; ua,ub  -  ua / ub       ( unsigned integer division)
;======================================================================
vmudiv:   mov A,R5
		  push Acc
		  mov A,R6
		  push Acc
		  mov A,R7
		  push Acc

		  mov A,R1
          push Acc

		  ; Load dividend
          mov op_3,#0
          mov op_2,#0
		  mov A,R1
          mov R0,A
		  dec R0
		  mov op_1,@R0
		  dec R0
		  mov op_0,@R0

		  ; Load divisor
		  mov A,R1
		  mov R0,A
		  inc R1
		  mov A,@R1
		  mov R1,A
		  mov A,@R0
		  mov R0,A

		  acall div_16

		  pop Acc
		  mov R1,Acc
		
		  ;get quotient
          dec R1
		  mov @R1, op_1
		  dec R1
		  mov @R1, op_0

		  ; restore registers
		  pop Acc
		  mov R7,A
		  pop Acc
		  mov R6,A
		  pop Acc
		  mov R5,A
          ret

;======================================================================
; MOD     
; ia,ib  -  ia MOD ib     ( integer modulus)
; result takes sign of dividend (ia)
;======================================================================
vmmod:    ; Capture sign information 
		  dec R1
		  mov A,@R1
		  rlc A		; sign to carry
		  mov vmsign,C
		  inc R1

		  acall vmm2p		; make sure operands +ve
		  acall vmumod		; do unsigned multiplcation

		  ajmp vminvn		; invert sign bit if needed

;======================================================================
; UMOD     
; ua,ub  -  ua UMOD ub    ( unsigned integer modulus)
;======================================================================
vmumod:   mov A,R5
		  push Acc
		  mov A,R6
		  push Acc
		  mov A,R7
		  push Acc

		  mov A,R1
          push Acc

		  ; Load dividend
          mov op_3,#0
          mov op_2,#0
		  mov A,R1
          mov R0,A
		  dec R0
		  mov op_1,@R0
		  dec R0
		  mov op_0,@R0

		  ; Load divisor
		  mov A,R1
		  mov R0,A
		  inc R1
		  mov A,@R1
		  mov R1,A
		  mov A,@R0
		  mov R0,A

		  acall div_16

		  pop Acc
		  mov R1,Acc
		
		  ;get remainder
          dec R1
		  mov @R1, tmp_1
		  dec R1
		  mov @R1, tmp_0

		  ; restore registers
		  pop Acc
		  mov R7,A
		  pop Acc
		  mov R6,A
		  pop Acc
		  mov R5,A
          ret

;======================================================================
; INC a - a+1
;======================================================================
vminc:	  mov   A,@R1
		  add	A,#1
		  mov   @R1,A
		  inc   R1
		  mov   A,@R1
		  addc	A,#0
		  mov   @R1,A
		  dec   R1
		  ret

;======================================================================
; DEC a - a-1
;======================================================================
vmdec:	  clr   C
		  mov   A,@R1
		  subb	A,#1
		  mov   @R1,A
		  inc   R1
		  mov   A,@R1
		  subb	A,#0
		  mov   @R1,A
		  dec   R1
		  ret

;======================================================================
; AND     
; ia,ib  -  ia AND ib     ( bitwise and)
;======================================================================
vmand:    mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          mov   A,@R1
          anl   A,@R0
          mov   @R1,A   ; Replace ial
          inc   R1      ; R1 --> iah
          inc   R0      ; R0 --> ibh
          mov   A,@R1
          anl   A,@R0
          mov   @R1,A   ; Replace iah
          dec   R1
          ret
;======================================================================
; OR      
; ia,ib  -  ia OR ib      ( bitwise or)
;======================================================================
vmor:     mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          mov   A,@R1
          orl   A,@R0
          mov   @R1,A   ; Replace ial
          inc   R1      ; R1 --> iah
          inc   R0      ; R0 --> ibh
          mov   A,@R1
          orl   A,@R0
          mov   @R1,A   ; Replace iah
          dec   R1
          ret
;======================================================================
; NOT     
; ia     -  NOT ia        ( 1s complement)
;======================================================================
vmnot:    mov A,r1
          mov R0,A
          
          mov A,@R0
          cpl A
          mov @R0,A

          inc R0

          mov A,@R0
          cpl A
          mov @R0,A

          ret          

;======================================================================
; EQU     a,b    -  a = b         ( bitwise comparison )
;======================================================================
vmequ:    mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          mov   A,@R1
          xrl   A,@R0
          mov   B,A
          inc   R0
          inc   R1
          mov   A,@R1
          xrl   A,@R0
          orl   A,B     ; if both words were equal: A now 0.
          mov   B,#0    ; false
          jnz   vmequt  ; jump if not the same (put false)
          mov   B,#0ffh; ; true
vmequt:   mov   @R1,B
          dec   R1
          mov   @R1,B
          ret

;======================================================================
; NEQU    a,b    -  a <> b        ( bitwise comparsion )
;======================================================================
vmnequ:   mov   A,R1   ; R0 --> ibl
          mov   R0,A
          dec   R1      ; R1 --> iah
          dec   R1      ; R1 --> ial
          mov   A,@R1
          xrl   A,@R0
          mov   B,A
          inc   R0
          inc   R1
          mov   A,@R1
          xrl   A,@R0
          orl   A,B     ; if both words were equal: A now 0.
          mov   B,#0    ; false
          jz   vmnequf  ; jump if the same (put false)
          mov   B,#0ffh; ; true
vmnequf:  mov   @R1,B
          dec   R1
          mov   @R1,B
          ret

;======================================================================
; LT      a,b    -  a < b         ( integer comparison )
; If a<b then a-b should be -ve.
; Hence if sign bit of (a-b) result is set, return true, else return
; false.
;======================================================================
vmlt:     acall vmcmp
          mov   B,#0            ; default result of false
          mov   A,vpscratc+1    ; ms byte of result
          jnb   ACC.7,vmltf     ; skip if false
          mov   B,#0FFh
vmltf:    mov   A,B
		  dec   R1
          mov   @R1,A
          dec   R1
          mov   @R1,A
          ret

;======================================================================
; GT      a,b    -  a > b         ( integer comparsion )
; if a>b then a-b is +ve & non-zero
;======================================================================
vmgt:     acall vmcmp
          mov   B,#0            ; default result of false
          mov   A,vpscratc
          orl   A,vpscratc+1    ; zero if result is zero
          jz    vmgtf           ; return false if zero
          mov   A,vpscratc+1    ; ms byte of result
          jb   ACC.7,vmgtf      ; false if sign bit is set
          mov   B,#0FFh
vmgtf:    mov   A,B
		  dec   R1
          mov   @R1,A
          dec   R1
          mov   @R1,A
          ret

;======================================================================
; LE      a,b    -  a <= b        ( integer comparison )
; if a<=b then a-b is -ve or zero
; Note- this is the same as vmgt with the values of true & false
; swapped over as a<=b == !(a>b)
;======================================================================
vmle:     acall vmcmp
          mov   B,#0FFh         ; default result of true
          mov   A,vpscratc
          orl   A,vpscratc+1    ; zero if result is zero
          jz    vmlet           ; return true if zero
          mov   A,vpscratc+1    ; ms byte of result
          jb   ACC.7,vmlet      ; true if sign bit is set
          mov   B,#0            ; set false
vmlet:    mov   A,B
          dec   R1
          mov   @R1,A
          dec   R1
          mov   @R1,A
          ret

;======================================================================
; GE      a,b    -  a >= b        ( integer comparsion )
; if a>=b then a-b is +ve or zero (sign bit not set)
;======================================================================
vmge:     acall vmcmp
          mov   B,#0            ; default result of false
          mov   A,vpscratc+1    ; ms byte of result
          jb    ACC.7,vmgef     ; if -ve is false
          mov   B,#0FFh
vmgef:    mov   A,B
          dec   R1
          mov   @R1,A
          dec   R1
          mov   @R1,A
          ret

;======================================================================
; DIVMOD	 ia,ib  -  ia DIV ib, ia MOD ib (combined division/modulus)
;======================================================================
vmdm:   
		; capture sign for div
		inc R1
		mov A,@R1 ; ibh
		dec R1
		dec R1
		xrl A,@R1	; iah - if same sign bit 7 = 0
		rlc A		; sign to carry
		mov vmsgn2,C

		; capture sign for mod
		mov A,@R1
		rlc A		; sign to carry
		mov vmsign,C
		inc R1

		acall vmm2p 	; make operands +ve
		acall vmudm		; do unsigned div-mod
		acall vminvn	; invert modulus if vmsign set

		; invert quotient if vmsgn2 set
		dec R1
		dec R1
		mov C,vmsgn2
		mov vmsign,C
		acall vminvn
		inc R1
		inc R1

		ret


;======================================================================
; UDIVMOD ua,ub  -  ua DIV ub, ua MOD ub (unsigned combinded division/modulus)
;======================================================================
vmudm:    mov A,R5
		  push Acc
		  mov A,R6
		  push Acc
		  mov A,R7
		  push Acc

		  mov A,R1
          push Acc

		  ; Load dividend
          mov op_3,#0
          mov op_2,#0
		  mov A,R1
          mov R0,A
		  dec R0
		  mov op_1,@R0
		  dec R0
		  mov op_0,@R0

		  ; Load divisor
		  mov A,R1
		  mov R0,A
		  inc R1
		  mov A,@R1
		  mov R1,A
		  mov A,@R0
		  mov R0,A

		  acall div_16

		  pop Acc
		  mov R1,Acc

		  mov R0,Acc
		  inc R0    ; point to msb of dest for remainder

		  ;get remainder
		  mov @R0, tmp_1
		  dec R0
		  mov @R0, tmp_0

		  ;get quotient
          dec R0
		  mov @R0, op_1
		  dec R0
		  mov @R0, op_0

		  ; restore registers
		  pop Acc
		  mov R7,A
		  pop Acc
		  mov R6,A
		  pop Acc
		  mov R5,A
          ret

;======================================================================
; MULDIV	 ia,ib,ic - (ia * ib) / ic (multiply to 32 bit & scale)
;======================================================================
vmmuld: mov A,r1
		mov r0,A

		mov A,@R0

		dec R0
		dec R0
		xrl A,@R0

		dec R0
		dec R0
		xrl A,@R0

		;-WIP HERE TODO!
		acall vmumd
		ret

;======================================================================
; UMULDIV ua,ub,uc - (ua * ub) / uc (unsigned multiply to 32 bit & scale)
;======================================================================
vmumd:     
		mov A,R5
		push Acc
		mov A,R6
		push Acc
		mov A,R7
		push Acc

		dec R1			; ignore divisor for the time being
		dec R1

		mov A,R1
		push Acc

		; Do the 16x16 mult to vpscratc
		mov   A,R1   
		mov   R0,A
		dec   R0      
		dec   R0      
		acall umul32  ; (R0) * (R1) -> result in vpscratc


		; 16 bit divisor in R1:R0 (ms in R1, ls in R0)
		inc R1
		inc R1
		mov A,@R1
		mov R0,A
		inc R1
		mov A,@R1
		mov R1,A

		acall div_16

		pop Acc
		mov R1,Acc

		;get quotient
		dec R1
		mov @R1, op_1
		dec R1
		mov @R1, op_0

		; restore registers
		pop Acc
		mov R7,A
		pop Acc
		mov R6,A
		pop Acc
		mov R5,A
		ret



		  
;======================================================================
; ZEQU    bl     -  not-bl        ( boolean not (true if tos=0,else false)
;======================================================================
vmzequ:   mov   A,@R1
          inc   R1
          orl   A,@R1
          mov   B,#0FFH
          jz    vmzeqt
          mov   B,#0
vmzeqt:   mov   @R1,B
          dec   R1
          mov   @R1,B
          ret

;======================================================================
; BRA     offset  -               ( relative branch (IP = IP+offset))
;======================================================================
vmbra:    mov   A,@R1   ; lsb of offset
          add   A,R6    ; add lsb of IP
          mov   R6,A
          inc   R1
          mov   A,@R1
          addc  A,R7    ; add msb of IP
          mov   R7,A
          dec   R1
          dec   R1
          dec   R1
          ret

;======================================================================
; ZBRA    flg,off -               ( rel branch if tos = 0)
;======================================================================
vmzbra:   mov A,R1
          mov R0,A
          dec R0
          mov A,@R0
          dec R0
          orl A,@R0     ; A=0 iff TOS= 0;
          jnz vmzbrf    ; skiip if not 0

          ; Add TOS to IP
          mov   A,@R1   ; lsb of offset
          add   A,R6    ; add lsb of IP
          mov   R6,A
          inc   R1
          mov   A,@R1
          addc  A,R7    ; add msb of IP
          mov   R7,A

vmzbrf:   dec R0
          dec R0
          mov A,R0
          mov R1,A
          ret

;======================================================================
; CALL    addr    -               ( PUSH(RSP) IP, IP = addr)
;======================================================================
vmcall:   mov   A,R4    ; RSP Low
          add   A,#2    ; Bump RSP for return addr
          mov   R4,A
          mov   dpl,A
          mov   A,R5
          addc  A,#0
          mov   R5,A
          mov   dph,A
          ; RSP now incremented & dptr pts to where
          ; return address should go. - write IP
          ; to RSP
          mov  a,R6
          movx @dptr,A
          inc  dptr
          mov  a,R7
          movx @dptr,A

          ; Now load up the call address into IP.
          mov   A,@R1 
          mov   R6,A
          inc   R1
          mov   A,@R1
          mov   R7,A
          dec   R1
          dec   R1
          dec   R1

          ret

;======================================================================
; ENTER   n       -               ( set up stack frame for n bytes of locals)
;======================================================================
vmenter:  mov   A,R4      ; Get rsp -> dptr
          mov   dpl,A
          mov   A,R5
          mov   dph,A

          inc   dptr      ; bump RSP (pre-increment)
          inc   dptr

          mov   A,dpl     ; save new stack ptr
          mov   R4,A
          mov   A,dph
          mov   R5,A

          mov   A,R2      ; write FP to stack
          movx  @dptr,A
          inc   dptr
          mov   A,R3
          movx  @dptr,A

          ; RSP->FP
          mov   A,R4
          mov   R2,A
          mov   A,R5
          mov   R3,A

          ; RSP+=N
          mov   A,R4
          add   A,@R1
          mov   R4,A
          inc   R1
          mov   A,R5
          addc  A,@R1
          mov   R5,A
          dec   R1
          dec   R1
          dec   R1

          ret

;======================================================================
; LEAVE           -               ( unwind stack frame )
;======================================================================
vmleave:  mov   A,R2      ; FP->RSP
          mov   R4,A
          mov   A,R3
          mov   R5,A

          ; POP FP (leaving dptr pointing to old TOS)
          mov   A,R4
          mov   dpl,A
          clr   C
          subb  A,#2
          mov   R4,A

          mov   A,R5
          mov   dph,A
          subb  A,#0
          mov   R5,A

          movx  A,@dptr ; get old FP
          mov   R2,A
          inc   dptr
          movx  A,@dptr
          mov   R3,A

          ret

;======================================================================
; RET             -               ( POP(RSP) -> IP )
;======================================================================
vmret:    mov   A,R4      ; RSP low
          mov   dpl,A
          clr   C     
          subb  A,#2
          mov   R4,A
          mov   A,R5      ; RSP high
          mov   dph,A
          subb  A,#0
          mov   R5,A
          ; RSP is decremented by 2 bytes, old tos (return addr)
          ; is now pointed to by DPTR
          movx  A,@dptr   ; return address ls byte
          mov   R6,A      ; stuff in IP low
          inc   dptr
          movx  A,@dptr   ; return address ms byte
          mov   R7,A      ; stuff in IP high

          ret

;======================================================================
; NRET    n       -               ( POP(RSP)->IP, RSP-=N )
;======================================================================
vmnret:   mov   A,R4      ; RSP low
          mov   dpl,A
          clr   C     
          subb  A,#2
          mov   R4,A
          mov   A,R5      ; RSP high
          mov   dph,A
          subb  A,#0
          mov   R5,A

          ; Subtract N from RSP
          mov   A,R4      ; RSP low
          clr   C
          subb  A,@R1
          mov   R4,A
          inc   R1   
          mov   A,R5      ; RSP high
          subb  A,@R1
          mov   R5,A
          dec   R1        ; drop
          dec   R1
          dec   R1

          ; RSP is decremented by 2 bytes + N, old tos (return addr)
          ; is now pointed to by DPTR
          movx  A,@dptr   ; return address ls byte
          mov   R6,A      ; stuff in IP low
          inc   dptr
          movx  A,@dptr   ; return address ms byte
          mov   R7,A      ; stuff in IP high

          ret

;======================================================================
; CCALL   addr    -               ( call C fn at addr)
;======================================================================
vmccall:  mov   dpl,@R1
          inc   R1
          mov   dph,@R1
          dec   R1
          dec   R1
          dec   R1
          clr   A
          jmp     @a+dptr     ; jump to table - use exiting return addr

;======================================================================
; DUP     n     -  n,n            ( duplicate TOS )
;======================================================================
vmdup:    mov  A,R1
          mov  R0,A
          inc  R0
          inc  R0
          mov  A,@R1
          mov  @R0,A
          inc  R0
          inc  R1
          mov  A,@R1
          mov  @R0,A
          inc  R1
          ret

;======================================================================
; OVER    a,b   -  a,b,a          ( pick 2nd TOS )
;======================================================================
vmover:   mov  A,R1
          mov  R0,A
          dec  R0     ; R0 -> 2TOS
          dec  R0
          inc  R1     ; R1 -> TOS+1
          inc  R1
          mov  A,@R0
          mov  @R1,A
          inc  R0
          inc  R1
          mov  A,@R0
          mov  @R1,A
          dec  R1
          ret

;======================================================================
; DROP    a     -                 ( drop TOS )
;======================================================================
vmdrop:   dec   R1
          dec   R1
          ret

;======================================================================
; PICK    (..)n -  (..) n-th item ( pick n-TOS )
; Note cheat - high byte of n is ignored as SP is only 8-bit anyway.
;======================================================================
vmpick:   mov   A,@R1
          add   A,@R1   ; 2 * n
          mov   B,A
          mov   A,R1
          clr   C
          subb  A,B     ; subtract offset from ESP
          mov   R0,A

          ; copy the word from n-TOS to TOS
          mov   A,@R0
          mov   @R1,A
          inc   R0
          inc   R1
          mov   A,@R0
          mov   @R1,A
          dec   R1

          ret

;======================================================================
; SWAP    a,b   -  b,a            ( swap TOS and 2-TOS )
;======================================================================
vmswap:   mov   A,R1
          mov   R0,A
          dec   R0    ; R0 -> 2TOS
          dec   R0
          
          mov   A,@R0     ; swap ls byte
          xch   A,@R1
		  mov   @R0,A

          inc   R0
          inc   R1

          mov   A,@R0     ; swap ms byte
          xch   A,@R1
		  mov   @R0,A

          dec   R1
          ret

;======================================================================
; REPL    a,b   -  b              ( replace 2-TOS with TOS )
;======================================================================
vmrepl:   mov   A,R1
          mov   R0,A
          dec   R1   
          inc   R0

          mov   A,@R0
          mov   @R1,A

          dec   R0
          dec   R1

          mov   A,@R0
          mov   @R1,A
          ret

;======================================================================
; ST      v,a   -                 ( [a] = v)
;======================================================================
vmst:     mov   dpl,@R1
          inc   R1
          mov   dph,@R1
          dec   R1

          dec   R1
          dec   R1

          mov   A,@R1
          movx  @dptr,A
          inc   dptr
          inc   R1
          mov   A,@R1
          movx  @dptr,A
          dec   R1

		  dec   R1
		  dec   R1

          ret

;======================================================================
; LD      a     -  v              ( v = [a] )
;======================================================================
vmld:     mov   dpl,@R1
          inc   R1
          mov   dph,@R1
          dec   R1
          
          movx  A,@dptr
          mov   @R1,A
          
          inc   R1
          inc   dptr

          movx  A,@dptr
          mov   @R1,A

          dec   R1
          ret

;======================================================================
; CST     c,a   -                 ( character store)
;======================================================================
vmcst:    mov   dpl,@R1
          inc   R1
          mov   dph,@R1
          dec   R1

          dec   R1          ; pt to char
          dec   R1

          mov   A,@R1       ; store char
          movx  @dptr,A

          dec   R1          ; drop char
          dec   R1

          ret

;======================================================================
; CLD     a     -  c              ( character fetch)
;======================================================================
vmcld:    mov   dpl,@R1
          inc   R1
          mov   dph,@R1
          mov   @R1,#0      ; zero ms byte of result
          dec   R1
          
          movx  A,@dptr     ; get & store char result
          mov   @R1,A

          ret

;======================================================================
; DATA    n     -  a              ( skip n bytes,& push original IP)
; Use for setting up inline data.
;======================================================================
vmdata:   mov   dpl,R6  ; IP low - save original IP - where data is
          mov   dph,R7  ; IP low
          mov   A,@R1
          add   A,R6    ; ls IP+N
          mov   R6,A
          
          mov   A,dpl   ; replace ls N with ls Addr
          mov   @R1,A

          inc   R1

          mov   A,@R1   ; ms IP+N
          addc  A,R7
          mov   R7,A

          mov   A,dph	; replace MS n with MS addr
          mov   @R1,A

          dec   R1
          ret



;======================================================================
; TOR     n    -                  ( tos to return stack )
;======================================================================
vmtor:    mov   A,R4    ; RS low
          add   A,#2
          mov   dpl,A
          mov   R4,A
          mov   A,R5    ; RS high
          addc  A,#0
          mov   dph,A
          mov   R5,A

          mov   A,@R1
          movx  @dptr,A

          inc   R1
          inc   dptr

          mov   A,@R1
          movx  @dptr,A

          dec   R1
          dec   R1
          dec   R1

          ret

;======================================================================
; FROMR        - n                ( return stack to tos )
;======================================================================
vmfromr:  mov   dpl,R4      ;load RSP to dptr
          mov   dph,R5      
          
          inc   R1          ; pt R1 where new TOS will go
          inc   R1

          ; mov (RSP) to (TOS)
          movx  A,@dptr
          mov   @R1,A
          inc   R1
          inc   dptr
          movx  A,@dptr
          mov   @R1,A
          dec   R1

          ; drop RSP by 2 bytes
          mov   A,R4
          clr   C
          subb  A,#2
          mov   R4,A
          mov   A,R5
          subb  A,#0
          mov   R5,A

          ret

;======================================================================
; FPADD   n    - FP+n             ( push FP+n )
;======================================================================
vmfpadd:  mov   A,@R1 
          add   A,R2      ; FP low
          mov   @R1,A

          inc   R1
          mov   A,@R1
          addc  A,R3      ; FP high
          mov   @R1,A

          dec   R1
          ret

;======================================================================
; FPLD    n    - val              ( return stack[FP+n] ->tos )
;======================================================================
vmfpld:   mov   A,R2      ; FP low
          add   A,@R1     ; + n low
          mov   dpl,A     
          inc   R1
          mov   A,R3
          addc  A,@R1
          mov   dph,A
          dec   R1        

          ; now FP+n is in dptr, move data to tos
          movx  A,@dptr
          mov   @R1,A
          
          inc   R1
          inc   dptr

          movx  A,@dptr
          mov   @R1,A
          
          dec   R1
          ret

;======================================================================
; FPST    v,n  -                  ( 2tos -> return stack[FP+n] )
;======================================================================
vmfpst:   mov   A,R2      ; FP low
          add   A,@R1     ; + n low
          mov   dpl,A     
          inc   R1
          mov   A,R3
          addc  A,@R1
          mov   dph,A
          dec   R1        
          
          dec   R1
          dec   R1
          mov   A,@R1
          movx  @dptr,A
          inc   R1
          inc   dptr
          mov   A,@R1
          movx  @dptr,A
          dec   R1

		  dec   R1
		  dec   R1
          ret

;======================================================================
; FPCLD   n    - c                ( return stack[FP+n] -> tos (char load ))
;======================================================================
vmfpcld:  mov   A,R2      ; FP low
          add   A,@R1     ; + n low
          mov   dpl,A     
          inc   R1
          mov   A,R3
          addc  A,@R1
          mov   dph,A
          mov   @R1,#0    ; set MSB of char to 16 bit.
          dec   R1        

          ; now FP+n is in dptr, move char data to tos
          movx  A,@dptr
          mov   @R1,A
          
          ret

;======================================================================
; FPCST   c,n  -                  ( 2tos -> (char) return stack[FP+n] )
;======================================================================
vmfpcst:  mov   A,R2      ; FP low
          add   A,@R1     ; + n low
          mov   dpl,A     
          inc   R1
          mov   A,R3
          addc  A,@R1
          mov   dph,A
          dec   R1        
          
          dec   R1
          dec   R1

          mov   A,@R1
          movx  @dptr,A
         
		  dec   R1
		  dec   R1
          ret

;======================================================================
; HALT            -               ( halt execution )
;======================================================================
vmhalt:   mov vmfltc,#vHalt       ; halt fault
          setb vmfstop            ; set stop flag
          ret

;======================================================================
; FAULT   n       -               ( raise fault n & halt execution )
;======================================================================
vmfault:  mov   A,@R1             ; get (byte) fault code
		  dec   R1
          dec   R1
          mov vmfltc,A            ;
          setb vmfstop            ; set stop flag
          ret

;======================================================================
; BND     v,l,u - v               ( check l<=v<=u (integer), fault if not )
;======================================================================
vmbnd:    mov   A,R1				; point r0 at u
		  mov   R0,A
		  
		  dec   R1					; point R1 at v
          dec   R1
          dec   R1
          dec   R1

		  ; R1 points to v, R0 to u
		  ; if u-v is -ve then fault
		  clr 	C
		  mov 	A,@R0			   ; u
		  subb	A,@R1			   ; - v
		  inc	R0				   ; to MS bytes
		  inc 	R1
		  mov   A,@R0			   ; u
		  subb	A,@R1			   ; - v
	      dec	R0
		  dec   R1
		  jb Acc.7,vmbndf			; -ve if bit 7 set from v > u

		  dec	R0				   ; point R0 at l
		  dec   R0

		  ; R1 points to v, R0 to l
		  ; if v-l is -ve then fault
		  clr 	C
		  mov 	A,@R1			   ; v
		  subb	A,@R0			   ; - l
		  inc	R0
		  inc 	R1
		  mov   A,@R1
		  subb	A,@R0
		  dec   R1
		  jb Acc.7,vmbndf		   ; -ve if l > v
		  
		  ret	; no fault

vmbndf:	  mov 	vmfltc,#vBound
		  setb 	vmfStop
          ret

;======================================================================
; ALLOC   n     - a               ( allocates n bytes on RS, returns its addr )
;======================================================================
vmalloc:  mov   A,R4    ; RSP low
          mov   dpl,A   ; save temp copy
          add   A,@R1   ; bump RSP
          mov   R4,A
          
          inc   R1
          mov   A,R5
          mov   dph,A
          addc  A,@R1
          mov   R5,A

		  inc   dptr	; RSP points to top element on stack so we need
		  inc   dptr	; to skip over that to return correct address

          mov   @R1,dph
          dec   R1
          mov   @R1,dpl

          ret

;======================================================================
; MOV     s,d,n -                 ( move n bytes from source to dest )
; Uses source pointer in vpstratc + 0 & +1
; Dest pointer in vpscratch +2 & +3
; Byte count in vpscratc +4 & +5
;======================================================================
vmmov:    inc   R1            ; n -> vpscratc + 4
          mov   vpscratc+5,@R1
          dec   R1
          mov   vpscratc+4,@R1
		  dec   R1

          mov   vpscratc+3,@R1 ; d -> vpscratc + 2
          dec   R1
          mov   vpscratc+2,@R1
		  dec   R1

          mov   vpscratc+1,@R1  ; s -> vpscratc + 0
          dec   R1
          mov   vpscratc+0,@R1
		  dec   R1

		  dec   R1

vmmovlp:  mov   A,vpscratc+4
          orl   A,vpscratc+5
          jz    vmmove

          ;Get a byte from source & bump source addr
          mov   dpl,vpscratc+0
          mov   dph,vpscratc+1
          movx  A,@dptr
          inc   dptr
          mov   vpscratc+0,dpl
          mov   vpscratc+1,dph

          ;Write a byte to dest & bump dest addr
          mov   dpl,vpscratc+2
          mov   dph,vpscratc+3
          movx  @dptr,A
          inc   dptr
          mov   vpscratc+2,dpl
          mov   vpscratc+3,dph

          ;Decrement counter
          mov   A,vpscratc+4
          clr   C
          subb  A,#1
          mov   vpscratc+4,A
          mov   A,vpscratc+5
          subb  A,#0
          mov   vpscratc+5,A

          sjmp  vmmovlp
vmmove:   ret

;======================================================================
; ESC     -                       ( prefix to 2 byte opcodes)
;======================================================================
vmesc:    mov   dpl,R6          ; load IP into dptr
          mov   dph,R7
          movx  A,@dptr         ; get opcode
          inc   dptr            ; ++IP
          mov   R6,dpl          ; save IP
          mov   R7,dph

          ljmp  vxDisp          ; execute extension


