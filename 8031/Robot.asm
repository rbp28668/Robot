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
;======================================================================
; MONITOR.ASM
; Machine code monitor main program
;
; Resources:
; T0 - main timing interrupt.
; T1 - baud rate generator.
; P3.2:P3.5 - Dual I2C master.
;======================================================================

;======================================================================
; Define interrupt vectors at start of dual mapped ram
;======================================================================
            .equ rambase,0000h      ; ToDO - figure out how this works at start of dual mapped ram, below this EPROM or Data RAM.  8000 gives error 201 - overflow.
            .equ intIE0,rambase+32
            .equ intTF0,rambase+64
            .equ intIE1,rambase+96
            .equ intTF1,rambase+128
            .equ intCom,rambase+160
            .equ intTF2,rambase+192
            .equ user,rambase+256

;======================================================================
;-- Reset & interrupt vectors
;======================================================================
            .org    0000h
            sjmp    monitor     ;jump over interrupt vectors
            .org    0003h       ;IE0 external interrupt 0 vector
            ljmp    intIE0
            .org    000Bh       ;TF0 timer/counter 0 interrupt vector
            ljmp    timeint     ;direct to main timing interrupt
            .org    0013h       ;IE1 external interrupt 1 vector
            ljmp    intIE1
            .org    001Bh       ;TF1 timer/counter 1 interrupt vector
            ljmp    intTF1
            .org    0023h       ;serial interrupt vector.
            ljmp    intCom
            .org    002Bh       ;T2
            ljmp    intTF2



;======================================================================
;-- Hardware equates
;======================================================================

;======================================================================
; Bit addresses of i2c lines, 2 software I2C masters, I2C-A and I2CB
;======================================================================
                .equ sda_a,P3.3         ; bit address of I2C-A SDA
                .equ scl_a,P3.2         ; bit address of I2C-A SCL
                .equ sda_b,P3.5         ; bit address of I2C-B SDA
                .equ scl_b,P3.4         ; bit address of I2C-B SCL


;======================================================================
;-- Start of main user program
;-- Runs the simple monitor program.
;======================================================================
monitor:    mov sp, #7Fh        ; 8032: start stack at start of top 128 bytes
                                ; 8031: start stack at #2Fh just above bit addressable area
            mov     p1,#0       ; Make sure motors off.

            ; clear internal memory to 0 (ignore stack area)
            mov     r0,#08      ; start above first register bank
            mov     r1,#120     ; for first 128 bytes less first register bank
mclrlp:     mov     @r0,#0      ; clear byte
            inc     r0          ; next
            djnz    r1,mclrlp

            ; init the hardware
            lcall   timinit     ; set timer interrupt
            lcall   scominit    ; init the serial comms

            ;Output logon message
            mov     dptr,#logmsg1
            acall   scomtxc
            mov     dptr,#logmsg2
            acall   scomtxc
            mov     dptr,#logmsg3
            acall   scomtxc


;-- monitor main loop
monloop:    mov     dptr,#rdymsg ; ready prompt
            acall   scomtxc
            acall   scomrxa     ; look for command character
            cjne    a,#':',monnorm ; a normal character if not a :
            acall   ihexinlp    ; after initial : has been read
            sjmp    monloop

monnorm:    clr     c           ; for sub
            subb    a,#'@'      ; map @->0, A->1 etc
            anl     a,#31       ; force into range 0..31
            rl      a           ; 2 bytes per table entry
            mov     dptr,#monloop ; set up return address
            push    dpl         ; push low byte of return address
            push    dph         ; push high byte of return address
            mov     dptr,#monjmp ; pointer to monitor jump table
            jmp     @a+dptr     ; jump to table

;-- vectored jump table to monitor commands            
monjmp:     ajmp   notimp      ; @
            ajmp   notimp      ; A
            ajmp   notimp      ; B
            ajmp   monrdcod    ; C - read code byte
            ajmp   notimp      ; D
            ajmp   echo        ; E - echo characters (until .)
            ajmp   notimp      ; F
            ajmp   notimp      ; G
            ajmp   notimp      ; H
            ajmp   ihexin      ; I - read intelhex file into data memory
            ajmp   vjump       ; J - jump to location
            ajmp   notimp      ; K
            ajmp   notimp      ; L
            ajmp   notimp      ; M
            ajmp   notimp      ; N
            ajmp   monrdmem    ; 0 - read from internal memory
            ajmp   monwrmem    ; P - write to intenral memory
            ajmp   notimp      ; Q
            ajmp   notimp      ; R
            ajmp   monrdsfr    ; S - SFR read aa
            ajmp   monwrsfr    ; T - SFR write aa,dd
            ajmp   notimp      ; U
            ajmp   notimp      ; V
            ajmp   notimp      ; W
            ajmp   monrdx      ; X - read from external memory
            ajmp   monwrx      ; Y - write to external memory
            ajmp   stop        ; Z - stop & power down
            ajmp   notimp      ; [
            ajmp   notimp      ; \
            ajmp   notimp      ; ]
            ajmp   notimp      ; ^
            ajmp   notimp      ; _

logmsg1:    .db     "80C32 Robot Monitor\r\n\0"
logmsg2:    .db     "Alvagem Software 1999-2011 \r\n\0"
logmsg3:    .db     "Including Virtual Machine\r\n\0"
notimsg:    .db     "Command not implemented\r\n\0"
rdymsg:     .db     "Ready>\0"
crlf:       .db     "\r\n\0"

;======================================================================
; notimp
; prints a "Command not implemented" message
;======================================================================
notimp:     mov     dptr,#notimsg   ; not implemented message
            ajmp    scomtxc         ; print and return

;======================================================================
; stop
; stops the processor where possible
;======================================================================
stop:       mov     dptr,#stopmsg
            acall   scomtxc
            orl     pcon,#02h   ; go into power down if CMOS processor
            sjmp    stop        ; infinite loop
stopmsg:    .db "Stop\r\n\0"

;======================================================================
; monwrmem 
; This reads a byte address (in hex), a comma, and data byte(in hex)
; from the serial port ands writes to the given address in internal
; memory
;======================================================================
monwrmem:   acall   scomrhx8        ; get addr in A
            mov     r0,A            ; save for indirect write later
            acall   scomrxa         ; get comma and discard
            acall   scomrhx8        ; get data byte to write 
            mov     @r0,A           ;
            mov     dptr,#crlf
            acall   scomtxc
            ret

;======================================================================
; monrdmem 
; This reads a byte address (in hex) from the serial port, reads the
; internal memory at that address, and sends the value back, in hex
; down the serial port
;======================================================================
monrdmem:   acall   scomrhx8        ; get addr in A
            mov     r0,A            ; setup for indirect fetch
            mov     A,#'-'          ; separator char
            acall   scomtxa
            mov     A,@r0           ; get the byte
            acall   scomtx8         ; and send it back to host
            mov     dptr,#crlf
            acall   scomtxc
            ret

;======================================================================
; monwrx 
; This reads a 16 bit address (in hex), a comma, and data byte(in hex)
; from the serial port ands writes to the given address in external
; memory
;======================================================================
monwrx:     acall   scomrh16        ; 16 bit addr in dptr
            acall   scomrxa         ; get comma and discard
            acall   scomrhx8        ; get data byte to write 
            movx    @dptr,a         
            mov     dptr,#crlf
            acall   scomtxc
            ret

;======================================================================
; monrdx
; This reads a 16 bit address (in hex) from the serial port, reads the
; external memory at that address, and sends the value back, in hex
; down the serial port
;======================================================================
monrdx:     acall   scomrh16        ; 16 bit addr in dptr
            mov     A,#'-'          ; separator char
            acall   scomtxa
            movx    a,@dptr         ; get the data byte
            acall   scomtx8         ; and send it back to host
            mov     dptr,#crlf
            acall   scomtxc
            ret

;======================================================================
; monrdcod
; This reads a 16 bit address (in hex) from the serial port, reads the
; code memory at that address, and sends the value back, in hex
; down the serial port
;======================================================================
monrdcod:   acall   scomrh16        ; 16 bit addr in dptr
            mov     A,#'-'          ; separator char
            acall   scomtxa
            clr     a               ; don't want offset for movc
            movc    a,@a+dptr       ; get the data byte
            acall   scomtx8         ; and send it back to host
            mov     dptr,#crlf
            acall   scomtxc
            ret

;======================================================================
; monwrsfr 
; This reads a byte address (in hex), a comma, and data byte(in hex)
; from the serial port ands writes to the given Special Function 
; Register(SFR).  This uses  direct addressing to ensure we get the 
; SFRs on an 8032, hence there is a mov A,nn; ret for every possible 
; SFR entry and a vectored jump done by computing the target 
; address in the table, pushing that address and doing a ret.
;======================================================================
monwrsfr:   mov     dptr,#monwrsrr  ; setup return address for vectored call
            push    dpl
            push    dph

            acall   scomrhx8    ; get sfr byte address
            anl     a,#127      ;map 128-255 to 0-127 (sfrs all >= 128)
            mov     b,a         ; save address in b
            mov     a,#3        ; each entry in code table is 3 bytes
            mul     ab          ; convert to offset into code table
            mov     dptr,#monwrsft ; address of code table
            add     a,dpl
            push    acc         ; set up low byte of jump addr on stack
            mov     a,b
            addc    a,dph
            push    acc         ; set  up high byte of jump address on stack
            acall   scomrxa     ; get comma (& ignore)
            acall   scomrhx8    ; get value to write in a
            ret                 ; pop jump addr off stack & go to it (vectored jump)

monwrsrr:   mov     dptr,#crlf
            acall   scomtxc
            ret                 ; from routine

monwrsft:   mov      128 ,A
            ret
            mov      129 ,A
            ret
            mov      130 ,A
            ret
            mov      131 ,A
            ret
            mov      132 ,A
            ret
            mov      133 ,A
            ret
            mov      134 ,A
            ret
            mov      135 ,A
            ret
            mov      136 ,A
            ret
            mov      137 ,A
            ret
            mov      138 ,A
            ret
            mov      139 ,A
            ret
            mov      140 ,A
            ret
            mov      141 ,A
            ret
            mov      142 ,A
            ret
            mov      143 ,A
            ret
            mov      144 ,A
            ret
            mov      145 ,A
            ret
            mov      146 ,A
            ret
            mov      147 ,A
            ret
            mov      148 ,A
            ret
            mov      149 ,A
            ret
            mov      150 ,A
            ret
            mov      151 ,A
            ret
            mov      152 ,A
            ret
            mov      153 ,A
            ret
            mov      154 ,A
            ret
            mov      155 ,A
            ret
            mov      156 ,A
            ret
            mov      157 ,A
            ret
            mov      158 ,A
            ret
            mov      159 ,A
            ret
            mov      160 ,A
            ret
            mov      161 ,A
            ret
            mov      162 ,A
            ret
            mov      163 ,A
            ret
            mov      164 ,A
            ret
            mov      165 ,A
            ret
            mov      166 ,A
            ret
            mov      167 ,A
            ret
            mov      168 ,A
            ret
            mov      169 ,A
            ret
            mov      170 ,A
            ret
            mov      171 ,A
            ret
            mov      172 ,A
            ret
            mov      173 ,A
            ret
            mov      174 ,A
            ret
            mov      175 ,A
            ret
            mov      176 ,A
            ret
            mov      177 ,A
            ret
            mov      178 ,A
            ret
            mov      179 ,A
            ret
            mov      180 ,A
            ret
            mov      181 ,A
            ret
            mov      182 ,A
            ret
            mov      183 ,A
            ret
            mov      184 ,A
            ret
            mov      185 ,A
            ret
            mov      186 ,A
            ret
            mov      187 ,A
            ret
            mov      188 ,A
            ret
            mov      189 ,A
            ret
            mov      190 ,A
            ret
            mov      191 ,A
            ret
            mov      192 ,A
            ret
            mov      193 ,A
            ret
            mov      194 ,A
            ret
            mov      195 ,A
            ret
            mov      196 ,A
            ret
            mov      197 ,A
            ret
            mov      198 ,A
            ret
            mov      199 ,A
            ret
            mov      200 ,A
            ret
            mov      201 ,A
            ret
            mov      202 ,A
            ret
            mov      203 ,A
            ret
            mov      204 ,A
            ret
            mov      205 ,A
            ret
            mov      206 ,A
            ret
            mov      207 ,A
            ret
            mov      208 ,A
            ret
            mov      209 ,A
            ret
            mov      210 ,A
            ret
            mov      211 ,A
            ret
            mov      212 ,A
            ret
            mov      213 ,A
            ret
            mov      214 ,A
            ret
            mov      215 ,A
            ret
            mov      216 ,A
            ret
            mov      217 ,A
            ret
            mov      218 ,A
            ret
            mov      219 ,A
            ret
            mov      220 ,A
            ret
            mov      221 ,A
            ret
            mov      222 ,A
            ret
            mov      223 ,A
            ret
            mov      224 ,A
            ret
            mov      225 ,A
            ret
            mov      226 ,A
            ret
            mov      227 ,A
            ret
            mov      228 ,A
            ret
            mov      229 ,A
            ret
            mov      230 ,A
            ret
            mov      231 ,A
            ret
            mov      232 ,A
            ret
            mov      233 ,A
            ret
            mov      234 ,A
            ret
            mov      235 ,A
            ret
            mov      236 ,A
            ret
            mov      237 ,A
            ret
            mov      238 ,A
            ret
            mov      239 ,A
            ret
            mov      240 ,A
            ret
            mov      241 ,A
            ret
            mov      242 ,A
            ret
            mov      243 ,A
            ret
            mov      244 ,A
            ret
            mov      245 ,A
            ret
            mov      246 ,A
            ret
            mov      247 ,A
            ret
            mov      248 ,A
            ret
            mov      249 ,A
            ret
            mov      250 ,A
            ret
            mov      251 ,A
            ret
            mov      252 ,A
            ret
            mov      253 ,A
            ret
            mov      254 ,A
            ret
            mov      255 ,A
            ret

;======================================================================
; monrdsfr 
; This reads a byte address (in hex) from the serial port,
; reads from the given Special Function Register(SFR) and returns
; the value read as 2 hex digits.
; This uses  direct addressing to ensure we get the 
; SFRs on an 8032, hence there is a mov A,nn; ret for every possible 
; SFR entry and a vectored jump done by computing the target 
; address in the table, pushing that address and doing a ret.
;======================================================================
monrdsfr:   acall   scomrhx8
            
            mov     r0,a
            mov     A,#'-'          ; separator char
            acall   scomtxa
            mov     a,r0

            anl     a,#127      ;map 128-255 to 0-127 (sfrs all >= 128)
            acall   monrdsfj    ;read sfr given address in a
            acall   scomtx8     ;send back a hex byte
            mov     dptr,#crlf
            acall   scomtxc
            ret
monrdsfj:   mov     b,a         ;save address in b
            mov     a,#3        ; each entry in code table is 3 bytes
            mul     ab          ; convert to offset into code table
            mov     dptr,#monrdsft ; address of code table
            add     a,dpl
            push    acc         ; set up low byte of jump addr on stack
            mov     a,b
            addc    a,dph
            push    acc         ; set  up high byte of jump address on stack
            ret                 ; pop jump addr off stack & go to it
monrdsft:   mov     A, 128 
            ret
            mov     A, 129 
            ret
            mov     A, 130 
            ret
            mov     A, 131 
            ret
            mov     A, 132 
            ret
            mov     A, 133 
            ret
            mov     A, 134 
            ret
            mov     A, 135 
            ret
            mov     A, 136 
            ret
            mov     A, 137 
            ret
            mov     A, 138 
            ret
            mov     A, 139 
            ret
            mov     A, 140 
            ret
            mov     A, 141 
            ret
            mov     A, 142 
            ret
            mov     A, 143 
            ret
            mov     A, 144 
            ret
            mov     A, 145 
            ret
            mov     A, 146 
            ret
            mov     A, 147 
            ret
            mov     A, 148 
            ret
            mov     A, 149 
            ret
            mov     A, 150 
            ret
            mov     A, 151 
            ret
            mov     A, 152 
            ret
            mov     A, 153 
            ret
            mov     A, 154 
            ret
            mov     A, 155 
            ret
            mov     A, 156 
            ret
            mov     A, 157 
            ret
            mov     A, 158 
            ret
            mov     A, 159 
            ret
            mov     A, 160 
            ret
            mov     A, 161 
            ret
            mov     A, 162 
            ret
            mov     A, 163 
            ret
            mov     A, 164 
            ret
            mov     A, 165 
            ret
            mov     A, 166 
            ret
            mov     A, 167 
            ret
            mov     A, 168 
            ret
            mov     A, 169 
            ret
            mov     A, 170 
            ret
            mov     A, 171 
            ret
            mov     A, 172 
            ret
            mov     A, 173 
            ret
            mov     A, 174 
            ret
            mov     A, 175 
            ret
            mov     A, 176 
            ret
            mov     A, 177 
            ret
            mov     A, 178 
            ret
            mov     A, 179 
            ret
            mov     A, 180 
            ret
            mov     A, 181 
            ret
            mov     A, 182 
            ret
            mov     A, 183 
            ret
            mov     A, 184 
            ret
            mov     A, 185 
            ret
            mov     A, 186 
            ret
            mov     A, 187 
            ret
            mov     A, 188 
            ret
            mov     A, 189 
            ret
            mov     A, 190 
            ret
            mov     A, 191 
            ret
            mov     A, 192 
            ret
            mov     A, 193 
            ret
            mov     A, 194 
            ret
            mov     A, 195 
            ret
            mov     A, 196 
            ret
            mov     A, 197 
            ret
            mov     A, 198 
            ret
            mov     A, 199 
            ret
            mov     A, 200 
            ret
            mov     A, 201 
            ret
            mov     A, 202 
            ret
            mov     A, 203 
            ret
            mov     A, 204 
            ret
            mov     A, 205 
            ret
            mov     A, 206 
            ret
            mov     A, 207 
            ret
            mov     A, 208 
            ret
            mov     A, 209 
            ret
            mov     A, 210 
            ret
            mov     A, 211 
            ret
            mov     A, 212 
            ret
            mov     A, 213 
            ret
            mov     A, 214 
            ret
            mov     A, 215 
            ret
            mov     A, 216 
            ret
            mov     A, 217 
            ret
            mov     A, 218 
            ret
            mov     A, 219 
            ret
            mov     A, 220 
            ret
            mov     A, 221 
            ret
            mov     A, 222 
            ret
            mov     A, 223 
            ret
            mov     A, 224 
            ret
            mov     A, 225 
            ret
            mov     A, 226 
            ret
            mov     A, 227 
            ret
            mov     A, 228 
            ret
            mov     A, 229 
            ret
            mov     A, 230 
            ret
            mov     A, 231 
            ret
            mov     A, 232 
            ret
            mov     A, 233 
            ret
            mov     A, 234 
            ret
            mov     A, 235 
            ret
            mov     A, 236 
            ret
            mov     A, 237 
            ret
            mov     A, 238 
            ret
            mov     A, 239 
            ret
            mov     A, 240 
            ret
            mov     A, 241 
            ret
            mov     A, 242 
            ret
            mov     A, 243 
            ret
            mov     A, 244 
            ret
            mov     A, 245 
            ret
            mov     A, 246 
            ret
            mov     A, 247 
            ret
            mov     A, 248 
            ret
            mov     A, 249 
            ret
            mov     A, 250 
            ret
            mov     A, 251 
            ret
            mov     A, 252 
            ret
            mov     A, 253 
            ret
            mov     A, 254 
            ret
            mov     A, 255 
            ret

;======================================================================
; ihexin
; Reads in intelhex directly into data memory.
; Format of record is :LLAAAATTdCC
; where:
; LL is the length of the data section (d) 
; AAAA is the load address
; TT is the record type (00 for data, 01 for end of file)
; d is the data section containing LL bytes
; CC is the checksum (all the bytes in the record, including the checksum
;                     should sum to 0)
; Parameters:   NONE
; Returns:      NOTHING
; Uses:         A - scratch
;               B - scratch
;               DPTR - load address
;               R0 - byte counter
;               R1 - checksum
;======================================================================
ihexin:     acall   scomrxa         ; read initial :
ihexinlp:   cjne    A,#':', ihexf1  ; fail if not

            mov     sbuf,#'.'       ; transmit confidence dot

            ; get record length & include in checksum
            acall   scomrhx8        ; read record length into A
            jc      ihexf2          ; invalid hex if carry set
            mov     r0,A            ; use R0 as byte counter
            mov     r1,A            ; zero checksum + first byte
            
            clr     scon.1          ; reset TI - char must have been transmitted
                                    ; as we've read 2 chars since sbuf set

            ; get load address in dptr & include in checksum
            acall   scomrh16        ; load address -> dptr
            jc      ihexf2          ; invalid hex if carry set
            mov     a,r1            ; get checksum
            add     a,dph           ; to include load address high byte
            add     a,dpl           ; and load address low byte
            mov     r1,a            ; save again
            
            ; record type: 00 is data, 01 is end
            acall   scomrhx8        ; read record type
            jc      ihexf2          ; invalid hex if carry set
            cjne    a,#1,ihexdata   ; 1 marks end record

            ; if record type was 1 then end record so finish:
            acall   scomrhx8        ; read checksum byte & discard
            jc      ihexf2          ; invalid hex if carry set
            mov     dptr,#ihexokm   ; ok message
            acall   scomtxc         ; & print it
            ret                     ; end of routine

ihexdata:   add     a,r1            ; include record type byte in checksum
            mov     r1,a

; -- loop getting data
ihexlp:     mov     a,r0            ; get count
            jz      ihexerec        ; end of record if done

            acall   scomrhx8        ; read data byte
            jc      ihexf2          ; invalid hex if carry set
            movx    @dptr,a         ; stuff it at current load address
            inc     dptr
            add     a,r1            ; include in checksum
            mov     r1,a

            dec     r0              ; decrement byte count
            sjmp    ihexlp          ; and get next data byte


ihexerec:   ; -- end of record, read in & check checksum
            acall   scomrhx8        ; read checksum byte
            jc      ihexf2          ; invalid hex if carry set
            add     a,r1            ; include in total
            jz      ihexok          ; should sum to 0 if 0K
            mov     dptr,#ihexf3m   ; checksum mismatch message
            ajmp    scomtxc         ; print & return


ihexok:     ; -- the complete record has been read in 
            ; -- skip any cr-lf combination on the end
            acall   scomrxa         ; get next char from input
            cjne    a,#13,ihexncr   ; is it a cr?
            sjmp    ihexok          ; ignore & look for next char if so
ihexncr:    cjne    a,#10,ihexnlf   ; is it a lf?
            sjmp    ihexok          ; ignore & look for next char if so
ihexnlf:    sjmp    ihexinlp        ; not a cr or lf so loop back for next record

ihexf1:     mov     dptr,#ihexf1m   ; missing :
            ajmp    scomtxc         ; print and return
ihexf2:     mov     dptr,#ihexf2m   ; invalid hex 
            ajmp    scomtxc         ; print and return
ihexokm:    .db   "Program loaded\r\n\0"
ihexf1m:    .db   "Missing : at start of Intel-hex record\r\n\0"
ihexf2m:    .db   "Invalid hex in Intel-hex upload\r\n\0"
ihexf3m:    .db   "Intel-hex checksum mis-match\r\n\0"


;======================================================================
; vjump
; vectored jump - gets jump address from serial port & jumps to it
; called routine should terminate with a ret (this is called from
; the monitor vector which already has pushed a return addr)
; Parameters:   NONE
; Returns:      NOTHING
;======================================================================
vjump:      acall   scomrh16        ; jump address -> dptr
            push    dpl             ; save jump address
            push    dph

            mov     dptr,#vjumpmsg
            acall   scomtxc

            ret                     ; jump to pushed address
vjumpmsg:   .db " Jump\r\n\0"       

;======================================================================
; echo
; echoes characters back to host until a . is received (test routine)
;======================================================================
echo:       acall   scomrxa     ; get a char
            acall   scomtxa     ; echo it
            cjne    a,#'.',echo ; loop if not "."
            ret

;======================================================================
;======================================================================
; Support routines
;======================================================================
;======================================================================

;======================================================================
; scominit
; Initialises the serial port to transmit/receive data at 19200 baud
; assuming an 11.059MHz crystal.
; Parameters:   NONE
; Returns:      NOTHING
;======================================================================
scominit:   anl     tmod,#0fh   ; clear t1 control bits
            orl     tmod,#20h   ; set t1 for mode 2 (auto reload)
            mov     th1,#253    ; 9600 baud for11.059MHz clock
            setb    tr1         ; TCON.6 - run timer 1
            orl     pcon,#80h   ; set SMOD - 2x baud ->19200
            mov     scon,#50h   ; 8 bit UART, receive enabled
            clr     ti          
            clr     ri
            ret

;======================================================================
; scomtxa
; Transmits the character in A
; Parameters:   char to transmit in A
; Returns:      NOTHING
;======================================================================
scomtxa:    mov     sbuf,a      ; transmit a
scomtxw:    jnb     scon.1, scomtxw ; wait until char transmitted
            clr     scon.1      ; reset TI before next char
            ret

;======================================================================
; scomtxc
; Transmits the zero terminated string, from code memory, pointed
; to by DPTR
; Parameters:   Pointer to code string in DPTR
; Returns:      NOTHING
; Modifies:     A, DPTR
;======================================================================
scomtxc:    clr     a           ; zero offset
            movc    a,@a+dptr
            jz      scomcz      ; 0 byte for end of string
            acall    scomtxa    ; transmit char
            inc     dptr
            sjmp    scomtxc     ; next char
scomcz:     ret

;======================================================================
; scomtxx
; Transmits the zero terminated string, from data memory, pointed
; to by DPTR
; Parameters:   Pointer to code string in DPTR
; Returns:      NOTHING
; Modifies:     A, DPTR
;======================================================================
scomtxx:    movx    a,@dptr
            jz      scomcxz     ; 0 byte for end of string
            acall    scomtxa    ; transmit char
            inc     dptr
            sjmp    scomtxx     ; next char
scomcxz:    ret

;======================================================================
; scomtx8
; Transmits A as 2 hex digits
; Parameters:   byte to transmit in A
; Returns:      NOTHING
; Modifies:     A,B,DPTR
;======================================================================
scomtx8:    mov     b,a         ; save for low byte
            swap    a           ; get ms nibble -> ls
            anl     a,#15       ; zero high nibble
            acall   scomhcon    ; convert to hex
            acall   scomtxa     ; and send it
            mov     a,b         ; get ls nibble saved earlier
            anl     a,#15       ; zero high nibble
            acall   scomhcon    ; convert to hex
            acall   scomtxa     ; and send it
            ret
scomhcon:   inc     a           ; need to allow 1 byte for ret
            movc    a,@a+pc     ; look up to convert binary to hex digit
            ret
scomhtab:   .db "0123456789ABCDEF"

;======================================================================
; scomcnib
; Translates a hex digit to its binary equivalent.  Note that the 
; lookup table in code memory must be the correct number of bytes 
; away from this routine if this is to work. Note that with the sparse
; look up table the other serial recieve routines are placed between
; this routine & the start of the lookup table ('0') 47 bytes further
; on.
; Parameters:   Hex digit in A
; Returns:      Translated byte in A, 255 if invalid hex
; Modifies:     A
; Bytes:        2
;======================================================================
scomcnib:   anl     a,#127      ; force to 7 bit ascii
            inc     acc         ; allow for ret
            movc    a,@a+pc     ; (1,2) convert ascii to hex nibble via lookup table
            ret                 ; (1,2)
.db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
.db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
.db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
.db 0,1,2,3,4,5,6,7,8,9,255,255,255,255,255,255
.db 255,10,11,12,13,14,15,255,255,255,255,255,255,255,255,255
.db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
.db 255,10,11,12,13,14,15,255,255,255,255,255,255,255,255,255
.db 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255

;======================================================================
; scomrxa
; Waits for, and reads the next byte from the serial port
; Parameters:   NONE
; Returns:      The received byte in A
; Modifies:     A
; Bytes:        8
;======================================================================
scomrxa:    jnb     RI,scomrxa  ; (3,2) wait for char present
            mov     A,SBUF      ; (2,1) read from serial buffer
            clr     RI          ; (2,1)
            ret                 ; (1,2)

;======================================================================
; scomrhx8
; Waits for, and reads a hex encoded byte (2 hex digits) from the
; serial port
; Parameters:   NONE
; Returns:      The received byte in A.  C is set if invalid hex
; Modifies:     A,B 
; Bytes:        
;======================================================================
scomrhx8:   acall   scomrxa             ; read a char into A
            acall   scomcnib            ; convert hex nibble to binary
            cjne    a,#255,scomh8o1     ; ok if not equal to error value
            sjmp    scomh8fl            ; error 
scomh8o1:   swap    a                   ; move digit to high nibble
            mov     b,a                 ; temp store in b while next char read
            acall   scomrxa             ; read a char into A
            acall   scomcnib            ; convert hex nibble to binary
            cjne    a,#255,scomh8o2     ; ok if not equal to error value
            sjmp    scomh8fl            ; error 
scomh8o2:   orl     a,b                 ; fold in ms nibble
            clr     c                   ; ok.
            ret                         ; 
scomh8fl:   mov     a,#0                ; return 0 on error
            setb    c                   ; report failure
            ret

;======================================================================
; scomrh16
; Waits for, and reads a hex encoded word (4 hex digits) from the
; serial port
; Parameters:   NONE
; Returns:      The received word in DPTR, C set if error, clear if ok
; Modifies:     A,B,DPTR 
; Bytes:        
;======================================================================
scomrh16:   acall   scomrhx8    ; ms 2 hex digits -> A
            jc      scomr16f    ; fail if carry set
            mov     dph,a       ; 
            acall   scomrhx8    ; ls 2 hex digits -> A
            jc      scomr16f    ; fail if carry set
            mov     dpl,a       ; 
            clr     c           ; signal success
            ret                 ; 
scomr16f:   mov     dph,#0
            mov     dpl,#0
            ret                 ; with carry set


;**********************************************************************
; Timer 0 routines to provide timing services.
;**********************************************************************

;======================================================================
; timinit
; routine to set up the T0 timer inerrupt.
; TODO- figure out timer values for c. 100Hz on 11.059MHz crystal
;======================================================================
timinit:    clr     P1.0            ; start with space
            anl     tmod,#0F0h      ; clear timer 0 bits
            orl     tmod,#001h      ; set up timer 0 to mode 1, free run
            mov     tl0,#0E0h       ; setup timer for c. 50Hz
            mov     th0,#0B1h       
            setb    et0             ; enable timer 0 overflow interrupt
            setb    ea              ; global interrupt enable
            setb    tcon.4          ; run timer            
            ret


;======================================================================
; timeint
; main timer interrupt routine, called from vector proper.  Provides
; downcounter in countdn variable and also calls any vectored routine
; via tickvect
;======================================================================
timeint:
            push    psw
            push    acc
            push    b

            clr     tcon.4          ; stop timer
            mov     tl0,#0E0h       ; setup timer for space
            mov     th0,#0B1h       
            clr     P1.0
            setb    tcon.4          ; run timer

            ; now time is running - get on with housekeeping tasks
            ; that are driven by timer tick

            ; will need these later...
            push dph
            push dpl

            ; dec countdown timer if non-zero
            mov     a,countdn
            jz      dochain
            dec     countdn         ; dec countdown timer


dochain:    ; see if there is a chain vector
            mov a,tickvect
            orl a,tickvect+1
            jz servdone             ; skip if no chain vector
            
            ; If there is a chain vector, simulate indirect call
            mov dptr,#servdone      ; fix up a return addr
            push dpl                ; on stack to simulate call
            push dph
            mov dpl,tickvect        ; get call vector
            mov dph,tickvect+1
            clr a
            jmp @a+dptr             ; go - called routine should use ret

; -- Restore & return
servdone:   pop dpl
            pop dph
            pop     b
            pop     acc
            pop     psw
            reti



;======================================================================
; settim
; sets the timeout value
;======================================================================
settim:     mov countdn,A
            ret

;======================================================================
; timer
; reads the timer value
;======================================================================
timer:      mov A,countdn
            ret

;======================================================================
; tkchain
; chains the tick value
; new value in dptr, returns old value in dptr
;======================================================================
tkchain:    mov   C,ea              ; get old global interrupt enable
            clr   ea                ; disable interrupts
            mov   A,tickvect        ; swap tickvect with dptr
            mov   tickvect,dpl
            mov   dpl,A
            mov   A,tickvect+1
            mov   tickvect+1,dph
            mov   dph,A
            mov   ea,C              ; restore global interrupt enable
            ret



;**********************************************************************
; I2C routines.  2 channels of I2C implemented called A and B.
;**********************************************************************

;======================================================================
; I2C Start routine for I2c channel A
;======================================================================
i2csta:     setb sda_a
            nop
            nop
            nop
            nop
            setb scl_a
            nop
            nop
            nop
            nop
            clr sda_a
            nop
            nop
            nop
            nop
            clr scl_a
            nop
            nop
            nop
            ret


;======================================================================
; I2C Stop routine for I2C channel A
;======================================================================
i2cspa:     clr sda_a
            nop
            nop
            nop
            nop
            setb scl_a
            nop
            nop
            nop
            nop
            setb sda_a
            nop
            nop
            nop
            ret

;======================================================================
; I2C transmit for I2C channel A.  Byte in A
; returns ~ACK in C
; uses R0
;======================================================================
i2ctxa:     mov r0,#8
i2atx1:     rlc a				; msb -> A
            mov sda_a,c
            setb scl_a
            nop
            nop
            nop
            nop
            clr scl_a
            nop
            djnz r0,i2atx1
            nop
            nop
            setb sda_a
            setb scl_a
            nop
            nop
            nop
            mov c,sda_a			; get ack bit, low is ACK, high NACK
            clr scl_a
            ret

;======================================================================
; I2C receive for channel A.  Ack flag in C, returns byte in A
; uses R0
;======================================================================
i2crxa:     setb sda_a			; make sure data released
            mov r0, #8
i2arx1:     rlc a
            setb scl_a
i2arx2:     jnb scl_a,i2arx2		; clock stretch from slave?
            nop
            nop
            mov c,sda_a
            clr scl_a
            nop
            nop
            djnz r0,i2arx1
            rlc a			; last bit to acc & ack to C
            mov sda_a,c
            setb scl_a
            nop
            nop
            nop
            nop
            clr scl_a
            setb sda_a
            ret


;======================================================================
; I2C Start routine for I2c channel B
;======================================================================
i2cstb:     setb sda_b
            nop
            nop
            nop
            nop
            setb scl_b
            nop
            nop
            nop
            nop
            clr sda_b
            nop
            nop
            nop
            nop
            clr scl_b
            nop
            nop
            nop
            ret


;======================================================================
; I2C Stop routine for I2C channel B
;======================================================================
i2cspb:     clr sda_b
            nop
            nop
            nop
            nop
            setb scl_b
            nop
            nop
            nop
            nop
            setb sda_b
            nop
            nop
            nop
            ret

;======================================================================
; I2C transmit for I2C channel B.  Byte in A
; returns ~ACK in C
; uses R0
;======================================================================
i2ctxb:     mov r0,#8
i2btx1:     rlc a				; msb -> A
            mov sda_b,c
            setb scl_b
            nop
            nop
            nop
            nop
            clr scl_b
            nop
            djnz r0,i2atx1
            nop
            nop
            setb sda_b
            setb scl_b
            nop
            nop
            nop
            mov c,sda_b			; get ack bit, low is ACK, high NACK
            clr scl_b
            ret

;======================================================================
; I2C receive for channel B.  Ack flag in C, returns byte in A
; uses R0
;======================================================================
i2crxb:     setb sda_b			; make sure data released
            mov r0, #8
i2brx1:     rlc a
            setb scl_b
i2brx2:     jnb scl_b,i2brx2		; clock stretch from slave?
            nop
            nop
            mov c,sda_b
            clr scl_b
            nop
            nop
            djnz r0,i2brx1
            rlc a			; last bit to acc & ack to C
            mov sda_b,c
            setb scl_b
            nop
            nop
            nop
            nop
            clr scl_b
            setb sda_b
            ret



;======================================================================
; Jump table into useful monitor routines.  This isolates other code
; from changes in the monitor routines by providing a fixed vector table
; into those routines
;======================================================================
            .org 1000h - 100h  ; allow 256 bytes jump table at top of bottom 4k
            ljmp stop          ; stop execution
            ljmp scominit      ; intitialise serial port
            ljmp scomtxa       ; transmit ascii char in A
            ljmp scomtxc       ; transmit string from code mem ptr to by dptr
            ljmp scomtxx       ; transmit string from ext mem ptr to by dptr
            ljmp scomtx8       ; transmit byte in A as 2 hex digits
            ljmp scomrxa       ; receive an ascii char in A
            ljmp scomrhx8      ; receive a hex encoded byte into A
            ljmp scomrh16      ; receive a hex encoded word into dptr
            ljmp ihexin        ; read intelhex file into data memory
            ljmp settim        ; setup timer - value in A
            ljmp timer         ; get timer count - return value in A
            ljmp tkchain       ; chain tick count interrupt (dptr in, dptr out)
            ljmp i2csta        ; I2C channel A - start
            ljmp i2cspa        ; I2C channel A - stop
            ljmp i2ctxa        ; I2C channel A - transmit A
            ljmp i2crxa        ; I2C channel A - receive to A
            ljmp i2cstb        ; I2C channel B - start
            ljmp i2cspb        ; I2C channel B - stop
            ljmp i2ctxb        ; I2C channel B - transmit A
            ljmp i2crxb        ; I2C channel B - receive to A



 ;********************* ROBOT VIRTUAL MACHINE **********************
            .org  1000h                 ; 3rd 2k page
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



 ;********************* ROBOT VM EXTENSIONS **********************
            .org 1800h   ; 4th 2k page
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





 ;******************************  END  *******************************
 .end
