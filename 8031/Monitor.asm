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


