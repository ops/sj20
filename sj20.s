;;;
;;; sj20
;;;

        .include "cbm_kernal.inc"
        .include "vic20.inc"

        .segment "SJ20"

        .export sj20_init

PT3             = $14           ; Pointer

SAVESTART       = $c1
LOADPTR         = $c3
LOADSTART       = $ac
LOADEND         = $ae

STATUS          = $90
VERCK           = $93
DFLTN           = $99           ; default input device
DFLTO           = $9A           ; default output device
MSGFLG          = $9D           ; Direct=$80/RUN=0
LA              = $B8

CHRGET          = $0073         ; GET NEXT CHAR
CHRGOT          = $0079         ; GET LAST CHAR

PTRSTR          := $CB1E        ; Print zero terminated sting
FRMNUM          := $cd8a        ; GET NUMERIC VALUE
FRMBYTE         := $d79e        ; GET BYTE VALUE TO X
CNVWORD         := $d7f7        ; CONVERT TO WORD VALUE INTO Y/A; $14 (PT3)
RSPAUSE         := $F160        ; SET TIMER
SEROUT1         := $E4A0
SEROUT0         := $E4A9
SRCLKHI         := $EF84
SRCLKLO         := $EF8D
SERGET          := $E4B2
SRBAD           := $EEB4        ; device not present
FNDFLNO         := $F3CF        ; Find file number (.X)
SETFLCH         := $F3DF        ; Set file characteristics of file (.X)

KERN_GETIN      := $F1F5
KERN_BASIN      := $F20E

ORIOST          := $FE6A        ; OR .A with the contents of STATUS
                                ; and store back


ICHKIN          := $031e
ICKOUT          := $0320
ICLRCH          := $0322
IBASIN          := $0324
IBSOUT          := $0326
IGETIN          := $032a
ICLALL          := $032c
ILOAD           := $0330
ISAVE           := $0332


sj20_init:
        lda     #<jiffy_load
        ldx     #>jiffy_load
        sta     ILOAD
        stx     ILOAD+1

.ifdef SJ20_SAVE
        lda     #<jiffy_save
        ldx     #>jiffy_save
        sta     ISAVE
        stx     ISAVE+1
.endif ; SJ20_SAVE

.ifdef SJ20_IO
        lda     #<jiffy_chkin
        ldx     #>jiffy_chkin
        sta     ICHKIN
        stx     ICHKIN+1
        lda     #<jiffy_ckout
        ldx     #>jiffy_ckout
        sta     ICKOUT
        stx     ICKOUT+1
        lda     #<jiffy_basin
        ldx     #>jiffy_basin
        sta     IBASIN
        stx     IBASIN+1
        lda     #<jiffy_bsout
        ldx     #>jiffy_bsout
        sta     IBSOUT
        stx     IBSOUT+1
        lda     #<jiffy_getin
        ldx     #>jiffy_getin
        sta     IGETIN
        stx     IGETIN+1
        lda     #<jiffy_clrch
        ldx     #>jiffy_clrch
        sta     ICLRCH
        stx     ICLRCH+1
        lda     #<jiffy_clall
        ldx     #>jiffy_clall
        sta     ICLALL
        stx     ICLALL+1
.endif ; SJ20_IO
        rts


jiffy_talk:
        ora     #$40
        .byte   $2c
jiffy_listen:
        ora     #$20
        jsr     RSPAUSE         ; SET TIMER
jiffy_listen2:
        pha                     ; save device address
        bit     $94             ; test deferred character flag
        bpl     :+              ; branch if no defered character
        sec                     ; flag EOI
        ror     $A3             ; rotate into EOI flag byte
        jsr     jiffy_send_byte
        lsr     $94             ; clear deferred character flag
        lsr     $A3             ; clear EOI flag
:       pla                     ; device address OR'ed with command
        sta     $95             ; save as serial defered character
        sei
        lda     #$00
        sta     $A3
        jsr     SEROUT1         ; set IEC data out high (0)
        cmp     #$3F
        bne     l6E38           ; branch if not $3F, this branch will always be taken

        jsr     SRCLKHI         ;

l6E38:  lda     VIA1_PA2        ; get VIA 1 DRA, no handshake
        ora     #$80            ; set IEC ATN low (1)
        sta     VIA1_PA2        ; set VIA 1 DRA, no handshake
lEE40:  jsr     SRCLKLO         ; set IEC clock out low
        jsr     SEROUT1         ; set IEC data out high (0)
        jsr     $EF96           ; 1ms delay

IEC_send_byte:
        sei
        jsr     SEROUT1         ; set serial data out high
        jsr     SERGET          ; get serial clock status
        lsr                     ; shift serial data to Cb
        bcs     l6EB4           ; Device_Not_Present
        jsr     SRCLKHI         ; set serial clock high
        bit     $A3             ; test EOI flag
        bpl     l6E66           ; branch if not EOI

; I think this is the EOI sequence so the serial clock has been released
; and the serial data is being held low by the peripherals. first up
; wait for the serial data to rise

:       jsr     SERGET          ; get serial clock status
        lsr                     ; shift serial data to Cb
        bcc     :-              ; loop if data low

; now the data is high, EOI is signalled by waiting for at least 200us
; without pulling the serial clock line low again. the listener should
; respond by pulling the serial data line low

:       jsr     SERGET          ; get serial clock status
        lsr                     ; shift serial data to Cb
        bcs     :-              ; loop if data high

; the serial data has gone low ending the EOI sequence, now just wait
; for the serial data line to go high again or, if this isn't an EOI
; sequence, just wait for the serial data to go high the first time

l6E66:  jsr     SERGET          ; get serial clock status
        lsr                     ; shift serial data to Cb
        bcc     l6E66           ; loop if data low

; serial data is high now pull the clock low, preferably within 60us

        jsr     SRCLKLO         ; set IEC clock out low

; now the Vic has to send the eight bits, LSB first. first it sets the
; serial data line to reflect the bit in the byte, then it sets the
; serial clock to high. The serial clock is left high for 26 cycles,
; 23us on a PAL Vic, before it is again pulled low and the serial data
; is allowed high again

; The jiffy routine detecs Jiffy devices within the routine
; jiffy_detect_device and X=2

        txa
        pha
        ldx     #$08            ; eight bits to do

@loop:  lda     VIA1_PA2
        and     #$02
        bne     @skip           ; IEC clock low (1) ?
        pla                     ; no
        tax                     ; restore X
        jmp     $EEB7           ; IEC_Timeout
@skip:  jsr     SEROUT1         ; set IEC data high (0)
        ror     $95             ; rotate bit to send into carry
        bcs     :+              ; branch if bit = 1
        jsr     SEROUT0         ; set IEC data low (1)
:       jsr     SRCLKHI         ; set IEC clock high (0)
        lda     VIA2_PCR
        and     #$DD            ; set data high (0)
        ora     #$02            ; set clock low (1)
        php
        pha
        jsr     jiffy_detect_device
        pla
        plp
        dex
        bne     @loop           ; next bit
        pla
        tax
        jmp     $EEA0

l6EB4:  jmp     SRBAD           ;err DEV NOT PRES


.proc jiffy_detect_device
        sta     VIA2_PCR        ; store in serial bus I/O port
        bit     VIA1_PA2        ; test ATN, attention
        bpl     @out            ; ATN=1, done
        cpx     #$02
        bne     @out            ; done

        lda     #$02            ; test bit 1 (DATA) of serial bus
        ldx     #$20            ; 1e??? wait for jiffy protocol
@wait:  bit     VIA1_PA2        ; test DATA
        beq     @wait2          ; data high (0) -> Jiffy signal
        dex
        bne     @wait
        beq     @no_jiffy       ; no Jiffy device

@wait2: bit     VIA1_PA2
        beq     @wait2          ; wait for end of Jiffy signal
        lda     $95
        ror
        ror
        ora     #$40
        sta     $A3             ; Flag as Jiffy device
@no_jiffy:
        ldx     #$02
@out:   rts
.endproc


get_byte:
        sei
        bit     $A3             ; is the device JiffyDOS equipped?
        bvs     :+              ; yes
        lda     #$00            ; no,
        jmp     $EF1C           ; drive not jiffydos equipped, use original routine

;Read a byte in using the SJLoad routine.
;total time for 1 byte received:
;NTSC: (18+11+14+8+8+23+19)  * (1/1.022727) = 98.75558189 microseconds
; PAL: (18+14+14+9+10+25+19) * (1/1.108405) = 98.339505867 microseconds
;if the PAL version were to be used on NTSC, it would take 106.577806199 microseconds.

;timing:
;NTSC: 18*(1/1.022727) microseconds = 17.6000004693 microseconds
; PAL: 18*(1/1.108405) microseconds = 16.239551428 microseconds

:       lda     VIA1_PA2        ;4 serial bus
        and     #$03            ;2 mask clock-in and data-in bits
        beq     :-              ;2 wait for one of them to be high

        lda     #$80            ;2 initialize the byte-received flag
        sta     $9C             ;3 to $80
        txa                     ;2 save .X
        pha                     ;3 i'm serious

;timing:
;NTSC: 11*(1/1.022727) microseconds = 10.755558424 microseconds
; PAL: 14*(1/1.108405) microseconds = 12.630762221 microseconds

        pha                     ;3
        pla                     ;4

.ifdef SJ20_NTSC
        nop                     ;2
        nop                     ;2
.endif

.ifndef SJ20_NTSC
        pha                     ;3
        pla                     ;4
.endif

;timing:
;NTSC: 14*(1/1.022727) microseconds = 13.688892539 microseconds
; PAL: 14*(1/1.022727) microseconds = 12.630762221 microseconds

        lda     VIA2_PCR        ;4 handshaking (bring serial bus data line high)
        and     #$DD            ;2 handshaking cont'd
        sta     VIA2_PCR        ;4 handshaking cont'd
        ora     #$20            ;2 set bit 5=1
        tax                     ;2 store

timing:
;NTSC: 8*(1/1.022727) microseconds = 7.822224308 microseconds
; PAL: 9*(1/1.108405) microseconds = 8.302200083 microseconds

        bit     $9C             ;timing
.ifndef SJ20_NTSC
        bit     $9C             ;timing
.endif
        bit     $9C             ;timing
.ifdef SJ20_NTSC
        nop
.endif

;timing:
;NTSC:  8*(1/1.022727) microseconds = 7.822224308 microseconds
; PAL: 10*(1/1.108405) microseconds = 9.02197305 microseconds

        lda     VIA1_PA2        ; get bit 0 & 1
        ror                     ; bit 0 (clock) -> bit 7
        ror                     ; bit 1 (data ) -> carry
        and     #$80            ; mask received bit 0

.ifndef SJ20_NTSC
        nop                     ;2
.endif

        ora     VIA1_PA2        ; get bit 2 & 3
        rol                     ; A = .....XXX
        rol                     ; A = ....XXXX
        sta     $B3             ; store lower nibble

        lda     VIA1_PA2        ; get bit 4 & 5
        ror                     ; bit 4 (clock) -> bit 7
        ror                     ; bit 5 (data ) -> carry
        and     #$80            ; mask received bit 4

.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing:
;NTSC: 19*(1/1.022727) microseconds = 18.577782732 microseconds
; PAL: 19*(1/1.108405) microseconds = 17.141748729 microseconds

        ora     VIA1_PA2        ; get bit 6 & 7
        rol                     ; A = .....XXX
        rol                     ; A = ....XXXX
        sta     $C0             ; store upper nibble
        lda     VIA1_PA2        ; get status bits
        stx     VIA2_PCR        ; data out (5) = 1
;end of timing sensitive portion

        sta     $9C             ; save status bits
        jsr     jiffy_combine_nibbles

        sta     $A4             ; received byte
        pla
        tax
        lda     $9C             ; restore status bits
        ror                     ; (clock) -> bit 7
        ror                     ; (data ) -> carry
        bpl     l7C54           ; Jiffy_Set_OK   ; clock = 0 -> OK
        bcc     lfC4f           ; Jiffy_Set_EOI  ; data  = 0 -> EOI
        lda     #$42            ; EOI (6) and time out (1) ($42)
        jmp     $EEB9           ; Set_IEC_Status ;;ERR staTUS, UNLISTEN


send_byte:
        bit     $94             ; test deferred character flag
        bmi     @send           ; branch if defered character
        sec                     ; set deferred character flag
        ror     $94
        bne     @out
@send:  pha
        jsr     jiffy_send_byte
        pla
@out:   sta     $95             ; save as serial defered character
        clc
        rts


jiffy_send_byte:
        sei
        bit     $A3             ; test to see if the device is a JiffyDOS drive
        bvs     JIFFY_OUT
        lda     $A3
        cmp     #$A0
        bcs     JIFFY_OUT
        jmp     IEC_send_byte

lfC4f:  lda     #$40            ; bit 6 = EOI
        jsr     ORIOST
l7C54:  lda     $A4
l7C56:  cli
        clc
        rts


JIFFY_OUT:
;JIFFYDOS PATCH SEND DATA ON SERIAL LINE in C64 docs
; the bits in BSOUR are sent in the following order %22114334

        txa             ; store .X on stack
        pha
        lda     $95     ; BSOUR, the byte to send
        lsr             ; put MSB in LSB
        lsr
        lsr
        lsr
        tax             ; give to .X
        lda     lFCCE,x ; get the corresponding data from the send table
        pha             ; save it
        txa             ; restore .A to .X
        lsr             ; next 2 bits
        lsr
        tax             ; give to .X
        lda     lFCCE,x ; get the corresponding send table data again
        sta     $B3
        lda     $95     ; restore BSOUR
        and     #$0F    ; get LSB of BSOUR
        tax             ; give to .X

        lda     #$02

;start of timing sensitive portion
;total time:
;NTSC: (15+14+20+17+18/19+13) * (1/1.022727) = 94.844469736 microseconds
; PAL: (15+16+22+19+20/21+13) * (1/1.108405) = 94.73071666 microseconds

;timing:
;NTSC: 15
; PAL: 15

:       bit     VIA1_PA2        ;4 wait for bit 1 (data) of $911F to be set
        beq     :-              ;2 loop until data is 1

        lda     VIA2_PCR        ;4 handshaking - (bring serial bus data line high)
        and     #$DD            ;2 yep
        sta     $9C             ;3 save what we want to handshake.

;timing
;NTSC: 14 *
; PAL: 20 *

        pha                     ;3
        pla                     ;4
        pha                     ;3
        pla                     ;4
.ifndef SJ20_NTSC
        nop                     ;2
        nop                     ;2
        nop                     ;2
.endif

;timing
;NTSC: 14
; PAL: 16

        sta     VIA2_PCR        ;4 handshaking - bring the data line high
        pla                     ;3 restore .A (gotten from send table earlier)
        ora     $9C             ;3 OR with handshake value to get value to send

.ifndef SJ20_NTSC
        nop                     ;2
.endif

        sta     VIA2_PCR        ;4 send to drive over serial bus

;timing
;NTSC: 20
; PAL: 22

        lda     $B3             ;3 get 2nd value to send
        ora     $9C             ;3 OR with old $912C
        ora     $9C             ;3 timing
        sta     VIA2_PCR        ;4 send to drive over serial bus
        lda     lFBBA,x         ;4 Get third value to send from table
        ora     $9C             ;3 OR with old $912C

.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing
;NTSC: 17
; PAL: 19

        sta     VIA2_PCR        ;4 send to drive over serial bus
        lda     lF39E,x         ;4 Get fourth value to send from table
        ora     $9C             ;3 OR with old $912C
        nop                     ;2 timing
        sta     VIA2_PCR        ;4 send to drive over serial bus

.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing
;NTSC: 18/19
; PAL: 20/21

        and     #$DD            ;2
        bit     $A3             ;3 is bit 7 of LDFLAG set?
        bmi     :+              ;2/3 yes, don't bring data line low yet
        ora     #$02            ;2 no, OR to bring serial bus data line low
:       sta     VIA2_PCR        ;4 handshaking - bring data line low

        pla                     ;4 restore .X
        tax                     ;2

.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing
;NTSC: 13
; PAL: 13

        lda     $9C             ;3 get old $912C
        ora     #$02            ;2 OR to bring data line low
        sta     VIA2_PCR        ;4 handshaking - bring data line low
        lda     VIA1_PA2        ;4 read serial bus
        and     #$02            ;2 is data line low?
;end of timing sensitive portion
        beq     l7C56           ; yes, we're done
        jmp     $EEB7           ; no, err TIME OUT


jiffy_combine_nibbles:
        lda     $B3
        and     #$0F
        sta     $B3
        lda     $C0
        asl
        asl
        asl
        asl
        ora     $B3
        rts


jiffy_untalk:
        lda     VIA1_PA2        ; get VIA 1 DRA, no handshake
        ora     #$80            ; set IEC ATN low (1)
        sta     VIA1_PA2        ; set VIA 1 DRA, no handshake
        jsr     SRCLKLO
        lda     #$5F
        .byte   $2c
jiffy_unlisten:
        lda     #$3F
        jsr     jiffy_listen2
        jsr     $EEC5
        txa
        ldx     #$0B
:       dex
        bne     :-
        tax
        jsr     SRCLKHI
        jmp     SEROUT1


jiffy_talksa:
        sta     $95
        jsr     lEE40
        jmp     $eed3


jiffy_listensa:
        sta     $95
        jsr     lEE40
        jmp     $eec5


;--------------JIFFY DATA TABLE
lFCCE:  .byte   $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22

lFBBA:  .byte   $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22

lF39E:  .byte   $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22
;--------------JIFFY DATA TABLE


jiffy_load:                     ; "fnam",PA,SA[,loadadr]
        ldx     DEVNUM          ; PA (device#)
        cpx     #4
        bcs     MY_IECLOAD_0
        jmp     $f549           ; OLD LOAD PROC


MY_IECLOAD_0:
        sta     VERCK
        lda     #0
        sta     LA              ; file# - flag for first byte
;        jsr     $f647           ; Print "SEARCHING"
        lda     #$f0            ; channel
        jsr     DISK_LISTEN
        jsr     IECNAMOUT
        bcc     :+
        rts

:       ldy     #$00
        lda     (FNAM),y
        cmp     #'$'
        bne     :+
        jmp     $F56D           ; KERN_LOAD
:       lda     #$60
        jsr     DISK_TALK
        jsr     get_byte        ; load address lo
        sta     LOADEND
        lda     STATUS
        lsr
        lsr
        bcc     :+
        jmp     $f787
:       jsr     get_byte        ; load address hi
        sta     LOADEND+1
        ldx     SECADR
.ifdef SJ20_BASIC_EXTENSIONS
        beq     sa0             ; SA=0 -->
        dex
        dex
.endif ; SJ20_BASIC_EXTENSIONS
        bne     sa1             ; SA=1 -->
.ifdef SJ20_BASIC_EXTENSIONS
        ; SA=2: LOAD CARTRIDGE AT $c3
        pha
        lda     LOADEND
        pha                     ; FIRST TWO BYTES ...
.endif ; SJ20_BASIC_EXTENSIONS

        ; SA=0: LOAD PROGRAM AT $c3
sa0:
.ifdef SJ20_BASIC_EXTENSIONS
        jsr     FRMWORD2        ; GET WORD VALUE
.endif ; SJ20_BASIC_EXTENSIONS
        lda     LOADPTR+1
        ldx     LOADPTR
.ifdef SJ20_BASIC_EXTENSIONS
        bcs     :+
        lda     PT3+1
        ldx     PT3
:
.endif ; SJ20_BASIC_EXTENSIONS

        sta     LOADEND+1
        stx     LOADEND

sa1:

.ifdef SJ20_EXT_MESSAGES
        jsr     PRINT_ATADR
.else
        jsr     $f66a            ; Print "LOADING / VERIFYING"
.endif ; SJ20_EXT_MESSAGES

        ldx     SECADR

.ifdef SJ20_BASIC_EXTENSIONS
        dex
        dex
        bne     @skip            ; SA!=2 -->
        ;STORE FIRST TWO BYTES
        ldy     #0
        pla
        jsr     STOREBYTE
        pla
        jsr     STOREBYTE
@skip:
.endif ; SJ20_BASIC_EXTENSIONS

        bit     $A3
        bvs     FB1F            ; Jiffy -->
        jsr     $F58A           ; KERN_LOAD
err2:   bcs     err
MYLO_E:
.ifdef SJ20_EXT_MESSAGES
        jsr     PRINT_TOADR
.endif ; SJ20_EXT_MESSAGES
        jsr     DISK_CLOSE_LO
        clc
        ldx     LOADEND
        ldy     LOADEND+1
        rts

FB1F:   jsr     jiffy_untalk
        lda     #$61
        jsr     DISK_TALK
        sei
        lda     $B2
        pha
        ldy     #$00            ; offset to address to store/verify byte from ($AE) (always 0)
FB25:   jsr     $F755           ; read $912F and wait for value to settle
        cmp     #$FE
        beq     FB5B

;timing:
;NTSC: 35*(1/1.022727)  = 34.222231348 microseconds
; PAL: 35*(1/1.1108405) = 31.507673694 microseconds

        lda     VIA2_PCR        ;4 read peripheral control register
        and     #$DD            ;2 turn off bits 5 and 1 (bring serial bus data line high)
        tax                     ;2 save value to bring serial bus high in .X
        ora     #$20            ;2 value to bring serial bus data line high
        sta     $B2             ;3 save the value to bring serial bus low in $BA
        stx     VIA2_PCR        ;4 bring serial bus data line high
        lda     #$80            ;2
        sta     $9C             ;3

:       lda     VIA1_PA2        ;4 read serial bus
        lsr                     ;2 shift out CLK
        bcc     :-              ;2 wait for a 1

        and     #$01            ;2 is DATA 1?
        beq     FB67            ;3 no, skip

        ldx     #$6D            ; number of times to loop before timeout
:       bit     VIA1_PA2        ; was DATA high, and is DATA currently high?
        beq     FB54            ; no, skip
        dex                     ; yes, decrement .X
        bne     :-              ;3/2 loop until timeout

        lda     #$42            ; timeout message
        .byte   $2c             ; skip next instruction
FB54:   lda     #$40            ; success
        jsr     ORIOST          ; set the result of the communication
        clc                     ; success
        .byte   $24             ; skip 'sec'
FB5B:   sec                     ; failure
        pla                     ; restore $B2
        sta     $B2             ; set $B2 to its old value
        bcc     MYLO_E
err:    jmp     $F6CB           ; failure: UNLISTEN, CLOSE, BREAK

FB67:   lda     #$02            ; bit 1 (DATA)

;timing (resync point):
;total timing
;NTSC: (6+25+13+10+25)*(1/1.022727)  = 75.288908966 microseconds
; PAL: (6+25+15+12+25)*(1/1.1108405) = 74.718197617 microseconds
;NTSC: 6*(1/1.022727)
; PAL: 6*(1/1.1108405)

:       bit     VIA1_PA2        ;4 check DATA
        beq     :-

;timing:
;NTSC: 23*(1/1.022727)  = 22.4888948859 microseconds
; PAL: 25*(1/1.1108405) = 22.50548121 microseconds

FB6E:   pha                     ;3 timing
        pla                     ;4 timing

.ifndef SJ20_NTSC
        nop                     ;2
.endif
        lda     $B2             ;3 value to store in $912C, handshaking control register
        sta     VIA2_PCR        ;4 set peripheral control register
        lda     #$01            ;2 bit 0 (CLK)
        bit     VIA1_PA2        ;4 check CLK
        beq     FB25            ;2/3 if zero, start over

;timing (1 + 14):
;NTSC: 13*(1/1.022727)  = 14.666670578 microseconds
; PAL: 15*(1/1.1108405) = 13.503288726 microseconds

        stx     VIA2_PCR        ;4 handshaking - bring serial bus DATA line high
        lda     VIA1_PA2        ;4 read serial bus
        ror                     ;2 shift bit 0 into carry and bit 1 into bit 0
        ror                     ;2 shift bit 0 into bit 7 and bit 1 into carry
.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing (12):
;NTSC: 10*(1/1.022727) = 11.733336462 microseconds
; PAL 12*(1/1.1108405) = 10.802630981 microseconds

        and     #$80            ;2 clear the bits we don't care about
        ora     VIA1_PA2        ;4 read serial bus to get next 2 bits
        rol                     ;2 shift back first two bits into bits 0 and 1, and
        rol                     ;2 the 2 new bits into bits 2 and 3
.ifndef SJ20_NTSC
        nop                     ;2
.endif

;timing: (25)
;NTSC: 25*(1/1.022727)  = 24.444450963 microseconds
; PAL: 25*(1/1.1108405) = 22.505481201 microseconds

        sta     $B3             ;3 save first nybble
        lda     VIA1_PA2        ;4 read serial bus again
        ror                     ;2 CLK
        ror                     ;2 DATA
        and     $9C             ;3 (and $80) - isolate CLK (bit 7) and DATA (carry)
        ora     VIA1_PA2        ;4 read serial bus to get the final 2 bits
        rol                     ;2 CLK
        rol                     ;2 DATA
        sta     $C0             ;3 save the other nybble
;-end of timing sensitive portion
        lda     #>(FB6E-1)      ; Return address on stack
        pha
        lda     #<(FB6E-1)
        pha
        jsr     jiffy_combine_nibbles

STOREBYTE:
        cpy     VERCK           ; verify?
        bne     FBB0            ; yes
        sta     ($AE),y         ; no, load, store the byte
FBA7:   inc     $AE             ; increment low byte of address
        bne     :+
        inc     $AF             ; increment hi byte of address
:       rts
FBB0:   ;VERIFY
        cmp     ($AE),y         ; verify byte
        beq     FBA7            ; byte is the same, continue
        lda     #$10            ; set STATUS
        sta     STATUS
        bne     FBA7            ; continue verifying

.ifdef SJ20_SAVE

; ========================================================================
; MY SAVE                   ENDADDR   = ($AE/$AF)    STARTADDR = ($C1/$C2)
;
; SAVESTART = $c1
; LOADPTR   = $c3
; LOADSTART = $ac
; LOADEND   = $ae
; ========================================================================

        ; SAVE VECTOR           :: "fnam",PA,SA[,fromadr,toaddr]
jiffy_save:
        ldx     DEVNUM
        cpx     #4
        bcs     @skip
        jmp     $F685           ; KERN_SAVE
@skip:
.ifdef SJ20_BASIC_EXTENSIONS
        jsr     FRMWORD2        ; GET WORD VALUE
        bcs     MYSA_0
        sty     LOADSTART
        sta     LOADSTART+1
        jsr     FRMWORD2        ; GET WORD VALUE
        bcs     MYSA_0
        sty     LOADEND
        sta     LOADEND+1
        ldy     LOADSTART
        lda     LOADSTART+1
        sty     SAVESTART
        sta     SAVESTART+1
.endif ; SJ20_BASIC_EXTENSIONS

MYSA_0:
        lda     #$f1            ; channel
        jsr     DISK_LISTEN
        jsr     IECNAMOUT
        bcs     MYSA_ERR
        lda     #$61
        jsr     DISK_LISTEN
        jsr     $fbd2           ; $C1/$C2 --> $ac/$ad
        lda     LOADSTART
        jsr     send_byte
        lda     LOADSTART+1
        jsr     send_byte

.ifdef SJ20_EXT_MESSAGES
        lda     #LOADSTART
        jsr     PRINT_ATADR_2
.else
        jsr     $f728           ; Print "SAVING"
.endif ; SJ20_EXT_MESSAGES

        ldy     #0
MYSA_00:
        jsr     $fd11           ;END ADDRESS?
        bcs     MYSA_E0         ;YES -->
        lda     (LOADSTART),y
        jsr     send_byte
        jsr     STOP
        bne     MYSA_02
        jsr     jiffy_unlisten
        jsr     DISK_CLOSE_SA
        jmp     $f6ce
MYSA_02:
        jsr     $fd1b           ; inc ADDR
        bne     MYSA_00
MYSA_E0:
        jsr     jiffy_unlisten
        jsr     DISK_CLOSE_SA

.ifdef SJ20_EXT_MESSAGES
        lda     #LOADSTART
        jsr     PRINT_TOADR_2
.endif ; SJ20_EXT_MESSAGES
        ;  jsr PRINT_DISK_ERR
        clc
MYSA_ERR:
        rts
.endif ; SJ20_SAVE

        ;PUT NAME TO IEC and UNLISTEN
IECNAMOUT:
        lda     STATUS
        bmi     DICM_ERR1
        jsr     IECNAMOUT_2
DICM_OK2:
        jsr     jiffy_unlisten
DICM_OK:
        clc
        rts

        ;PUT NAME TO IEC
IECNAMOUT_2:
        ldx     FNAM_LEN
        beq     DICM_OK2
        ldy     #0
@loop:  lda     (FNAM),y
        jsr     send_byte
        iny
        dex
        bne     @loop
        rts
DICM_ERR1:
        jmp     $f78a           ;ERR 'DEVICE NOT PRESENT'    CF=1

DISK_LISTEN:
        pha
        lda     #0
        sta     STATUS
        beq     DILI_2
DISK_LISTEN_2:
        pha
DILI_2: lda     DEVNUM
        jsr     jiffy_listen
        pla
        jsr     jiffy_listensa
DITA_5: lda     STATUS
        bpl     DICM_OK
        sec
        rts

DISK_TALK:
        pha
        lda     #0
        sta     STATUS
        lda     DEVNUM
        jsr     jiffy_talk
        pla
        jmp     jiffy_talksa

DISK_CLOSE_SA:
        lda     #$e1
        bne     DICL_1

DISK_CLOSE_LO:
        jsr     jiffy_untalk
        lda     #$e0
DICL_1:
        jsr     DISK_LISTEN_2
        jmp     jiffy_unlisten

.ifdef SJ20_BASIC_EXTENSIONS
        ; GET WORD VALUE IN Y/A and (PT3)
FRMWORD2:
        jsr     CHKCOM
        bcs     FRWO_3
FRMWORD:
        jsr     FRMNUM
        jsr     CNVWORD
        clc
FRWO_3: rts

CHKCOM:
        jsr     CHRGOT
        cmp     #','
        sec
        bne     :+
        jsr     CHRGET
        clc
:       rts
.endif ; SJ20_BASIC_EXTENSIONS

.ifdef SJ20_EXT_MESSAGES

PRINT_ATADR:
        lda     #LOADEND
PRINT_ATADR_2:
        ldx     MSGFLG
        bmi     skip
out:    rts
skip:   pha
        lda     #<MSG_LOAD_FROM
        ldy     #>MSG_LOAD_FROM
        jsr     PTRSTR
        pla
bb3:    tax
        lda     $01,x
        pha
        lda     $00,x
        tax
        pla
        jmp     hex_out
        ; PRINT LOAD AT ADDRESS
PRINT_TOADR:
        lda     #LOADEND
PRINT_TOADR_2:
        ldx     MSGFLG
        bpl     out
        pha
        lda     #<MSG_LOAD_TO
        ldy     #>MSG_LOAD_TO
        jsr     PTRSTR
        pla
        jsr     bb3
        lda     #13
        jmp     BSOUT

        ; Print HEX value in X/A
.proc hex_out
        pha
        lda     #'$'
        jsr     BSOUT
        pla
        beq     HEX0
        jsr     HEX2
HEX0:   txa
HEX2:   pha
        lsr
        lsr
        lsr
        lsr
        jsr     HEX1
        pla
        and     #$0F
HEX1:   clc
        adc     #246
        bcc     :+
        adc     #6
:       adc     #58
        jmp     BSOUT
.endproc

MSG_LOAD_FROM:
        .byte   " from ", 0
MSG_LOAD_TO:
        .byte   " to ", 0

.endif ; SJ20_EXT_MESSAGES


.ifdef SJ20_IO

jiffy_chkin:
        jsr     FNDFLNO         ; search logical file#
        beq     @1
        jmp     $F784           ; "file not open" error msg
@1:     jsr     SETFLCH         ; set file param
        lda     DEVNUM          ; device#
        cmp     #8
        bcs     @2
        jmp     $F2D2           ; KERN_CHKIN
@2:     tax
        jsr     jiffy_talk
        lda     SECADR
        bpl     @3
        jmp     $F2F8
@3:     jsr     jiffy_talksa
        jmp     $F301

jiffy_ckout:
        jsr     FNDFLNO         ; search logical file#
        beq     @1
        jmp     $F784           ; "file not open" error msg
@1:     jsr     SETFLCH         ; set file param
        lda     DEVNUM          ; device#
        cmp     #8
        bcs     @2
        jmp     $F314           ; KERN_CKOUT
@2:     tax
        jsr     jiffy_listen
        lda     SECADR
        bpl     @3
        jmp     $F33A
@3:     jsr     jiffy_listensa
        jmp     $F342

jiffy_getin:
        lda     DFLTN           ; device# in
        cmp     #8
        bcs     x2
        jmp     KERN_GETIN

jiffy_basin:
        lda     DFLTN           ; device# in
        cmp     #8
        bcs     x2
        jmp     KERN_BASIN
x2:     lda     STATUS
        beq     @3
        jmp     $F268           ; KERN_BASIN
@3:     jmp     get_byte

jiffy_bsout:
        pha
        lda     DFLTO           ; device# out
        cmp     #8
        bcs     @2
        jmp     $F27B           ; KERN_BSOUT
@2:     pla
        jmp     send_byte

jiffy_clall:
        lda     #0
        sta     $98
jiffy_clrch:
        ldx     #3
        cpx     DFLTO           ; device# out
        bcs     @1
        jsr     jiffy_unlisten
@1:     cpx     DFLTN           ; device# in
        bcs     @2
        jsr     jiffy_untalk
@2:     jmp     $f403           ; KERN_CLRCH

.endif ; SJ20_IO
