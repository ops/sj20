;;;
;;; sj20
;;;

        .setcpu "6502"

        .include "cbm_kernal.inc"
        .include "vic20.inc"

        .export __LOADADDR__: absolute = 1
        .segment "LOADADDR"
        .addr   *+2

        .segment "CODE"


PT3             = $14           ;Pointer

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

SY_STROUT       := $cb1e        ; String in AC/YR ausgeben
FRMNUM          := $cd8a        ; GET NUMERIC VALUE
FRMBYTE         := $d79e        ; GET BYTE VALUE TO X
CNVWORD         := $d7f7        ; CONVERT TO WORD VALUE INTO Y/A; $14 (PT3)
RSPAUSE         := $F160        ; SET TIMER
SEROUT1         := $E4A0
SEROUT0         := $E4A9
SRCLKHI         := $EF84
SRCLKLO         := $EF8D
SERGET          := $E4B2
SRBAD           := $eeb4
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


; ==============================================================
; JIFFY PROCS
; ==============================================================

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
        sei ;????
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

        jmp     $eeb7           ;err TIME OUT   POIS!!!!!!!!!!

.proc jiffy_detect_device
        sta     VIA2_PCR
        bit     VIA1_PA2
        bpl     @out
        cpx     #$02
        bne     @out

        lda     #$02
        ldx     #$20            ; 1e??? wait for jiffy protocol
@wait:  bit     VIA1_PA2
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
        bit     $A3
        bvs     :+
        lda     #$00
        jmp     $EF1C           ; KERN_GET_BYTE
:       lda     VIA1_PA2
        and     #$03
        beq     :-

        lda     #$80
        sta     $9C

        txa
        pha

        pha
        pla

        pha
        pla

        lda     VIA2_PCR
        and     #$DD
        sta     VIA2_PCR
        ora     #$20
        tax

        bit     $9C
        bit     $9C
        bit     $9C

        lda     VIA1_PA2	; get bit 0 & 1
        ror			; bit 0 (clock) -> bit 7
        ror			; bit 1 (data ) -> carry
        nop
        and     #$80		; mask received bit 0
        ora     VIA1_PA2	; get bit 2 & 3
        rol			; A = .....XXX
        rol			; A = ....XXXX
        sta     $B3		; store lower nibble
        lda     VIA1_PA2	; get bit 4 & 5
        ror			; bit 4 (clock) -> bit 7
        ror			; bit 5 (data ) -> carry
        and     #$80		; mask received bit 4
        nop
        ora     VIA1_PA2	; get bit 6 & 7
        rol			; A = .....XXX
        rol			; A = ....XXXX
        sta     $C0		; store upper nibble
        lda     VIA1_PA2	; get status bits
        stx     VIA2_PCR	; data out (5) = 1
        sta     $9C		; save status bits
        jsr     jiffy_combine_nibbles
        sta     $A4		; received byte
        pla
        tax
        lda     $9C		; restore status bits
        ror			; (clock) -> bit 7
        ror			; (data ) -> carry
        bpl     l7C54		; Jiffy_Set_OK   ; clock = 0 -> OK
        bcc     lfC4f		; Jiffy_Set_EOI  ; data  = 0 -> EOI
        lda     #$42		; EOI (6) and time out (1) ($42)
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
        bit     $A3		; test to see if the device is a JiffyDOS drive
        bvs     JIFFY_OUT
        lda     $A3
        cmp     #$A0
        bcs     JIFFY_OUT
        jmp     IEC_send_byte

lfC4f:  lda     #$40		; bit 6 = EOI
        jsr     ORIOST
l7C54:  lda     $A4
l7C56:  cli
        clc
        rts

JIFFY_OUT:
        txa
        pha
        lda     $95
        lsr
        lsr
        lsr
        lsr
        tax
        lda     lFCCE,X
        pha
        txa
        lsr
        lsr
        tax
        lda     lFCCE,X
        sta     $B3
        lda     $95
        and     #$0F
        tax

        lda     #$02
:       bit     VIA1_PA2
        beq     :-

        lda     VIA2_PCR
        and     #$DD
        sta     $9C

        pha
        pla
        pha
        pla
        nop
        nop
        nop

        sta     VIA2_PCR
        pla
        ora     $9C
        nop
        sta     VIA2_PCR
        lda     $B3
        ora     $9C
        ora     $9C
        sta     VIA2_PCR
        lda     lFBBA,X
        ora     $9C
        nop
        sta     VIA2_PCR
        lda     lF39E,X
        ora     $9C
        nop
        sta     VIA2_PCR
        nop
        and     #$DD
        bit     $A3
        bmi     :+
        ora     #$02
:       sta     VIA2_PCR
        pla
        tax
        nop
        lda     $9C
        ora     #$02
        sta     VIA2_PCR
        lda     VIA1_PA2
        and     #$02
        beq     l7C56
        jmp     $EEB7           ; err TIME OUT


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


;--------------JIFFY BYTE IN


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

;#if PRINTADDRESS == 0 & PRINTMESSAGE == 1
;;;  jsr $f647 ;Print "SEARCHING"
;#endif

        lda     #$f0            ; channel
        jsr     DISK_LISTEN
        jsr     IECNAMOUT
        bcc     :+
        rts
:       ldy     #$00
        lda     (FNAM),Y
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
        ;#if LOADPARAMS == 1
        beq     sa0             ; SA=0 -->
        dex
        dex
        ;#endif
        bne     sa1             ; SA=1 -->
        ; SA=2: LOAD CARTRIDGE AT $c3
        pha
        lda     LOADEND
        pha                     ; FIRST TWO BYTES ...

        ; SA=0: LOAD PROGRAM AT $c3
sa0:    jsr     FRMWORD2        ; GET WORD VALUE
        lda     LOADPTR+1
        ldx     LOADPTR
;#if LOADPARAMS == 1
        bcs     :+
        lda     PT3+1
        ldx     PT3
:
;#endif
        sta     LOADEND+1
        stx     LOADEND


sa1:
.ifdef SJ20_EXT_MESSAGES
        jsr     PRINT_ATADR
.endif ; SJ20_EXT_MESSAGES

;#else
;#if PRINTMESSAGE == 1
;;;  jsr $f66a ;Print "LOADING / VERIFYING"
;#endif
;#endif

        ldx     SECADR

;#if LOADPARAMS == 1

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
;#endif

;--------------JIFFY FASTLOAD INIT
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

;--------------JIFFY FASTLOAD INIT
FB1F:   jsr     jiffy_untalk
        lda     #$61
        jsr     DISK_TALK
;--------------JIFFY FASTLOAD staRT
        sei
        lda     $B2
        pha
        ldy     #$00
FB25:   jsr     $F755           ;STOP Taste abfragen
        cmp     #$FE
        beq     FB5B
        lda     VIA2_PCR
        and     #$DD
        tax
        ora     #$20
        sta     $B2
        stx     VIA2_PCR
        lda     #$80
        sta     $9C

:       lda     VIA1_PA2
        lsr
        bcc     :-

	and     #$01
        beq     FB67

	ldx     #$6D
:       bit     VIA1_PA2
        beq     FB54
        dex
        bne     :-

	lda     #$42
        .byte   $2c
FB54:   lda     #$40
        jsr     ORIOST
        clc
        .byte   $24
FB5B:   sec                     ;STOP!
        pla
        sta     $B2
        bcc     MYLO_E
err:    jmp     $F6CB           ;UNLISTEN, CLOSE, BREAK

FB67:   lda     #$02
:       bit     VIA1_PA2
        beq     :-
FB6E:   pha
        pla
        nop
        lda     $B2
        sta     VIA2_PCR
        lda     #$01
        bit     VIA1_PA2
        beq     FB25
        stx     VIA2_PCR
        lda     VIA1_PA2
        ror
        ror
        nop
        and     #$80
        ora     VIA1_PA2
        rol
        rol
        nop
        sta     $B3
        lda     VIA1_PA2
        ror
        ror
        and     $9C
        ora     VIA1_PA2
        rol
        rol
        sta     $C0
        lda     #>(FB6E-1)      ; Return address on stack
        pha
        lda     #<(FB6E-1)
        pha
        jsr     jiffy_combine_nibbles

STOREBYTE:
        cpy     VERCK
        bne     FBB0
        sta     ($AE),Y
FBA7:   inc     $AE
        bne     :+
        inc     $AF
:       rts

FBB0:   ;VERIFY
        cmp     ($AE),Y
        beq     FBA7
        lda     #$10            ;VERIFY ERROR
        sta     STATUS
        bne     FBA7

.ifdef SJ20_SAVE

; ========================================================================
; MY SAVE                   ENDADDR   = ($AE/$AF)    staRTADDR = ($C1/$C2)
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
;#if SAVEPARAMS == 1
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
;#endif

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

;#if PRINTADDRESS == 1
        lda     #LOADSTART
        jsr     PRINT_ATADR_2
;#else
;#if PRINTMESSAGE == 1
;;;  jsr $f728 ; Print "SAVING"
;#endif
;#endif

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
        jsr     $fd1b           ;incR ADDR
        bne     MYSA_00
MYSA_E0:
        jsr     jiffy_unlisten
        jsr     DISK_CLOSE_SA

;#if PRINTADDRESS == 1
        lda     #LOADSTART
        jsr     PRINT_TOADR_2
;#endif
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

;#if LOADPARAMS == 1 | SAVEPARAMS == 1

        ; GET WORD VALUE IN Y/A and (PT3)
FRMWORD2:
        jsr     CHKCOM
        bcs     FRWO_3
FRMWORD:
        jsr     FRMNUM
        jsr     CNVWORD
        clc
FRWO_3: rts
        ;#endif

CHKCOM:
        jsr     CHRGOT
        cmp     #','
        sec
        bne     :+
        jsr     CHRGET
        clc
:       rts


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
        jsr     SY_STROUT
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
        jsr     SY_STROUT
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
