;;;
;;; sj20
;;;

        .include "cbm_kernal.inc"
        .include "vic20.inc"

        .export __LOADADDR__: absolute = 1
        .segment "LOADADDR"
        .addr   *+2

        .import sj20_init

        .segment "CODE"

PTRSTR          := $CB1E        ; Print zero terminated sting

        jsr     sj20_init
        lda     #<banner
        ldy     #>banner
        jsr     PTRSTR
        rts

banner: .byte "sj20 activated", 13, 0
