;The Turbo-Assembler source
;---------------------------------------
;- monitor by Polonus
;to be used by Padua-Members,friends,etc
;---------------------------------------

         *= $6000     ;adres startu
         jmp jump
         .text "TKI16"
sttext   lda #<tk     ;drukuj naglowek
         ldy #>tk
         jmp new_ab1e
tk       ;.byte $93,$12,$0e,$05
         .byte 13,10
         .text "MasterMon (C) KEBAB 1992"
         ;.byte 13,10
         .text " by Polonus/Padua wersja 4.1"
         .byte 13,10
         .text "fit to Multicomp by Moneo in 2016"
         .byte 13,10
         .text "Legends never die."
         .byte 13,10
         ;---- wiadomosc dnia: ---------------
         .text "wiadomosc dnia:"
         .byte 13,10
         .text "pamietaj logery nie loguja $8000 i $9000"
         .byte 13,10
         .text "procedury nieuzywane sa wyciete"
         .byte 13,10
         .text "wielkie A assembluje, reszta male"
         .byte 13,10,13,10
         .byte 0
rtxt     .byte $0d
         .text "-pc- ac xr yr sp nv-bdizc irq. FFD5 FFD6 FFD7"
         .byte $0d,$0a,0
         
jump     

         
start    cld         ;kasuj decymalny
         jsr ACIA_Init
         ;jsr asnt10 ; lda #$37 sta 1
         jsr sttext  ;wydrukuj naglowek
         jsr raport
         
         sei
         ldx #$ff
         txs
         jsr zachowaj_ram         

renew    
;        lda #<break ;ustaw wektor
;        sta $0316   ;software-break
;        lda #>break
;        sta $0317
         jsr trpc
         jsr ret
petla         
         jsr cf_lf    
         
         ldx #$00
         stx $6d

petl1    jsr new_ffcf   ;pobierz tekst
         cmp #8
         bne  petl1b      
         
         jsr Put_Chr
         ;lda #"~"
         ;jsr Put_Chr        

         cpx #$00
         beq petl1c
         dex     
petl1c         
         ;lda #$20
         ;sta field,x               
         jmp petl1
petl1b        

         cmp #27        ; esc
         bne petla1b
         jmp petla             
petla1b   
         sta field,x
         ;jsr loger2     ;<----- LOGGER
         jsr Put_Chr
         cmp #$0d
         beq petl2
         inx
         cpx #72
         bne petl1
petl2    cpx #$00
         beq petla   ;"pusty" return
         
         
         ldx $6d
petl3    lda field,x
         inx
         stx $6d
         cmp #$20
         beq petl3
         ldx #$00    ;czy to rozkaz?
rec      cmp ordname,x
         beq ok
         inx
         cpx #ordlo-ordname
         bcc rec
dopetla  jsr ret
         jmp petla   ;brak rozpoznania
ok       lda ordlo,x ;rozkaz - pobierz
         ;sta $60
         sta goto+1  ;adres dla skoku
         lda ordhi,x
         ;sta $61
         sta goto+2
         
         ;jsr cf_lf  ;$60/61 i reszta dla kontroli dzialania
         ;jsr cf_lf
         ;jsr hextype
         ;jsr cf_lf
         ;jsr cf_lf 
         ;jsr cf_lf
                         
goto     jsr $ffff   ;wykonaj rozkaz
         jmp dopetla
;------------------------------------------------------------
ordname  .text "xrdm_:[<ft;aji@lsh*$k"
         .text "!SLgq"
         .byte 0
ordlo    .byte <exit  ;x
         .byte <rap   ;r
         .byte <dis   ;d
         .byte <mon   ;m
         .byte <romsw ;_
         .byte <ent   ;:
         .byte <asnt  ;[
         .byte <scnt  ;<
         .byte <fill  ;f
         .byte <trsf  ;t
         .byte <sredn ;;  
         .byte <sredn ;a
         .byte <subr  ;j
         .byte <show  ;i 
         .byte <dos   ;@ 
         .byte <loadget ;l
         .byte <save  ;s
         .byte <hunt  ;h
         .byte <takerap;*  
         .byte <dolc  ;$
         .byte <help  ;k
         .byte <digi  ;!
         .byte <psave ;S
         .byte <pload ;L
         .byte <subr  ;g
         .byte <irqquit ;q

ordhi    .byte >exit,>rap,>dis,>mon
         .byte >romsw,>ent,>asnt
         .byte >scnt,>fill,>trsf
         .byte >sredn,>sredn,>subr
         .byte >show,>dos,>loadget
         .byte >save,>hunt,>takerap
         .byte >dolc,>help
         .byte >digi,>psave,>pload
         .byte >subr,>irqquit
;---------------------------------------
irqquit
pload  
         rts
         ;sei
         ;jsr $fda3
         ;jsr $fd15
         ;jsr $e518
         ;pla
         ;pla
         ;jmp start
;--------------------------------------         
cf_lf
          lda #13
          jsr Put_Chr
          lda #10
          jmp Put_Chr
          
new_ab1e
          sta new_ab1e1+1
          sty new_ab1e1+2
          
          ldy #0
new_ab1e1          
          lda $ffff,y
          beq new_ab1e_exit
          jsr Put_Chr
          iny
          bne new_ab1e1
new_ab1e_exit          
          rts
;------------------------------------------------------------
new_ffcf
          jsr Get_Chr
          bcc new_ffcf
          ;jsr loger    ;<----- LOGGER
          rts
new_ffd2
          jmp Put_Chr
new_ffe1
          JSR	Get_Chr
          BCC	NotCTRLC  ; jesli nic nie nacisniete to nienacisniete
          CMP	#3
          BNE	NotCTRLC  ; if CTRL-C not pressed then exit
          		        ; Carry set if control C pressed
          LDA #$00
          SEC
          RTS
NotCTRLC
          LDA #1		; Carry clear if control C not pressed
          CLC
          RTS          
;------------------------------------------------------------      
ACIA        = $FFD0
ACIAControl = ACIA+0
ACIAStatus  = ACIA+0
ACIAData    = ACIA+1    
                 
ACIA_Init
          LDA #$95		; Set ACIA baud rate, word size and Rx interrupt (to control RTS)
          STA	ACIAControl
          RTS

;------------------------------------------------------------
Get_Chr
          LDA	ACIAStatus
          AND	#1
          CMP	#1
          BNE	NoDataIn
          LDA	ACIAData
          SEC		; Carry set if key available
          RTS
NoDataIn
          CLC		; Carry clear if no key pressed
          RTS
;------------------------------------------------------------
Put_Chr
                PHA
SerialOutWait
                LDA	ACIAStatus
                AND	#2
                CMP	#2
                BNE	SerialOutWait
                PLA
                STA	ACIAData
                RTS
;------------------------------------------------------------ 
field    = $0100      ;pole rozp.znakow
aval     = $43        ;rejestr A
xval     = $44        ;rejestr B
yval     = $45        ;rejestr C
flags    .byte $21    ;stan flag
rom      .byte $34    ;wskaznik $01
pc       .byte <jump,>jump ;prg.counter
;---------------------------------------
; tabela pierwszych liter nazw rozkazow:
;
firstlet .byte $42,$4f,$53,$41,$53,$41
         .byte $50,$53,$42,$43,$55,$4a
         .byte $41,$52,$42,$52,$50,$42
         .byte $53,$52,$45,$4c,$50,$4c
         .byte $41,$4a,$42,$43,$52,$41
         .byte $52,$52,$50,$41,$42,$53
         .byte $53,$41,$53,$53,$44,$54
         .byte $58,$42,$54,$54,$4c,$4c
         .byte $4c,$4c,$54,$54,$4f,$42
         .byte $43,$54,$43,$43,$44,$44
         .byte $49,$44,$53,$42,$43,$43
         .byte $53,$49,$49,$49,$4e,$42
         .byte $53
;---------------------------------------
; tabela drugich liter nazw rozkazow:
;
seclet   .byte $52,$52,$54,$53,$4b,$53
         .byte $48,$4b,$50,$4c,$4e,$53
         .byte $4e,$4c,$49,$4f,$4c,$4d
         .byte $45,$54,$4f,$53,$48,$53
         .byte $4c,$4d,$56,$4c,$54,$44
         .byte $52,$4f,$4c,$52,$56,$45
         .byte $54,$58,$54,$54,$45,$58
         .byte $41,$43,$59,$58,$44,$44
         .byte $44,$41,$41,$41,$41,$43
         .byte $4c,$53,$50,$4d,$43,$45
         .byte $4e,$45,$41,$4e,$4c,$50
         .byte $42,$4e,$4e,$4e,$4f,$45
         .byte $45
;---------------------------------------
; tabela trzecich liter nazw rozkazow:
;
thirdlet .byte $4b,$41,$50,$4f,$42,$4c
         .byte $50,$57,$4c,$43,$50,$52
         .byte $44,$41,$54,$4c,$50,$49
         .byte $43,$49,$52,$45,$41,$52
         .byte $52,$50,$43,$49,$53,$43
         .byte $41,$52,$41,$52,$53,$49
         .byte $41,$53,$59,$58,$59,$41
         .byte $41,$43,$41,$53,$59,$41
         .byte $58,$58,$59,$58,$4c,$53
         .byte $56,$58,$59,$50,$4d,$43
         .byte $59,$58,$58,$45,$44,$58
         .byte $43,$53,$43,$58,$50,$51
         .byte $44
;---------------------------------------
; tabela dekodowania nazw rozkazow:
;
cname    .byte $00,$01,$02,$03,$04,$01
         .byte $05,$03,$06,$01,$05,$03
         .byte $07,$01,$05,$03,$08,$01
         .byte $02,$03,$04,$01,$05,$03
         .byte $09,$01,$0a,$03,$07,$01
         .byte $05,$03,$0b,$0c,$02,$0d
         .byte $0e,$0c,$0f,$0d,$10,$0c
         .byte $0f,$0d,$0e,$0c,$0f,$0d
         .byte $11,$0c,$02,$0d,$04,$0c
         .byte $0f,$0d,$12,$0c,$0a,$0d
         .byte $07,$0c,$0f,$0d,$13,$14
         .byte $02,$15,$04,$14,$17,$15
         .byte $16,$14,$17,$18,$19,$14
         .byte $17,$15,$1a,$14,$02,$15
         .byte $04,$14,$17,$15,$1b,$14
         .byte $0a,$15,$07,$14,$17,$15
         .byte $1c,$1d,$02,$1e,$04,$1d
         .byte $1f,$1e,$20,$1d,$1f,$21
         .byte $19,$1d,$1f,$1e,$22,$1d
         .byte $02,$1e,$04,$1d,$1f,$1e
         .byte $23,$1d,$0a,$1e,$07,$1d
         .byte $1f,$1e,$04,$24,$04,$25
         .byte $26,$24,$27,$25,$28,$02
         .byte $29,$2a,$26,$24,$27,$25
         .byte $2b,$24,$02,$02,$26,$24
         .byte $27,$25,$2c,$24,$2d,$02
         .byte $02,$24,$02,$02,$2e,$2f
         .byte $30,$31,$2e,$2f,$30,$31
         .byte $32,$2f,$33,$34,$2e,$2f
         .byte $30,$31,$35,$2f,$02,$31
         .byte $2e,$2f,$30,$31,$36,$2f
         .byte $37,$02,$2e,$2f,$30,$31
         .byte $38,$39,$04,$3a,$38,$39
         .byte $3b,$3a,$3c,$39,$3d,$3e
         .byte $38,$39,$3b,$3a,$3f,$39
         .byte $02,$3a,$04,$39,$3b,$3a
         .byte $40,$39,$0a,$3a,$07,$39
         .byte $3b,$3a,$41,$42,$04,$43
         .byte $41,$42,$44,$43,$45,$42
         .byte $46,$02,$41,$42,$44,$43
         .byte $47,$42,$02,$43,$04,$42
         .byte $44,$43,$48,$42,$0a,$43
         .byte $07,$42,$44,$43
;---------------------------------------
; tabela dopuszczalnych trybow adresowa-
; nia poszczegolnych rozkazow:
;
ctype    .byte $00,$11,$00,$11,$70,$77
         .byte $77,$77,$00,$09,$00,$99
         .byte $80,$88,$88,$88,$bb,$22
         .byte $00,$02,$30,$33,$a3,$a3
         .byte $00,$55,$00,$05,$00,$44
         .byte $04,$04,$98,$11,$90,$11
         .byte $77,$77,$77,$77,$00,$99
         .byte $00,$99,$88,$88,$88,$88
         .byte $bb,$22,$00,$22,$30,$33
         .byte $a3,$33,$00,$55,$00,$05
         .byte $40,$44,$54,$54,$90,$11
         .byte $00,$11,$70,$77,$77,$77
         .byte $00,$99,$00,$99,$88,$88
         .byte $88,$88,$bb,$22,$00,$22
         .byte $00,$33,$33,$33,$00,$55
         .byte $00,$55,$00,$44,$44,$44
         .byte $90,$11,$00,$11,$70,$77
         .byte $77,$77,$00,$99,$00,$09
         .byte $86,$88,$88,$88,$bb,$22
         .byte $00,$22,$00,$33,$33,$33
         .byte $00,$55,$00,$55,$00,$44
         .byte $44,$44
;---------------------------------------
hextype  lda $61      ;procedura druku
         jsr bytetype ;slowa hex
         lda $60      ;procedura druku
bytetype pha          ;bajtu hex
         jsr ciepw
         jsr htype
         pla
         and #$0f
htype    cmp #$0a     ;drukuj znak hex
         bcs l1
         ora #$30
l2       jmp new_ffd2
l1       clc
         adc #$37
         bne l2
space    lda #$20     ;podprogramy druku
         bne l2       ;czesto wystepuja-
ret      lda #$0d     ;cych znakow
         bne l2
dwukr    lda #":"
         bne l2
srd      lda #";"
         bne l2
dolar    lda #"$"
         bne l2
hasz     lda #"#"
         bne l2
asterix  lda #"*"
         bne l2
separ    lda #">"
         bne l2
what     lda #"?"
         bne l2
lew      lda #"("
         bne l2
praw     lda #")"
         bne l2
przec    lda #","
         bne l2
xo       lda #"x"
         bne l2
yo       lda #"y"
         bne l2
minus    lda #$2d
         bne l2
procent  lda #"%"
         bne l2
gora     lda #$91
         bne l2

bintype  sta $62      ;podprogram druku
         ldx #$07     ;bajtu w systemie
bint1    lda #$00     ;binarnym
         asl $62
         adc #$30
         jsr new_ffd2
         dex
         bpl bint1
         rts

todec    stx $68      ;podprogram druku
         sta $69      ;adresu w systemie
         ldy #$05     ;dziesietym
todec4   lda #$00
         ldx #$11
         clc
         bcc todec2
todecb   rol a
         cmp #$0a
         bcc todec2
         sbc #$0a
todec2   rol $68
         rol $69
         dex
         bne todecb
         pha
         dey
         bne todec4
         ldy #$05
todecf   pla
         dey
         beq todec7
         cmp #$00
         beq todecf
todec7   ora #$30
         jsr new_ffd2
         dey
         bmi trpcr
         pla
         jmp todec7

chartype sty char+1   ;druk odpowiednika
         tay          ;graficznego poda-
         lda $c7      ;nej wartosci A
         pha
         tya
         and #$7f
         cmp #$20
         bcs char2
         ldy #$01
         sty $c7
         clc
         adc #$40
char2    jsr new_ffd2
         ldy #$00
         sty $d4
char     ldy #$00
         pla
         sta $c7
         rts
trpc     lda pc
         sta $60
         lda pc+1
         sta $61
trpcr    rts

;--------------------------------------------
raport   lda #<rtxt   ;raport monitora
         ldy #>rtxt
         jsr new_ab1e
         jsr trpc
         jsr hextype
         ldx #$00
r1       jsr space
         lda aval,x
         jsr bytetype
         inx
         cpx #$03
         bcc r1
         jsr space
         tsx
         txa
         jsr bytetype
         jsr space
         lda flags
         jsr bintype
         jsr space
         lda $ffff ;lda $0315
         jsr bytetype
         lda $fffe ;lda $0314
         jsr bytetype
         jsr space
         jsr space
         lda $ffd5 ;lda rom
         jsr bytetype
         jsr space
         jsr space
         jsr space
         lda $ffd6 ;lda rom
         jsr bytetype
         jsr space
         jsr space
         jsr space
         lda $ffd7 ;lda rom
         jsr bytetype
         rts
;---------------------------------------
exit     
         jsr odchowaj_ram
         ldx #$fb    ;powrot do Basic
         txs
         jmp $ff00        
         
         ;jsr $e3bf
         ;jmp $a474   ;skocz do Basic
;---------------------------------------
rap      jsr trpc    ;rozkaz "R"
         jmp raport
;---------------------------------------
count    .byte 1,2,2,2,3,3,3,2,3,2,2,2
;---------------------------------------

pole     = $0e
set      pha
         lda pole+1
         sta pole
         lda pole+2
         sta pole+1
         lda pole+3
         sta pole+2
         pla
         sta pole+3
         rts
pack     = pole+4
get      lda #$00     ;pobierz wartosc
         sta $64      ;liczbowa w HEX
         sta $65
         sta pole+3
         lda #$30
         sta pole
         sta pole+1
         sta pole+2
get0     ldx $6d
         lda field,x
         cmp #$0d
         bne get1
error    sec          ;blad pobrania
         rts
get1     cmp #$2c     ;","
         bne get2
get3     inc $6d
         bmi error
         bpl get0
get2     cmp #$20
         beq get3
get5     ldx $6d
         lda field,x
         cmp #$30
         bcc ee
         cmp #"g"
         bcs ee
         cmp #$3a
         bcc get4
         cmp #$41
         bcc ee
get4     jsr set
         inc $6d
         jmp get5
ee       lda pole+3
         beq error
         lda pole
         jsr change
         asl a
         asl a
         asl a
         asl a
         sta $65
         lda pole+1
         jsr change
         ora $65
         sta $65
         lda pole+2
         jsr change
         asl a
         asl a
         asl a
         asl a
         sta $64
         lda pole+3
         jsr change
         ora $64
         sta $64
         tax
         ldy $65
         clc
         rts
change   bne chan0
         lda #$30
chan0    cmp #$41
         bcs chan1
         sec
         sbc #$30
         rts
chan1    sec
         sbc #$37
         rts
;---------------------------------------
;nie jest procedura gupioodporna         
niu_get
              
        jsr get_field
        sta pole      
        jsr get_field
        tay
        lda pole
        jsr Proc_ASCII
        sta $61

        jsr get_field
        sta pole      
        jsr get_field
        tay
        lda pole
        jsr Proc_ASCII
        sta $60       

        rts 
        
get_field
            ldx $6d
get_field_1
            lda field,x
            inx
            stx $6d
            cmp #$20        ;jak spacja to zignoruj
            bne get_field_2 
            beq get_field_1
get_field_2            
            cmp #$2c         ;","
            bne get_field_3
            beq get_field_1
get_field_3
            cmp #$08
            bne get_field_4
            beq get_field_1
get_field_4                                  
            rts
            
;usage:
;lda #$41
;ldy #$31
;jsr Proc_ASCII
;sta xxxx  (A=$a1)
;---
Proc_ASCII
      CLC
      JSR st
      ASL
      ASL
      ASL
      ASL
      STA byteFB
      TYA
      JSR st
      ORA byteFB
      RTS
st
      CMP #$40
      BCC stl1
      ADC #$08
stl1
      AND #$0f
      rts
byteFB = pole+1
;---------------------------------------
mon      
         ldx $6d
         lda field,x
         cmp #$0d         ;je¿eli samo m
         beq mon1         ;to kontynuacja
           
         jsr niu_get      ;rozkaz "M"
         ;bcs mon1        ;pobierz adres
         ;stx $60
         ;sty $61
mon1     ;ldx $6d
         ;lda field,x
         ;cmp #$2d     ;druk ciagly?
         ;bne mon2
         

         ;lda #$00
         ;.byte $2c
mon2     ;lda #$01
         ;sta $62
         lda #0
         sta $62
         lda #$16
         sta $63
gain     jsr ret
         jsr dwukr
         jsr hextype
         jsr space
         jsr romset
         ldy #$07
mon3     lda ($60),y
         sta field,y
         dey
         bpl mon3
         jsr asnt10
         ldx #$00
mon4     lda field,x
         jsr bytetype
         jsr space
         inx
         cpx #$08
         bne mon4
         ;lda #$0f
         ;sta $0286
         jsr separ
         ldx #$00
mon5     lda field,x
         jsr chartype
         inx
         cpx #$08
         bne mon5
         lda $60
         clc
         adc #$08
         sta $60
         bcc monek
         inc $61
monek    
         jsr cf_lf
         ;lda #$01
         ;sta $0286
         jsr new_ffe1
         bne moon+1
moon     rts
         lda $62
         cmp #1
         bne mon6
         dec $63
         bmi moon
         jmp gain
mon6     
        inc $62
        jmp gain
;---------------------------------------
romsw    
         jsr ret    ;rozkaz "_"
         jsr gora
         lda #$1d
         jsr new_ffd2
         lda #$12
         jsr new_ffd2
         lda #"r"
         jsr new_ffd2
         lda #"o"
         jsr new_ffd2
         lda #"m"
         jsr new_ffd2
         jsr dwukr
         jsr dolar
         ldx rom
         inx
         txa
         and #$07
         ora #$30
         sta rom
         jsr bytetype
         jmp gora
;---------------------------------------
dis      
         jsr cf_lf
         ldx $6d
         lda field,x
         cmp #$0d         ;je¿eli samo d
         beq dis00         ;to kontynuacja
                   
         jsr niu_get      ;disassemblacja pobierz adres
dis00         
         ;jsr get     ;disassemblacja
         ;bcs dis0    
         ;stx $60
         ;sty $61
         lda #8
         sta $62
dis0     ;ldx $6d
         ;lda field,x
         ;cmp #$2d    ;czy ma byc ciagla?
         ;bne dis2
dis1     ;lda #$00    ;tak
         ;.byte $2c
dis2     ;lda #$01    ;nie
         ;sta $62
dis3     lda #$16
         sta $63
again    jsr ret     ;return
         ;jsr cf_lf
         jsr srd     ;srednik
         jsr hextype ;adres
         jsr space   ;spacja
         ;jsr romset  ;wylacz ROM
         ldy #$02
dis4     lda ($60),y ;pobierz 3 bajty
         sta field,y
         dey
         bpl dis4
         jsr asnt10
         ldx field
         jsr gettype ;rodzaj adresowania
         tay
         lda count,y ;jaki dlugi rozkaz
         sta dis8+1
         sty dityp+1
         jsr space   ;spacja
         ldx field
         ldy cname,x
         lda firstlet,y
         jsr new_ffd2   ;drukuj 1 litere
         lda seclet,y
         jsr new_ffd2   ;drukuj 2 litere
         lda thirdlet,y
         jsr new_ffd2   ;drukuj 3 litere
         jsr space   ;spacja
dityp    ldx #$00
         jsr didruk  ;drukuj tryb adres.
doitag   jsr space   ;spacja
         lda $d3
         cmp #$14
         bcc doitag
         ;lda #$0f    ;zmien kolor
         ;sta $0286
         lda #$09
         jsr Put_Chr
         lda #$09
         jsr Put_Chr         
         jsr separ   ;drukuj separator
         ldx dis8+1
         ldy #$00
dirt     lda field,y ;drukuj bajty
         jsr bytetype
         jsr space   ;odstep
         iny
         dex
         bne dirt
         lda #$1e
         sta $d3
         jsr separ
         ldx #$00
dis7     lda field,x ;drukuj znaki
         jsr chartype
         inx
dis8     cpx #$00
         bne dis7
         lda $60
         clc
         adc dis8+1
         sta $60
         bcc dis999
         inc $61
dis999   lda field
         cmp #$60     ;czy to RTS?
         bne disc
kres     jsr kreska
         jmp dis9
disc     cmp #$4c     ;czy to JMP?
         beq kres
dis9     ;lda #$01     ;ustaw kolor
         ;sta $0286
         jsr new_ffe1    ;klawisz BREAK?
         bne disa
dis99    rts
disa     lda $62      ;druk ciagly?
         cmp #0       ;ile linii
         beq disb     ;tak

         dec $63      ;nie
         bmi dis99    ;czy koniec
disb     
         jsr cf_lf
         inc $62
         jmp again    ;nastepny rozkaz

kreska   
         jsr ret      ;drukuj kreske
         lda #11
         jsr new_ffd2
         ldx #$26
         lda #$2d
pip      jsr new_ffd2
         dex
         bpl pip
         dec $63
         rts
;---------------------------------------
didruk   cpx #$00     ;drukowanie trybow
         bne did1     ;adresowania
         rts
did1     cpx #$01
         bne did2
         jsr lew
         jsr dolar
         lda field+1
         jsr bytetype
         jsr przec
         jsr xo
         jsr praw
         rts
did2     cpx #$02
         bne did3
         jsr lew
         jsr dolar
         lda field+1
         jsr bytetype
         jsr praw
did22    jsr przec
         jsr yo
         rts
did3     cpx #$03
         bne did4
         jsr dolar
did33    lda field+1
         jsr bytetype
         jsr przec
         jsr xo
         rts
did4     cpx #$04
         bne did5
         jsr dolar
         lda field+2
         jsr bytetype
         jmp did33
did5     cpx #$05
         bne did6
         jsr dolar
         lda field+2
         jsr bytetype
did55    lda field+1
         jsr bytetype
         jmp did22
did6     cpx #$06
         bne did7
         jsr lew
         jsr dolar
         lda field+2
         jsr bytetype
         lda field+1
         jsr bytetype
         jmp praw
did7     cpx #$07
         bne did8
         jsr dolar
did77    lda field+1
         jmp bytetype
did8     cpx #$08
         bne did9
         jsr dolar
         lda field+2
         jsr bytetype
         jmp did77
did9     cpx #$09
         bne did10
         jsr hasz
         jsr dolar
         jmp did77
did10    cpx #$10
         bne did11
         jsr dolar
         jmp did55
did11    lda $60
         sta $64
         jsr dolar
         lda $61
         sta $65
         lda field+1
         bmi did12
         clc
         adc #$02
         clc
         adc $60
         sta $60
         bcc did111
         inc $61
did111   jsr hextype
         lda $64
         sta $60
         lda $65
         sta $61
         rts
did12    eor #$ff     ;obliczanie adresu
         sta dido+1   ;dla rozkazow
         lda $60      ;typu BRACH
         sec
dido     sbc #$00
         sta $60
         bcs did001
         dec $61
did001   inc $60
         bne plums
         inc $61
plums    jmp did111
;---------------------------------------
ent      jsr get      ;rozkaz ":" -
         bcc ent1     ;wprowadzanie
         jmp what     ;danych
ent1     stx $60
         sty $61
         lda #$00
         sta $62
ent0     jsr get
         bcs ent2
         ldx $62
         sta field,x
         inc $62
         lda $62
         cmp #$08
         bcc ent0
ent2     lda $62
         beq ent3
         jsr romset
         ldy #$00
ent4     lda field,y
         sta ($60),y
         iny
         cpy $62
         bne ent4
         jsr asnt10
ent3     ldx #$00
         stx $63
         inx
         stx $62
         jsr gora
         jmp gain
;---------------------------------------
asnt     lda #$ff     ;wprowadzaj tekst
         .byte $2c    ;w ASCII
scnt     lda #$3f     ;lub w ScreenCode
         sta asnt5+1
         jsr get
         bcc asnt1
         jmp what
asnt1    stx $60
         sty $61
         ldy #$00
         ldx $6d
asnt0    lda field,x
         cmp #$0d
         bne asnt2
         jmp what
asnt2    cmp #$22
         beq asnt3
         inx
         bpl asnt0
         jmp what
asnt3    inx
         jsr romset
asnt4    lda field,x
         cmp #$22
         beq asnt10
         cmp #$0d
         beq asnt10
asnt5    and #$ff
         sta ($60),y
         iny
         inx
         bpl asnt4
asnt10   ;lda #$37
         ;sta $01
         cli
         rts
;---------------------------------------
fill     jsr get      ;rozkaz Fill
         bcc fill1
err      jmp what     ;brak adr.startu
fill1    
         stx $60
         sty $61
         jsr get
         bcs err      ;brak adr.konca
         stx $68
         sty $69
         jsr get      ;brak wart.wypeln.
         bcs err
         stx $62
fill2
         jsr romset
         ldy #$00
         lda $62
         sta ($60),y
         jsr asnt10
         jsr new_ffe1
         beq fill4
         inc $60
         bne fill3
         inc $61
fill3    lda $61
         cmp $69
         bne fill2
         lda $60
         cmp $68
         bne fill2
fill4    rts
;---------------------------------------
trsf     jsr get      ;rozkaz Transfer
         bcs err
         stx $60
         sty $61
         jsr get
         bcs err
         stx $68
         sty $69
         jsr get
         bcs err
         stx $6a
         sty $6b
         cpy $61
         bcc solv1
         cpx $60
         bcc solv1
         dec $61      ;przepisywanie
         dec $69      ;"od konca"
         lda $68
         sta $66
         lda $69
         sta $67
         sec
         sbc $61
         sta $67
         lda $66
         sec
         sbc $60
         sta $66
         bcs trsf1
         dec $67
trsf1    lda $6a
         clc
         adc $66
         sta $6a
         bcc trsf2
         inc $6b
trsf2    lda $6b
         clc
         adc $67
         sta $6b
         dec $6b
         jsr romset
         ldy #$ff
trsf0    lda ($68),y
         sta ($6a),y
         lda $68
         bne trsf3
         dec $69
trsf3    dec $68
         lda $6a
         bne trsf4
         dec $6b
trsf4    dec $6a
         lda $69
         cmp $61
         bne trsf0
         lda $68
         cmp $60
         bne trsf0
         jmp asnt10
solv1                 ;przepisywanie
         jsr romset   ;"od poczatku"
         ldy #$00
solv0    lda ($60),y
         sta ($6a),y
         inc $60
         bne solv2
         inc $61
solv2    inc $6a
         bne solv3
         inc $6b
solv3    lda $61
         cmp $69
         bcc solv0
         lda $60
         cmp $68
         bcc solv0
         jmp asnt10
;---------------------------------------
nr       = $5e
rozk     = $c0
int      ldx $6d      ;iterpretuj tekst
         ldy #$48     ;jako rozkaz 6510
int1     lda field,x
         cmp firstlet,y
         bne int0
         lda field+1,x
         cmp seclet,y
         bne int0
         lda field+2,x
         cmp thirdlet,y
         beq int2
int0     dey
         bpl int1
errint   sec
         rts
int2     sty nr       ;testuj tryb
         inx          ;adresowania
         inx
int3     inx
         stx $6d
         lda field,x
         cmp #$20
         beq int3
         cmp #"("
         beq int4
         cmp #"$"
         beq int5
         cmp #"#"
         beq int6
         lda #$00
         beq inno

int4     inx
         lda field,x
         cmp #$24
         bne errint
         inc $6d
         inc $6d
         jsr get
zupp     bcs errint
         stx rozk+1
         sty rozk+2
         ldx $6d
         lda field,x
         cmp #")"
         bne innt
         inx
         lda field,x
         cmp #","
         bne inn1
         inx
         lda field,x
         cmp #"y"
         beq inn2
         bne errint

inn1     lda #$06
inno     sta nr+1
         jmp search
inn2     lda #$02
         bne inno
innt     inx
         lda field,x
         cmp #"x"
         bne errint
         lda #$01
         bne inno
int6     inx
         stx $6d
         lda field,x
         cmp #$24
         bne errint
         inc $6d
         jsr get
         bcs zupp
         stx rozk+1
         lda #$09
         bne inno
int5     inc $6d
         inx
         stx cytat+1
         jsr get
         bcs zupp
         stx rozk+1
         sty rozk+2
         ldx nr
         lda firstlet,x
         cmp #$42
         bne int51
         ldy seclet,x
         cpy #$49
         beq int51
         lda #$0b
         bne inno
int51    cmp #"j"
         bne int52
cyt1     lda #$08
cyt2     bne inno
int52    lda $6d
         sec
cytat    sbc #$00
         cmp #$03
         bcs int53
         lda #$00
         .byte $2c
int53    lda #$01
         sta $6c
         ldx $6d
         lda field,x
         cmp #","
         beq int54
         lda $6c
         beq int55
         jmp cyt1
int55    lda #$07
         bne cyt2
int54    inx
         lda field,x
         cmp #"x"
         beq int60
         cmp #"y"
         beq int70
         jmp errint
int60    lda $6c
         beq int61
         lda #$04
cyto     jmp inno
int61    lda #$03
         bne cyto
int70    lda $6c
         beq int71
         lda #$05
         bne cyto
int71    lda #$0a
         bne cyto
search   ldx #$00
ser0     lda cname,x
         cmp nr
         bne ser1
         jsr gettype
         cmp nr+1
         beq ser2
ser1     inx
         bne ser0
         jmp errint
ser2     stx rozk
         clc
         rts
;---------------------------------------
sredn    jsr niu_get      ;rozkaz ";"
         ;bcc asso
         ;jmp what
asso     ;stx $60
         ;sty $61
         ldx $6d
         dex
sred1    inx
         bpl asr
assrr    jmp what
asr      stx $6d
         lda field,x
         cmp #$20
         beq sred1
         stx $6d
         jsr int
         bcs assrr
         lda nr+1
         cmp #$0b
         bne sred4
         jsr branch
sred4    sei
         ldx nr+1
         lda count,x
         sta $6c
         ;jsr romset
         ldy #$00
sred5    lda rozk,y
         sta ($60),y
         iny
         cpy $6c
         bcc sred5
         jsr asnt10
         ldx #$01
         stx $62
         dex
         stx $63
         stx $d4
         ;jsr gora
         jsr again
         ;jsr ret
         jsr cf_lf
         
         ;lda #$0d
         ;jsr Put_Chr
         
         jsr srd
         jsr hextype
         jsr space
         jsr space
         ;jsr gora
         ;lda #$1d
         ;ldx #$06     ;uzycie "dynami-
dkey     ;sta $0277,x  ;cznej klawiatury"
         ;dex
         ;bpl dkey
         lda #$07
         sta $c6
 
         tsx  ;zebranie ze stosu skoku do tej petli
         dex
         dex
         txs
                 
         ldx #0
         stx $6d
dkey1
         lda #";"
         sta field,x
         sta $2000,x
         inx
         
         lda $61
         jsr bhex
         pha
         tya
         sta field,x
         sta $2000,x
         inx
         pla
         sta field,x
         sta $2000,x
         inx

         lda $60
         jsr bhex
         pha
         tya
         sta field,x
         sta $2000,x
         inx
         pla
         sta field,x
         sta $2000,x
         inx
         
         lda #$20
         sta field,x
         sta $2000,x
         inx        

         jmp petl1

bhex         
         PHA
         LSR A
         LSR A
         LSR A
         LSR A
         JSR bHEX2
         tay
         PLA
         AND #$0F
bHEX2    CMP #$0A
         BCC bHEX3
         ADC #$06
bHEX3    ADC #$30
         rts
                 
                  
branch   lda $60      ;obliczanie arg.
         sec          ;dla BRANCH
         sbc rozk+1
         clc
         adc #$01
         eor #$ff
         sta rozk+1
         rts
;---------------------------------------
subr     jsr trpc     ;rozkaz "J" - skok
         ;jsr get      ;pod podany adres
         ;bcs subr1
         ;stx $60
         ;sty $61
         jsr niu_get
subr1    lda $60
         sta subr0+1
         sta pc
         lda $61
         sta subr0+2
         sta pc+1
         lda flags
         pha
         lda aval
         ldx xval
         ldy yval
         plp
subr0    jsr $ffff
         php
         cld
         sta aval
         stx xval
         sty yval
         pla
         sta flags
         jsr asnt10
         jmp rap
;---------------------------------------
show     
         jsr niu_get      ;rozkaz "I"
         ;bcs show2
         ;stx $60
         ;sty $61
show2    ;ldx $6d
         ;lda field,x
         ;cmp #$2d     ;ciagle drukowac?
         ;beq show3
         ;lda #$01
         ;.byte $2c
show3    lda #$00
         sta $62
         lda #$16
         sta $63
ragain   
         jsr cf_lf
         jsr hextype
         jsr space
         jsr romset
         ldy #$1f
show4    lda ($60),y
         sta field,y
         dey
         bpl show4
         jsr asnt10
         ldy #$00
show5    lda field,y
         jsr chartype
         iny
         cpy #$20
         bcc show5
         lda $60
         clc
         adc #$20
         sta $60
         bcc show6
         inc $61
show6    jsr new_ffe1
         bne show7
         rts
show7    lda $62
         beq ragain
         dec $63
         bpl ragain
         rts
;---------------------------------------
dos      
         rts
 
;---------------------------------------

;---------------------------------------

loadget
         
         lda #<load_kom     ;drukuj naglowek
         ldy #>load_kom
         jsr new_ab1e
         jsr vsl
         rts
load_kom
         .byte 13,10
         .text "enter ascii code:"
         .byte 13,10,0
         
         

;---------------------------------------


;---------------------------------------
brtxt    .byte $0d,$05
         .text "b*"
         .byte 0
break    cld          ;obsluga software-
         ;jsr asnt10   ;-break
         pla
         sta yval
         pla
         sta xval
         pla
         sta aval
         pla
         sta flags
         pla
         sec
         sbc #$02
         sta pc
         pla
         sbc #$00
         sta pc+1
         lda #<brtxt
         ldy #>brtxt
         jsr new_ab1e
         jsr raport
         jmp renew
;---------------------------------------
hunc     jsr get      ;rozkaz Hunt
         bcc hunt1
hunterr  pla
         pla
         jmp what
hunt1    stx $60
         sty $61
         jsr get
         bcs hunterr
         stx $68
         sty $69
         rts
hunt     jsr hunc
         ldy #$00
         sty nr
hunt0    jsr get
         bcs hcont
         txa
         ldy nr
         sta field,y
         inc nr
         bpl hunt0
         bmi hunterr
hcont    ldy nr
         cpy #$00
         beq hunterr
vol00    dey
         sty nr
         jsr ret
hunt88
         jsr romset
         ldy nr
hunt9    lda ($60),y
         cmp field,y
         bne hunt10
         dey
         bpl hunt9
         jmp huntok
hunt10   jsr incr60
         bcc hunt88
         cpx $68
         bcc hunt88
         jmp asnt10
huntok   jsr asnt10
         jsr hextype
         jsr space
         jmp hunt10
takeerr  jmp what
;---------------------------------------
incr60   inc $60
         bne *+4
         inc $61
         lda $61
         ldx $60
         cmp $69
         rts
;---------------------------------------
takerap  
         rts

;---------------------------------------
dolc     jsr get      ;przelicz z hex
         ;bcs takeerr  ;na dec i bin
         txa          ;(rozkaz $xxxx)
         pha
         tya
         pha
         jsr hasz
         tya
         jsr todec
         jsr space
         jsr procent
         pla
         jsr bintype
         jsr space
         pla
         jmp bintype
;---------------------------------------
romset   rts
         ;sei
         ;lda rom      ;ustawianie wart.
         ;sta $01  ;roboczej dla $01
         ;rts
;---------------------------------------
gettype  txa
         sta $66
         and #$7f
         tax
         lda ctype,x
         ldx $66
         bpl gt1
ciepw    lsr a
         lsr a
         lsr a
         lsr a
gt1      and #$0f
         rts
;---------------------------------------
help     
         rts

;---------------------------------------
digi     
         rts

;---------------------------------------
;
;      Hello Anonym and The Rest!!!!
;      =============================
;
;---------------------------------------
odchowaj_ram
            lda $ffd6
            pha
            lda #0
            sta $ffd6
            
            ldx #0
odchowaj_ram_1
            
            lda $8000,x
            sta $0000,x
            lda $8200,x
            sta $0200,x
            lda $8300,x
            sta $0300,x
            inx
            bne odchowaj_ram_1
            pla
            sta $ffd6                     
            rts
zachowaj_ram
            lda $ffd6
            pha
            lda #$00
            sta $ffd6
                       
            ldx #0
zachowaj_ram_1           
            .byte $bd,$00,$00   ;lda $0000,X bo mi durny kompiluje...
            STA $8000,X
            lda $0200,x
            sta $8200,x
            lda $0300,x
            sta $8300,x
            inx
            bne zachowaj_ram_1
            pla
            sta $ffd6
            rts
;-----------------------------------
;------------------------------------------------------------
;
; VSL VERSION 1.1
; Very Simple Loader for Multicom 6502
; OSI 6502 BASIC VERSION 1.0 REV 3.2
; CODE BY MONEO 22.9.2016
; Program nie jest idiotoodporny.
; Aby uzyskaæ ci¹g znaków HEX nale¿y w WinHeX zaladowaæ
; program w formacie c64 (dwa pierwsze bajty to adres ³adowania)
; do przes³ania, nastêpnie zaznaczyæ ca³oœæ i wybraæ
; Edit -> Copy Block -> Hex Values.
;
; Potem terminalu klikamy prawym przyciskiem myszy.
; Aby zakoñczyæ program loadera wybieramy ;.
; VSL ustawia wektor dla PRINT USR(0) na adres pocz¹tkowy
; ³adowania.
; Po za³adowaniu wystarczy wykonaæ w konsoli polecenie
; PRINT USR(0).
;------------------------------------------------------------ 
; poke 11,0:poke12,218 <- dla PRINT USR(0), gdy adres $DA00
;------------------------------------------------------------   

vec   = $0b           ;wektor ³adowania, a tak¿e wektor
                      ;dla PRINT USR(0)
vsl                      
      SEI
     
      JSR ACIA_Init
                 
      jsr Get_ASCII ;odebranie adresu ³adowania
      BCS exit_d    ;je¿eli jest ";" to koniec
      sta vec       ;m³odszy bajt adresu ³adowania
      sta exit_d+1
      jsr Get_ASCII 
      BCS exit_d 
      sta vec+1     ;starszy bajt adresu ³adowania
      sta exit_e+1          
      
pentla_gw
      jsr Get_ASCII
      BCS exit_d
      LDY #0
      STA (VEC),Y
      JSR inc_vec
      jsr hex1
      jmp pentla_gw

exit_d
      lda #$00
exit_e      
      ldy #$60
      sta vec
      sty vec+1
      
      cli
      rts
;------------------------------------------------------------ 
inc_vec
        inc vec
        bne inc_vec1
        inc vec+1
inc_vec1
        rts
;------------------------------------------------------------
;odbiór bajtu i zamiana go na HEX
Get_ASCII   
      JSR Get_Chr
      BCC Get_ASCII
      cmp #27        ;czy ESC
      beq exit_esc
      cmp #3
      beq exit_esc
      
      cmp #";"       ;czy zakonczenie ladowania?
      beq exitxx     
      
      cmp #$0d       ;czy to renturn?
      beq Get_ASCII  ;odbierz kolejny bajt
      cmp #$0a       ;czy to koniec linii?
      beq Get_ASCII  ;

      sta bufor_1
Get_ASCII1      
      JSR Get_Chr
      BCC Get_ASCII1
      cmp #27        ;czy ESC
      beq exit_esc
      cmp #3
      beq exit_esc
      
      cmp #";"
      beq exitxx
      
      cmp #$0d       ;czy to renturn?
      beq Get_ASCII1  ;odbierz kolejny bajt
      cmp #$0a       ;czy to koniec linii?
      beq Get_ASCII1  ;
           
      sta bufor_1+1
      
      lda bufor_1
      ldy bufor_1+1
      jsr Proc_ASCII
      clc
      rts
exitxx
      sec
      rts
exit_esc
      jmp jump
;------------------------------------------------------------                            
bufor_1
        .byte 0,0
          
;------------------------------------------------------------ 
;WYDRUK LICZBY HEX
; LDA #>LICZBA LUB LDA #LICZBA
; LDX #<LICZBA     JSR HEX1
; JSR HEX
;------------------------------------------------------------ 
HEX      
         ;CLC
         PHA
         TXA
         JSR HEX1
         PLA
HEX1     
         ;CLC
         PHA
         LSR A
         LSR A
         LSR A
         LSR A
         JSR HEX2
         PLA
         AND #$0F
HEX2     CMP #$0A
         BCC HEX3
         ADC #$06
HEX3     ADC #$30
         JMP Put_Chr
;------------------------------------------------------------   
psave    
save     
         ;rts
 


         jsr ACIA_Init
         
         
         lda #<kom_save
         ldy #>kom_save
         jsr new_ab1e        
         
         jsr sa0 ;pobranie adresu z terminala
         
         jsr sa9 
         
         sta $60
         jsr sa8
         tay
         lda $60
         jsr Proc_ASCII
         sta $61
         
         jsr sa8
         sta $60
         jsr sa8
         tay
         lda $60
         jsr Proc_ASCII
         sta $60
                         
         lda #<kom_save2
         ldy #>kom_save2
         jsr new_ab1e 
         
         jsr sa0 ;pobranie adresu z terminala
         
         jsr sa9 ;ustawienie ldx #0 i pobranie 1 ascii
         
         sta $62
         jsr sa8 ;pobranie 2 ascii
         tay
         lda $62
         jsr Proc_ASCII
         sta $63
         lda $62
         cmp #$ff
         bcc sa13
         dec $63
sa13         
         jsr sa8  ;pobranie 3 ascii
         sta $62
         jsr sa8  ;pobranie 4 ascii
         tay
         lda $62
         jsr Proc_ASCII
         sta $62
         
         jsr cf_lf
         
         
         lda #<kom_save5
         ldy #>kom_save5
         jsr new_ab1e 
         lda $60  ;poczatek save
         ldx $61
         jsr HEX
         
         ;lda #" "
         ;jsr Put_Chr
         
         lda #<kom_save6
         ldy #>kom_save6
         jsr new_ab1e 
         lda $62 ;koniec save
         ldx $63
         inx
         jsr HEX
         lda #"."
         jsr Put_Chr   

         lda #<kom_save4
         ldy #>kom_save4
         jsr new_ab1e         
         
         lda $60
         jsr HEX1
         lda $61
         jsr HEX1
         jsr cf_lf

         ldy #0
         sty $64
sa10     

         lda ($60),y
         jsr HEX1

         inc $60
         inc $64
         lda $64
         cmp #60
         bne sa12
         lda #$00
         sta $64
         jsr cf_lf
         ;jsr wstaw_0d0a        
sa12         
         lda $61
         cmp $63
         beq sa11
         
         lda $60
         bne sa10
         inc $61
         jmp sa10
sa11
         lda $60
         cmp $62
         bne sa10
         
         lda #3
         jsr Put_Chr
         ;jsr wstaw_0d0a
         lda #";"
         jsr Put_Chr
         
         lda #<kom_save3
         ldy #>kom_save3
         jsr new_ab1e 

sa_koniec   
         jmp jump

              

wstaw_0d0a
         lda #"0"
         jsr Put_Chr
         lda #"D"
         jsr Put_Chr
         lda #"0"
         jsr Put_Chr
         lda #"A"
         jmp Put_Chr        
         
kom_save
         ;.byte 13,10
         ;.text "save"
         .byte 13,10
         .text "start adress save:$"
         .byte 0
kom_save2
         .byte 13,10
         .text "end adress save:$"
         .byte 0
kom_save3
          .byte 13,10
          .text "--- end of file---"
          .byte 13,10,13,10,0
kom_save4
          .byte 13,10
          .text "--- start file ---"
          .byte 13,10,0
kom_save5
          .byte 13,10
          .text "Program data from $"
          .byte 0
kom_save6 .text " to $"
          .byte 0
          
 
sa0         
         ldx #0
         ;stx $6d
sa1      
         jsr Get_Chr
         bcc sa1
         cmp #27        ;czy ESC
         beq exit_esc1
         cmp #3
         beq exit_esc1
         
         cmp #8       ;czy backspace
         bne sa2
         cpx #0
         beq sa3
         dex
sa3      
         jsr Put_Chr
         jmp sa1
sa2      cmp #27      ;czy ecs
         bne sa4
         jmp sa7
sa4               
         sta field,x
         
         jsr Put_Chr
         cmp #13
         beq sa5
         inx
         cpx #72
         bne sa1
         jmp sa6
sa5
         cpx #0
         bne sa6
         jmp sa0
sa7         
         jsr cf_lf
         jmp sa0        
sa6         
         rts
         ;----
sa9
         ldx #$00
sa8         
         lda field,x
         inx
         cmp #$20
         beq sa8
         cmp #$2c
         beq sa8
         cmp #"$"
         beq sa8
         rts          
exit_esc1 jmp jump         
         