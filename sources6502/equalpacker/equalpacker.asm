;---------------------------------------
;equalpacker v1.0
;code by polonus
;diassemble by moneo
;;---------------------------------------
;version 1
;1995.11.11
;---------------------------------------
     *= $cd00

	jmp l1
;---------------------------------------
ops 	.byte $00,$21,$08,$c7,$07,$9e
    	.byte $32,$30
	.byte $38,$33,$20,$41,$4c,$49
	.byte $20,$4d
	.byte $41,$43,$54,$4f,$4d,$20
	.byte $31,$39
	.byte $39,$32
	.byte 32,32,32,32,32,32,0,0,0
	ldy #$80
	sei
	sty $d011
	lda #$35
	sta $01
k1	lda $0838,y
	sta $033f,y
	dey
	bne k1
	jmp $0340
	ldx #0
k2	lda $ffff,x
	sta $0790,x
	sta $d020
	inx
	bne k2
k6	inx
	jsr $0388
t1	cmp #$07
	bne k3
	jsr $0388
	cmp #2
	bne k4
	lda #$37
	sta $01
	jsr $e518
t6	lda #$00
t5	ldx #$00
	sta $2d
	stx $2e
	cli
	jsr $a659
	jmp $a7ae
k4	tax
	jsr $0388
k3	dey
t3	sta $ffff,y
	sta $d020
	bne k5
	dec $037a
k5	dex
	bne k3
	beq k6
	lda $0394
	bne k7
	dec $0395
k7    	dec $0394
t8	lda $ffff
	rts
;---------------------------------------
l1 	lda #>start
	ldx #<start
	sta $0312
	stx $0311
	rts
;---------------------------------------
start	jsr $e518
	ldy #0
	tya
l3	sta $0400,y
	sta $d800,y
	dey
	bne l3
	sei
	inc $01
	sty $fb
	lda #$08
	sta $fc
	sty $ff
l6	lda ($fb),y
	cmp $ff
	beq l4
	sta $ff
	tax
	lda $0400,x
	cmp #$ff
	beq l4
	inc $0400,x
l4	iny
	bne l5
	inc $fc
l5	lda $fc
	cmp $2e
	bcc l6
	cpy $2d
	bcc l6
	ldx #$00
l8	ldy $0400,x
	cpy $fb
	beq l7
	inx
	bne l8
	inc $fb
	bne l8
l7	dec $01
	lda #1
	sta $d800,x
	stx $0340
	stx t1+1
	stx $0791
	lda #2
	sta $0790
	inc $01
	ldy $2e
	sty $21
	sty t2+1
	dey
	sty t3+2
	ldy $2d
	sty t4+1
	sty t3+1
	dey
	bne l9
	dec $21
l9	sty $20
	ldy #0
	lda ($20),y
	eor #$01
	sta ($2d),y
	sty $fb
	ldx #$08
	stx $fc
	dex
	stx $fe
	lda #$92
	sta $fd
l17	ldy #0
	lda ($fb),y
	sta $0342
l11	iny
	beq l10
	cmp ($fb),y
	beq l11
l10	sty $0341
	cpy #$00
	beq l12
	lda $0342
	cmp $0340
	bne l13
	lda #1
	sta $0341
	bne l12
l13	cpy #3
	bcc l14
l12	ldy #$02
l15	tya
	eor #$03
	tax
	dex
	lda $0340,x
	sta ($fd),y
	dey
	bpl l15
	ldx $0341
	jsr skok1
	jsr skok1
l18	jsr skok1
l16	jsr skok2
	dex
	bne l16
	beq l17
l14	lda $0342
	ldy #0
	sta ($fd),y
	ldx #1
	bne l18
skok1	inc $fd
	bne l19
	inc $fe
l19 	rts
skok2 	inc $fb
	bne l20
	inc $fc
l20	lda $fc
	cmp $2e
	bcc l19
	lda $fb
	cmp $2d
	bcc l19
	lda $2e
	sta t5+1
	lda $2d
	sta t6+1
	nop
	jsr skok3
	bne l21
	lda #$90
	ldx #$08
	bne l22
l21	lda $fd
	ldx $fe
l22	sta $fb
	stx $fc
	sta k2+1
	stx k2+2
	ldy #0
l23	lda $0790,y
	sta ($fb),y
	iny
	bne l23
	inc $fc
	lda $fb
	ldx $fc
	sta $2d
	stx $2e
	dec $01
	.byte $2c,$63,$a6
	lda $fd
	sta t8+1
	lda $fe
	sta t8+2
	ldx #$90
l24	lda ops-1,x
	sta $07ff,x
	dex
	bne l24
	ldx #$90
	lda #$20
l25	sta $0700,x
	inx
	bne l25
	cli
	lda #<txt1
	ldy #>txt1
	jsr $ab1e
	jmp dale1
;---------------------------------------
txt1  	.byte 5,13,13,13,13,13,13
	.byte 13,13
;	.text "controlbyte:"
  .text "CONTROLBYTE:"
	.byte 0
;---------------------------------------
dale1	ldx $0340
	lda #$00
	jsr $bdcd
	lda #<txt2
	ldy #>txt2
	jsr $ab1e
	jmp t4
;---------------------------------------
txt2	.byte 13
	.text "OLD END:"
	.byte 0
;---------------------------------------
t4	ldx #$00
t2	lda #$00
	jsr $bdcd
	lda #<txt3
	ldy #>txt3
	jsr $ab1e
	jmp dale3
;---------------------------------------
txt3	.byte 13
	.text "NEW END:"
	.byte 0
;---------------------------------------
dale3 	ldx $2d
	lda $2e
	jsr $bdcd
	lda #13
	jsr $ffd2
	jsr $ffd2
	jmp dale4
skok3	lda $fe
	cmp #$07
	beq l26
	cmp #$09
	bcs l27
	lda $fd
	cmp #$90
	bcc l26
l27	lda #1
	rts
l26	lda #0
	rts
dale4	jmp $a474
;---------------------------------------