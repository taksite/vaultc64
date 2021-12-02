; modification for mulicomp Tomasz KIełbiowski
;
; The complete source code for Commodore 128 TEDMON
; Extracted and disassembled by Teemu Komulainen 2017
;
; Only for personal use. Use at your own risk.
;

iexmon	= $032e		;$B006 
sysvec	= $0a00
;chrout	= $ffd2  	; wyświetlanie na ekranie
; jprimm	= $ff7d		;$FF7D: PRIMM – print string following the caller’s code ++
jjmpfar	= $ff71		;jump to routine in remote bank
mmureg	= $ff00		; rejestr MMU
;jsetmsg	= $ff90		;SETMSG. Set system error display switch at memory address $009D.
;jindfet	= $ff74 	;get byte from remote bank
;jindcmp	= $ff7a		;compare .A to byte in remote bank
;jindsta	= $ff77		;put byte to remote bank
jbasin	= $ffcf		;CHRIN. Read byte from default input (for keyboard, read a line from the screen).
jopen	= $ffc0		;open a logical file
jchkin	= $ffc6		;CHKIN. Define file as default input. (Must call OPEN beforehands.)
jclose	= $ffc3		;CLOSE. Close file.
jsetnam	= $ffbd		;SETNAM. Set file name parameters.
jstop	= $ffe1		;STOP. Query Stop key indicator, at memory address $0091; if pressed, call CLRCHN and clear keyboard buffer.
jsetlfs	= $ffba		;SETLFS. Set file parameters.
jclrch	= $ffcc		;CLRCHN. Close default input/output files (
jckout	= $ffc9		;CHKOUT. Define file as default output.
;jsetbnk	= $ff68		;set bank for load/save ++
;jjsrfar	= $ff6e		;call subroutine in remote bank
jload	= $ffd5		;LOAD. Load or verify file. 
jsave	= $ffd8		;SAVE. Save file. (Must call SETLFS and SETNAM beforehands.)

monlo	= $00
monhi	= $60

; If the memory location is changed, monlo/monhi should be changed
; accordingly.

* = $D000 ; Desired memory address where TEDMON should be located

; TEDMON must always be located at bank 0x0f in C128

jmoninit ; TEDMON cold-start entry point
			jmp moninit ; Print 'call' entry
			.TEXT "TKI16"
jmonbrk		jmp monbrk ; Break entry point (IBRK at $0316-$0317 points here)
new_ffd2	jmp Put_Chr
			jmp ACIA_Init
			jmp Get_Chr
			jmp cf_lf
			jmp new_ffcf
			jmp new_ffe1
			jmp druk
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
ACIA        	= $DFE0				;$FFD0
ACIAControl 	= ACIA+0
ACIAStatus  	= ACIA+0
ACIAData    	= ACIA+1    
                 
ACIA_Init
				LDA #$95		; Set ACIA baud rate, word size and Rx interrupt (to control RTS)
				STA ACIAControl
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
cf_lf
				lda #13
				jsr Put_Chr
				lda #10
				jmp Put_Chr
;----------------------------
new_ffcf
				jsr Get_Chr
				bcc new_ffcf
          ;jsr loger    ;<----- LOGGER
				rts
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
			
; ===================================
;jsetmsg
			;STA $9D
			;RTS
; ===================================
jprimm		

;---
;PROCEDURA DRUKUJACA C+4 ($FBD8)
;URZYWAC:  JSR DRUK
;          .TEXT "ALA MA KOTA"
;          .BYTE 0
;          DALSZY PROGRAM...
;---
DRUK
         PHA
         TYA
         PHA
         TXA
         PHA
         TSX
         INX
         INX
         INX
         INX
         LDA $0100,X
         STA $BC
         INX
         LDA $0100,X
         STA $BD
         INC $BC
         BNE DRUK1
         INC $BD
DRUK1    LDY #0
DRUK3    LDA ($BC),Y
         BEQ DRUK2
         JSR new_FFD2
         INY
         BNE DRUK3
DRUK2    TYA
         TSX
         INX
         INX
         INX
         INX
         CLC
         ADC $BC
         STA $0100,X
         LDA #$00
         ADC $BD
         INX
         STA $0100,X
         PLA
         TAX
         PLA
         TAY
         PLA
         RTS
; ===================================
jsetbnk		;set bank for load/save ++
			;STA $C6
			;STX $C7
			;RTS
; ===================================

jimonrtn ; Re-entry point from Kernal IEXMON vector
		jmp mm3

monbrk		; do poprawy zapis rejestrów
		jsr jprimm

		.byte $0d,$42,$52,$45,$41,$4b,$07,$00
		
		pla
		sta $02
		ldx #$05
mb1		pla
		sta $03,X
		dex
		bpl mb1
		bmi mi1

moninit				;Print 'call' entry

		;lda #$00 		; Cold-start routine
		;sta mmureg  	; ust. bank pamięci
		sta $05			; SYS and MLM register save (A)
		stX $06			; SYS and MLM register save (X)
		sty $07			; SYS and MLM register save (Y)
		sta $08			; SYS and MLM register save (SR)
		lda >jmoninit	;#monlo		; SYS address high, MLM register PC
		ldy <jmoninit	;#monhi		; SYS address low, MLM register PC
		sta $04
		sty $03
		lda #$0f		; Bank Number, Jump to SYS Address
		sta $02
		
		;lda <jmonbrk
		;sta $0316	;wektor brk w c64
		;lda >jmonbrk
		;sta $0317
		
		jsr jprimm		; primm         Print Immediate

		;.byte $0d,$4d,$4f,$4e,$49,$54,$4f,$52,$00 ; '{CR}monitor {0}'
		;.byte $0d - for c64/128
		.byte $0d, $0a	;for m6502
		.text "TEDMON 6502"
		.byte $0d, $0a	; $0a for m650a
		.byte $00

mi1		cld				
		tsx
		stx $09			; SYS and MLM register save (SP)
		lda #$c0		; setmsg        Control OS Messages
		;jsr jsetmsg		; sta $9d, RTS
		cli

; Perform [r]
showreg  jsr jprimm ; Print registers
		 
		.byte $0d,$0a,$20,$20,$20,$20,$50,$43,$20 ; '{CR}    pc '
		.byte $20,$53,$52,$20,$41,$43,$20,$58 ; ' sr ac x'
		.byte $52,$20,$59,$52,$20,$53,$50,$0d ; 'r yr sp{CR}'
		.byte $0a,$3b,$20,$00 ; $1b,$51,$00			; '; {ESC}q{0}'

		lda $02			; Bank Number, Jump to SYS Address
		jsr cvtbyt		; byte to 2x ascii code  A & X

		txa			; 
		jsr new_ffd2	;jsr chrout	;$ffd2
		lda $03			; SYS address low, MLM register PC
		jsr prntbyt		;stx $0AAF ; Prints two ASCII characters for a byte value
		ldy #$02	
sreg1	lda $02,Y
		jsr ph2		; prntbyt plus space
		iny
		cpy #$08
		bcc sreg1
		
; Get Command
monmain 
		jsr cf_lf	;jsr curstnxt ; Main loop  new line  ;jsr cf_lf 
		ldx #$00 	; c128,c64
		stx $7A		; do przechowywania pozycji w $200
mm1		jsr new_ffcf	;jsr jbasin		;$ffcf
		;STA $3000,X
		CMP #$1B	;ESCAPE
		BNE MM1A
		JMP MONMAIN
MM1A
		CMP #$08	;BACKSPACE
		BNE MM1B
		jsr new_ffd2
		dex
		
		jsr jprimm
		.byte $20,$08,$00
		
		jmp mm1
MM1B		
		sta $0200,X
		jsr new_ffd2	;echo dla terminala
		inx
		cpx #70		; za dużo znaków w linii
		bcs mm5			; Error
		cmp #$0D		; czy był enter?
		bne mm1			;pobieramy dalej
		lda #$00		;jak był to zakańczamy w buforze na zero
		sta $01FF,X
		;STX $7A
mm2		jsr tc1			; Get Character from bufor
		bne mm2a
		jmp monmain
mm2a		
		cmp #$20		;pomin spacje
		beq mm2
		;jmp (iexmon) odwołanie do ROM $B006 (JMP$ B0B2) D dokładnie procedury poniżej
		; JMP ($032E) Machine Lang Monitor Link [B006]
			
mm3		ldx #$15
mm4		cmp comtbl,X	; Commands
		beq mm6
		dex
		bpl mm4
mm5		jsr jprimm			; primm         Print Immediate ERROR
		.byte $1d,$3f,$00	; '{RIGHT}?{0}'
		jmp monmain			; Get Command
		
mm6		cpx #$13			;ILE POLECEŃ	
		bcs mm7
		cpx #$0F
		bcs mm8
		txa
		asl A
		tax
		lda exectbl+1,X		;na stos adres wybranej procedury
		pha					; Vectors
		lda exectbl,X
		pha
		jmp gp1
mm7		
		CMP #"L"
		BNE MM_T1
		jmp proc_load
MM_T1
		jmp komunukat_wycieca	
		;sta $93				; Work value, monitor, LOAD / SAVE 0 = LOAD, 1 = VERIFY
		;jmp monlsv
mm8		jmp numxvrt

exitmon	
		
		; jmp $E37B
		;PHA ;pod $e37b
		;JMP $B7A7
		jsr jprimm
		.text "THERE IS NO ESCAPE!"
		.byte $0d,$00
		jmp monmain
; -------------------------------------------------------------		
		;jmp (sysvec) ; Back to BASIC via system vector

comtbl ; Table of TEDMON commands
		.byte $41,$43,$44,$46,$47,$48,$4a,$4d ;ACDFGHJM
		.byte $52,$54,$58,$40,$2e,$3e,$3b,$24 ;RTX@.>;$
		.byte $2b,$26,$25,$4c,$53,$56,$00,$00 ;+&%LSV
; -------------------------------------------------------------
exectbl ; Table of command execution addresses

		.word assmble-1		; Perform [a]
		.word cmpxfr-1  	; Perform [c]
		.word disassm-1		; Perform [d]
		.word fillmem-1		; Perform [f]
		.word gotoloc-1		; Perform [g]
		.word search-1		; Perform [h]
		.word jmpsub-1		; Perform [j]
		.word showmem-1		; Perform [m]
		.word showreg-1		; Perform [r]
		.word xfr-1			; Perform [t]
		.word exitmon-1		; Perform [x]
		.word diskcmd-1		; Perform [@]
		.word assmble-1		; Perform [.][a]
		.word chngmem-1		; Perform [>]
		.word chngreg-1		; Perform [:]
		.word proc_load-1	; Perform [L]
;------------------------------------------------------------
jindfet		
			LDY #$00
			LDA ($66),Y
			RTS
jindsta
			LDY #$00
			STA ($66),Y
			RTS
jindcmp
			LDY#$00
			CMP ($60),Y
			rts
; ===================================
mindfet	stx $0AB2 	; Kernal INDFET call for the monitor
		;ldx $68		; o dziwo, musi być
					;lda #$66	; vector $66
		;sei
		LDA ($66),Y  ;jsr jindfet	; indsta        Bank STA (stavec),y
		;cli
		ldx $0AB2
		rts

mindsta	stx $0AB2 	; 
		;ldx $68		; o dziwo, musi być
		;sei
		STA ($66),Y	
		;cli
		ldx $0AB2
		rts

mindcmp	stx $0AB2 ; Kernal INDCMP call for the monitor
		;ldx #$66
		;stx $02C8
		ldx $68
		CMP ($66),Y; jsr jindcmp
		php
		ldx $0AB2
		plp
		rts
;------------------------------------------------------------
showmem	bcs sm1 	 ; M ->  Memory display
		jsr moveval  ; $60 -> $66, $61 -> $67, $62 -> $68
		jsr gp1		 ; zamiana adresu na bajty
		bcc sm2
sm1		
		lda #$08	 ; ilośc linijek do wyswietlenia tu 8
		sta $60
		bne sm4
sm2		jsr calccnt	 ;Calculates number of bytes and banks to display or move
		bcc sm7
		;ldx #$03
		;bit $d7
		;bpl sm3
		;inx
sm3		lsr $62
		ror $61
		ror $60
		dex
		bne sm3
sm4		jsr new_ffe1	;jsr jstop
		beq sm6
		jsr showlin		; Display a line of memory
		lda #$08
		;bit $d7		; 40 / 80 columns:  0 = 40, 128 = 80  $d7	
		;bpl sm5		; if 40 columns show 8 bytes
		;asl A		; if 80 columns show 16 bytes
sm5		jsr ip1		;Increments address pointer    ($66/$67/$68)+1
		jsr decpoco ;Decrements pointer / counter  ($60/$61/$62)-1
		lda $60
		bne sm4
		;bcs sm4
sm6		jmp monmain
sm7		jmp mm5
;------------------------------------------------------------
chngreg jsr chngadd ; Change register
		ldy #$00
cr1		jsr gp1
		bcs cr2
		lda $60
		sta $0005,Y
		iny
		cpy #$05
		bcc cr1
cr2		jmp monmain

chngmem	bcs cm3 		; Change memory
		jsr moveval		; Transfers address and bank values to working pointer
						; $60 -> $66, $61 -> $67, $62 -> $68
		ldy #$00
cm1		jsr gp1			; Evaluates a parameter in the input buffer
		bcs cm3
		lda $60
		jsr mindsta		; ZAPIS DO PAMIECI2
		iny
		;bit $D7
		;bpl cm2
		;cpy #$10
		;bcc cm1
cm2		cpy #$08
		bcc cm1
cm3		jsr jprimm

		;.byte $1b,$4f,$91,$00
		.BYTE $1B, $3E, $00
		
		jsr showlin
		
		jmp monmain
;------------------------------------------------------------
showlin	jsr curstnxt 	; Display a line of memory
		lda #$3E
		jsr new_ffd2	;jsr chrout		; (ibsout)      Output Vector, chrout
		jsr prnthex		; Display 5-Digit Address
		ldy #$00
		beq sl2
sl1		jsr ph3			; Print Space
sl2		jsr mindfet		; Read Banked Memory
		jsr prntbyt		; Output 2-Digit Byte
		iny
		cpy #$08
		;bit $d7			; 40 / 80 columns:  0 = 40, 128 = 80
		;bpl sl3
		;cpy #$10
sl3		bcc sl1
		jsr jprimm

		;.byte $3a,$12,$00 ; ':{RVSON}{0}'
		.byte $3a,$00		; ':'

		ldy #$00
sl4		jsr mindfet		; Read Banked Memory
		pha
		and #$7F
		cmp #$20
		pla
		bcs sl5
		lda #$2E
sl5		jsr new_ffd2	;jsr chrout		; (ibsout)      Output Vector, chrout [EF79]
		iny
		;bit $d7			; 40 / 80 columns:  0 = 40, 128 = 80  for c128
		;bpl sl6
		;cpy #$10
		;bcc sl4
sl6		cpy #$08
		bcc sl4
		rts
;------------------------------------------------------------		
gotoloc	;jsr chngadd ; Go routine
		;ldx $09
		;txs
		;jmp jjmpfar	; Changes bank and address
;------------------------------------------------------------
jmpsub	jsr chngadd ; Jump to subroutine
		jsr jjsrfar
		jmp monmain
;jjsrfar	
;		lda $60
;		sta jjsrfaradr+1
;		lda $61
;		sta jjsrfaradr+2
;jjsrfaradr
;	
;		.byte $20,$ff, $ff	; jsr $ffff
;		.byte $60
jjsrfar
		lda $60
		sta $0201
		lda $61
		sta $0202
		jmp ($0201)
		
	
;------------------------------------------------------------
cmpxfr	lda #$00 	; Compare / Transfer memory
		.byte $2c	; BIT $80A9
xfr		.byte $a9	; lda #$80
		.byte $80
		sta $93
		lda #$00
		sta $0AB3
		jsr prepptr	;Prepares pointers for dual-address operations
		bcs cx1 
		jsr gp1		;Evaluates a parameter in the input buffer
		bcc cx2
cx1		jmp mm5
cx2		bit $93
		bpl cx4		; skok jeśli  N = 0 czyli w $93 jest 00
		sec
		lda $66
		sbc $60
		lda $67
		sbc $61
		bcs cx4
		lda $63
		adc $60
		sta $60
		lda $64
		adc $61
		sta $61
		lda $65
		adc $62
		sta $62
		ldx #$02
cx3		lda $0AB7,X
		sta $66,X
		dex
		bpl cx3
		lda #$80
		sta $0AB3
cx4		jsr curstnxt 	; New Line
		ldy #$00
cx5		jsr new_ffe1	;jsr jstop		; (istop)       Test-Stop Vector [F66E]
		beq cx11
		jsr mindfet		; Read Banked Memory
		;ldx #$60		; VECTOR 1
		;stx $02B9
		;stx $02C8
		ldx $62			;REJESTR mmu
		;sei
		bit $93			; Work value, monitor, LOAD / SAVE 0 = LOAD, 1 = VERIFY
		bpl cx6			; skok jeśli  N = 0 
		
						;jsr jindsta		; indsta        Bank STA (stavec),y
		ldy #$00
		sta ($60),y
						
cx6		
		ldx $62			;REJESTR mmu
		cmp ($60),y		;jsr jindcmp
		beq cx7
		jsr prnthex
		jsr ph3
		jsr ph3
cx7		bit $0AB3
		bmi cx8
		inc $60
		bne cx9
		inc $61
		bne cx9
		jmp mm5
cx8		jsr decpoco			;Decrements pointer / counter  ($60/$61/$62)-1
		jsr decptr			;Decrements address pointer  ($66/$67/$68)-1
		jmp cx10
cx9		jsr incptr			;Increments address pointer ($66/$67/$68)+1
cx10	jsr deccnt
		bcs cx5
cx11	jmp monmain

;------------------------------------------------------------
search	jsr prepptr ; Search for pattern
		bcs sr9
		ldy #$00
		jsr tc1
		cmp #$27
		bne sr2
		jsr tc1
		cmp #$00
		beq sr9
sr1		sta $0A80,Y
		iny
		jsr tc1
		beq sr4
		cpy #$20
		bne sr1
		beq sr4
sr2		sty $0100
		jsr getparm
sr3		lda $60
		sta $0A80,Y
		iny
		jsr gp1
		bcs sr4
		cpy #$20
		bne sr3
sr4		sty $93
		jsr curstnxt
sr5		ldy #$00
sr6		jsr mindfet
		cmp $0A80,Y
		bne sr7
		iny
		cpy $93
		bne sr6
		jsr prnthex
		jsr ph3
		jsr ph3
sr7		jsr new_ffe1	;jsr jstop
		beq sr8
		jsr incptr
		jsr deccnt
		bcs sr5
sr8		jmp monmain
sr9		jmp mm5
;------------------------------------------------------------
;monlsv	
;
;monsave
;
;monload
;		jsr komunukat_wycieca
;		jmp monmain
;
;preplove ; Prepare for load/verify
;		ldx $66
;		ldy $67
;		lda #$00
;		sta $B9
;		beq monload
;----------------------------------------------------
fillmem	jsr prepptr ; Fill memory
		bcs fm3
		lda $68
		cmp $0AB9
		bne fm3
		jsr gp1
		bcs fm3
		ldy #$00
fm1		lda $60
		jsr mindsta
		jsr new_ffe1	;jsr jstop
		beq fm2
		jsr incptr
		jsr deccnt
		bcs fm1
fm2		jmp monmain
fm3		jmp mm5
;----------------------------------------------------
assmble	bcs as6 ; Assemble
		jsr moveval
as1		ldx #$00
		stx $0AA1
		stx $0AB4
as2		jsr tc1
		bne as3
		cpx #$00
		bne as3
		jmp monmain
as3		cmp #$20
		beq as1
		sta $0AAC,X
		inx
		cpx #$03
		bne as2
as4		dex
		bmi as7
		lda $0AAC,X
		sec
		sbc #$3F
		ldy #$05
as5		lsr A
		ror $0AA1
		ror $0AA0
		dey
		bne as5
		beq as4
as6		

		jmp mm5
as7		ldx #$02
as8		lda $0AB4
		bne as13
		jsr cvtparam
		beq as12
		bcs as6
		lda #$24
		sta $0AA0,X
		inx
		lda $62
		bne as6
		ldy #$04
		lda $0AB6
		cmp #$08
		bcc as9
		cpy $0AB4
		beq as10
as9		lda $61
		bne as10
		ldy #$02
as10	lda #$30
as11	sta $0AA0,X
		inx
		dey
		bne as11
as12	dec $7A
as13	jsr tc1
		beq as14
		cmp #$20
		beq as8
		sta $0AA0,X
		inx
		cpx #$0A
		bcc as8
		bcs as6
as14	stx $63
		ldx #$00
		stx $0AB1
as15	ldx #$00
		stx $9F
		lda $0AB1
		jsr calcmn
		ldx $0AAA
		stx $64
		tax
		lda mnemtbl1,X
		jsr as30
		lda mnemtbl,X
		jsr as30
		ldx #$06
as16	cpx #$03
		bne as18
		ldy $0AAB
		beq as18
as17	lda $0AAA
		cmp #$E8
		lda #$30
		bcs as20
		jsr as29
		dey
		bne as17
as18	asl $0AAA
		bcc as19
		lda modetbl-1,X
		jsr as30
		lda modetbl+5,X
		beq as19
		jsr as30
as19	dex
		bne as16
		beq as21
as20	jsr as29
		jsr as29
as21	lda $63
		cmp $9F
		beq as22
		jmp as31
as22	ldy $0AAB
		beq as27
		lda $64
		cmp #$9D
		bne as25
		lda $60
		sbc $66
		tax
		lda $61
		sbc $67
		bcc as23
		bne as28
		cpx #$82
		bcs as28
		bcc as24
as23	tay
		iny
		bne as28
		cpx #$82
		bcc as28
as24	dex
		dex
		txa
		ldy $0AAB
		bne as26
as25	lda $005F,Y
as26	jsr mindsta
		dey
		bne as25
as27	lda $0AB1
		jsr mindsta
		
		;lda #$0d
		;jsr new_ffd2
		;jsr curstcur
		
		jsr jprimm

		; dla c128
		;.byte $41,$20,$1b,$51,$00 ; 'a {ESC}q{0}' in c128
		; wyswietlanie wprowadzonej linijki kodu
		; dla c64
		.byte $0D,$41,$20,$00			

		jsr da7
		inc $0AAB
		lda $0AAB
		jsr ip1
		;lda #$0D
		;jsr new_ffd2	;jsr chrout		
		JSR cf_lf
		jmp zakoncz_ass

		
;--------------------------------------------
; dynamiczna klawiatura c128
; keybuf $034a-$0353
; dynamiczna klawiatura c64
; keybuf $0277-$0280

		
;		lda #$41		;c128
;		sta $0277		;sta $034A
;		lda #$20
;		sta $0278		;sta $034B
;		lda $68
;		jsr cvtbyt		; Byte to 2 Ascii
;		stx $0279		; stx $034C
;		lda $67
;		jsr cvtbyt		; Byte to 2 Ascii
;		sta $027a		;sta $034D
;		stx $027b		;stx $034E
;		lda $66
;		jsr cvtbyt		; Byte to 2 Ascii
;		sta $027c		;sta $034F
;		stx $027d		;stx $0350
;		lda #$20
;		sta $027e		;sta $0351	
;		lda #$08	
;		sta $c6			;sta $D0		; Number of characters in keyboard buffer
;		jmp monmain+3
;--------------------------------------------		
as28	jmp mm5
as29	jsr as30
as30	stx $0AAF
		ldx $9F
		cmp $0AA0,X
		beq as32
		pla
		pla
as31	inc $0AB1
		beq as28
		jmp as15
as32	inc $9F
		ldx $0AAF
		rts
;--------------------------------------------
; wersja dla chrout
zakoncz_ass
		
		ldx #$00
		STX $7A
		LDA #"A"
		JSR pam_bufor_X
		LDA #" "
		JSR pam_bufor_X
		
		lda $68			;bank pamieci
		jsr cvtbyt		; Byte to 2 Ascii
		TXA
		jsr	pam_bufor_X
		
		lda $67
		jsr cvtbyt		; Byte to 2 Ascii
		jsr	pam_bufor_X
		
		lda $67
		jsr cvtbyt	
		txa
		jsr	pam_bufor_X
		
		lda $66
		jsr cvtbyt		; Byte to 2 Ascii
		jsr	pam_bufor_X
		
		lda $66
		jsr cvtbyt		; Byte to 2 Ascii
		txa
		jsr	pam_bufor_X
		
		lda #" "
		jsr	pam_bufor_X
		lda #" "
		jsr	pam_bufor_X
		
		LDX #0
		
zakoncz_ass1
		
		lda $0200,x
		jsr new_ffd2
		inx
		cpx $7a
		bcc zakoncz_ass1
		
		ldx $7a
		TXA

		LDX #$00
		STX $7A
		tax
		jmp mm1

;------------------------------------------------------------
;
pam_bufor_X
				ldx $7a
				sta $0200,x		
				INC $7A
				rts
;------------------------------------------------------------
		
disassm	bcs da1 ; Disassemble
		jsr moveval
		jsr gp1
		bcc da2
da1		lda #$14
		sta $60
		bne da3
da2		jsr calccnt
		bcc da5
da3		jsr jprimm
	
		;c128
		;.byte $0d,$1b,$51,$00
		.byte $0d,$0a,$00

		jsr new_ffe1	;jsr jstop
		beq da4
		jsr da6
		inc $0AAB
		lda $0AAB
		jsr ip1
		lda $0AAB
		jsr dp1
		bcs da3
da4		jmp monmain
da5		jmp mm5
da6		lda #$2E
		jsr new_ffd2	;jsr chrout
		jsr ph3
da7		jsr prnthex
		jsr ph3
		ldy #$00
		jsr mindfet
		jsr calcmn
		pha
		ldx $0AAB
		inx
da8		dex
		bpl da9
		jsr jprimm

			.byte $20,$20,$20,$00

		jmp da10
da9		jsr mindfet
		jsr ph2
da10	iny
		cpy #$03
		bcc da8
		pla
		ldx #$03
		jsr prntmn
		ldx #$06
da11	cpx #$03
		bne da13
		ldy $0AAB
		beq da13
da12	lda $0AAA
		cmp #$E8
		php
		jsr mindfet
		plp
		bcs da15
		jsr prntbyt
		dey
		bne da12
da13	asl $0AAA
		bcc da14
		lda modetbl-1,X
		jsr new_ffd2	;jsr chrout
		lda modetbl+5,X
		beq da14
		jsr new_ffd2	;jsr chrout
da14	dex
		bne da11
		rts
da15	jsr da17
		clc
		adc #$01
		bne da16
		inx
da16	jmp ph1
da17	ldx $67
		tay
		bpl da18
		dex
da18	adc $66
		bcc da19
		inx
da19	rts

calcmn	tay ; Calculates mnemonic and addressing mode
	lsr A
	bcc cmn1
	lsr A
	bcs cmn3
	cmp #$22
	beq cmn3
	and #$07
	ora #$80
cmn1	lsr A
	tax
	lda opcdtbl,X
	bcs cmn2
	lsr A
	lsr A
	lsr A
	lsr A
cmn2	and #$0F
	bne cmn4
cmn3	ldy #$80
	lda #$00
cmn4	tax
	lda indctbl,X
	sta $0AAA
	and #$03
	sta $0AAB
	tya
	and #$8F
	tax
	tya
	ldy #$03
	cpx #$8A
	beq cmn7
cmn5	lsr A
	bcc cmn7
	lsr A
cmn6	lsr A
	ora #$20
	dey
	bne cmn6
	iny
cmn7	dey
	bne cmn5
	rts

prntmn	tay ; Prints mnemonic for opcode
	lda mnemtbl,Y
	sta $63
	lda mnemtbl1,Y
	sta $64
pm1	lda #$00
	ldy #$05
pm2	asl $64
	rol $63
	rol A
	dey
	bne pm2
	adc #$3F
	jsr new_ffd2	;jsr chrout
	dex
	bne pm1
	jmp ph3

opcdtbl ; Opcode decoding table

.byte $40,$02,$45,$03,$d0,$08,$40,$09,$30,$22,$45,$33
.byte $d0,$08,$40,$09,$40,$02,$45,$33,$d0,$08,$40,$09
.byte $40,$02,$45,$b3,$d0,$08,$40,$09,$00,$22,$44,$33
.byte $d0,$8c,$44,$00,$11,$22,$44,$33,$d0,$8c,$44,$9a
.byte $10,$22,$44,$33,$d0,$08,$40,$09,$10,$22,$44,$33
.byte $d0,$08,$40,$09,$62,$13,$78,$a9

indctbl ; Table of addressing mode indicators

.byte $00,$21,$81,$82,$00,$00,$59,$4d,$91,$92,$86,$4a
.byte $85,$9d

modetbl ; Table of mode identification characters

.byte $2c,$29,$2c,$23,$28,$24,$59,$00,$58,$24,$24,$00

mnemtbl ; Table of mnemonics in packed form (first byte)

.byte $1c,$8a,$1c,$23,$5d,$8b,$1b,$a1,$9d,$8a,$1d,$23
.byte $9d,$8b,$1d,$a1,$00,$29,$19,$ae,$69,$a8,$19,$23
.byte $24,$53,$1b,$23,$24,$53,$19,$a1,$00,$1a,$5b,$5b
.byte $a5,$69,$24,$24,$ae,$ae,$a8,$ad,$29,$00,$7c,$00
.byte $15,$9c,$6d,$9c,$a5,$69,$29,$53,$84,$13,$34,$11
.byte $a5,$69,$23,$a0

mnemtbl1 ; Table of mnemonics in packed form (second byte)

.byte $d8,$62,$5a,$48,$26,$62,$94,$88,$54,$44,$c8,$54
.byte $68,$44,$e8,$94,$00,$b4,$08,$84,$74,$b4,$28,$6e
.byte $74,$f4,$cc,$4a,$72,$f2,$a4,$8a,$00,$aa,$a2,$a2
.byte $74,$74,$74,$72,$44,$68,$b2,$32,$b2,$00,$22,$00
.byte $1a,$1a,$26,$26,$72,$72,$88,$c8,$c4,$ca,$26,$48
.byte $44,$44,$a2,$c8,$0d,$20,$20,$20

getparm	dec $7A ; Evaluates a parameter in the input buffer
gp1		jsr cvtparam
		bcs gp3
		jsr testchr
		bne gp2
		dec $7A
		lda $0AB4
		bne gp5
		beq gp4
gp2		cmp #$20
		beq gp5
		cmp #$2C
		beq gp5
gp3		pla
		pla
		jmp mm5
gp4		sec
		.byte $24
gp5		.byte $18
		lda $0AB4
		rts

cvtparam ; Transforms a numeric parameter into byte value
		lda #$00
		sta $60
		sta $61
		sta $62
		sta $0AB4
		txa
		pha
		tya
		pha
cp1		jsr tc1
		bne cp2
		jmp cp12
cp2		cmp #$20
		beq cp1
		ldx #$03
cp3		cmp comtbl+15,X
		beq cp4
		dex
		bpl cp3
		inx
		dec $7A
cp4		ldy basetbl,X
		lda basetbl+4,X
		sta $0AB6
cp5		jsr tc1
		beq cp12
		sec
		sbc #$30
		bcc cp12
		cmp #$0A
		bcc cp6
		sbc #$07
		cmp #$10
		bcs cp12
cp6		sta $0AB5
		cpy $0AB5
		bcc cp11
		beq cp11
		inc $0AB4
		cpy #$0A
		bne cp8
		ldx #$02
cp7		lda $60,X
		sta $0AB7,X
		dex
		bpl cp7
cp8 	ldx $0AB6
cp9		asl $60
		rol $61
		rol $62
		bcs cp11
		dex
		bne cp9
		cpy #$0A
		bne cp10
		asl $0AB7
		rol $0AB8
		rol $0AB9
		bcs cp11
		lda $0AB7
		adc $60
		sta $60
		lda $0AB8
		adc $61
		sta $61
		lda $0AB9
		adc $62
		sta $62
		bcs cp11
cp10	clc
		lda $0AB5
		adc $60
		sta $60
		txa
		adc $61
		sta $61
		txa
		adc $62
		sta $62
		bcs cp11
		and #$F0
		bne cp11
		beq cp5
cp11	sec
		.byte $24
cp12	.byte $18
		sty $0AB6
		pla
		tay
		pla
		tax
		lda $0AB4
		rts

basetbl ; Table of bases and bits-per-digit

			.byte $10,$0a,$08,$02,$04,$03,$03,$01

prnthex		lda $68 ; Prints a hexadecimal value
			jsr cvtbyt
			txa
			jsr new_ffd2	;jsr chrout
			lda $66
			ldx $67
ph1			pha
			txa
			jsr prntbyt
			pla
ph2			jsr prntbyt
ph3			lda #$20
			jmp new_ffd2	;jmp chrout

curstcur ; Moves cursor to start of current line
		 ;	jsr jprimm
		 ;	.byte $0d,$0a,$91,$00
		 	rts

curstnxt ; Moves cursor to start of next line
		 ;lda #$0D
		 ;jmp new_ffd2	;jmp chrout
		 jmp cf_lf

clrlin	jsr jprimm ; Clears a screen line

.byte $0d,$0a,$1b,$51,$20,$00

	rts

prntbyt	stx $0AAF ; Prints two ASCII characters for a byte value
	jsr cvtbyt
	jsr new_ffd2	;jsr chrout
	txa
	ldx $0AAF
	jmp new_ffd2	;jmp chrout

cvtbyt	pha ; Converts a byte value into two ASCII characters
	jsr cb1
	tax
	pla
	lsr A
	lsr A
	lsr A
	lsr A
cb1	and #$0F
	cmp #$0A
	bcc cb2
	adc #$06
cb2	adc #$30
	rts

testchr	dec $7A ; Tests next character in the input buffer
tc1		stx $0AAF	;zachowanie rej X
		ldx $7A
		lda $0200,X
		beq tc2
		cmp #$3A	; pomijamy : 
		beq tc2
		cmp #$3F	; pomijamy >
tc2		php
		inc $7A
		ldx $0AAF	;odchowanie rej X
		plp
		rts

moveval	lda $60 ; Transfers address and bank values to working pointer
		sta $66
		lda $61
		sta $67
		lda $62
		sta $68
		rts

calccnt	sec ; Calculates number of bytes and banks to display or move
	lda $60
	sbc $66
	sta $60
	lda $61
	sbc $67
	sta $61
	lda $62
	sbc $68
	sta $62
	rts

decpoco	lda #$01 ; Decrements pointer / counter
dp1	sta $0AAF
	sec
	lda $60
	sbc $0AAF
	sta $60
	lda $61
	sbc #$00
	sta $61
	lda $62
	sbc #$00
	sta $62
	rts

deccnt	sec ; Decrements byte count
	lda $63
	sbc #$01
	sta $63
	lda $64
	sbc #$00
	sta $64
	lda $65
	sbc #$00
	sta $65
	rts

incptr	lda #$01 ; Increments address pointer
ip1	clc
	adc $66
	sta $66
	bcc ip2
	inc $67
	bne ip2
	inc $68
ip2	rts

decptr	sec ; Decrements address pointer
	lda $66
	sbc #$01
	sta $66
	lda $67
	sbc #$00
	sta $67
	lda $68
	sbc #$00
	sta $68
	rts

chngadd	bcs ca1 ; Changes bank and address
	lda $60
	ldy $61
	ldx $62
	sta $04
	sty $03
	stx $02
ca1	rts

prepptr	bcs ppex ; Prepares pointers for dual-address operations
	jsr moveval
	jsr gp1
	bcs ppex
	lda $60
	sta $0AB7
	lda $61
	sta $0AB8
	lda $62
	sta $0AB9
	jsr calccnt
	lda $60
	sta $63
	lda $61
	sta $64
	lda $62
	sta $65
	bcc ppex
	clc
	.byte $24
ppex .byte $38
	 rts

numxvrt	jsr getparm ; Performs number base conversion
	jsr clrlin
	lda #$24
	jsr new_ffd2		;jsr chrout
	lda $62
	beq nx1
	jsr cvtbyt
	txa
	jsr new_ffd2		;jsr chrout
nx1	lda $60
	ldx $61
	jsr ph1
	jsr clrlin
	lda #$2B
	jsr new_ffd2		;jsr chrout
	jsr hexdec
	lda #$00
	ldx #$08
	ldy #$03
	jsr pb1
	jsr clrlin
	lda #$26
	jsr new_ffd2		;jsr chrout
	lda #$00
	ldx #$08
	ldy #$02
	jsr pntobd
	jsr clrlin
	lda #$25
	jsr new_ffd2		;jsr chrout
	lda #$00
	ldx #$18
	ldy #$00
	jsr pntobd
	jmp monmain

hexdec	jsr moveval ; Converts a hexadecimal value to decimal
	lda #$00
	ldx #$07
hd1	sta $0AA0,X
	dex
	bpl hd1
	inc $0AA7
	ldy #$17
	php
	sei
	sed
hd2	lsr $68
	ror $67
	ror $66
	bcc hd4
	clc
	ldx #$03
hd3	lda $0AA4,X
	adc $0AA0,X
	sta $0AA0,X
	dex
	bpl hd3
hd4	clc
	ldx #$03
hd5	lda $0AA4,X
	adc $0AA4,X
	sta $0AA4,X
	dex
	bpl hd5
	dey
	bpl hd2
	plp
	rts

pntobd	pha ; Prints octal, binary or decimal values
	lda $60
	sta $0AA2
	lda $61
	sta $0AA1
	lda $62
	sta $0AA0
	lda #$00
	sta $0AA3
	pla
pb1	sta $0AB4
	sty $0AB6
pb2	ldy $0AB6
	lda #$00
pb3	asl $0AA3
	rol $0AA2
	rol $0AA1
	rol $0AA0
	rol A
	dey
	bpl pb3
	tay
	bne pb4
	cpx #$01
	beq pb4
	ldy $0AB4
	beq pb5
pb4	inc $0AB4
	ora #$30
	jsr new_ffd2	;jsr chrout
pb5	dex
	bne pb2
	rts

diskcmd	
showdir	
		;jmp komunukat_wycieca
komunukat_wycieca
		
		;LOGER
		LDX $7A
		STA $3101
KW1		
		LDA $0200,X
		STA $3000,X
		INX
		BNE KW1
		
		jsr jprimm
		.byte $0d,$0a
		.text "CANCELED"
		.byte $0d,$0a,$00
		jmp monmain
;----------------------------------------------------------		
; Mandatory credits follows :-)
; (C) 1986 COMMODORE ELECTRONICS, LTD.
; ALL RIIGHTS RESERVED.
.byte $28,$43,$29,$31,$39,$38,$36,$20,$43,$4f,$4d,$4d
.byte $4f,$44,$4f,$52,$45,$20,$45,$4c,$45,$43,$54,$52
.byte $4f,$4e,$49,$43,$53,$2c,$20,$4c,$54,$44,$2e,$20
.byte $41,$4c,$4c,$20,$52,$49,$47,$48,$54,$53,$20,$52
.byte $45,$53,$45,$52,$56,$45,$44,$2e,$ff

;----------------------------------------------------------		
proc_load

		jsr jprimm
		.byte $0D,$0A
		.text "LOAD HERE: "
		.byte $0d,$0a,$00
		
		JSR PROC_LOAD_ASCII
		
		jsr jprimm
		.byte $0D,$0A
		.text "END OF LOAD "
		.byte $0d,$0a,$00
		
		
		jmp monmain
		
;----------------------------------------------------------		
bufor_1 = $0200			;	.byte 0,0
byteFB = BUFOR_1+2  	;	.byte 0  
vec   = $60           	;wektor │adowania, a tak┐e wektor
						;$60/$61  dla PRINT USR(0)
						;$62/$63
						;
PROC_LOAD_ASCII         
      SEI
     
      JSR ACIA_Init
      
      lda #$00
      ldy #$ff
      sta vec
      sty vec+1
      
      jsr Get_ASCII ;odebranie adresu │adowania
      BCS exit_d    ;je┐eli jest ";" to koniec
      sta vec       ;m│odszy bajt adresu │adowania
      ;sta exit_d+1
	  sta vec+2
      jsr Get_ASCII 
      BCS exit_d 
      sta vec+1     ;starszy bajt adresu │adowania
      ;sta exit_e+1 
	  sta vec+3
      
pentla_gw
      jsr Get_ASCII
      BCS exit_d
      LDY #0
      STA (VEC),Y
      JSR inc_vec
      jsr hex1
      jmp pentla_gw

exit_d
      lda vec+2
exit_e      
      ldy vec+3
      sta vec
      sty vec+1
      
      cli
      RTS
;------------------------------------------------------------ 
inc_vec
        inc vec
        bne inc_vec1
        inc vec+1
inc_vec1
        rts
;------------------------------------------------------------
;odbiˇr bajtu i zamiana go na HEX
Get_ASCII   
      JSR Get_Chr
      BCC Get_ASCII
      cmp #";"       ;czy zakonczenie ladowania?
      beq exit     
      CMP #$1B		 ;czy ESC
	  beq exit
      cmp #$0d       ;czy to renturn?
      beq Get_ASCII  ;odbierz kolejny bajt
      cmp #$0a       ;czy to koniec linii?
      beq Get_ASCII  ;

      sta bufor_1
Get_ASCII1      
      JSR Get_Chr
      BCC Get_ASCII1
      cmp #";"
      beq exit
      CMP #$1B		 ;czy ESC
	  beq exit
      cmp #$0d       ;czy to renturn?
      beq Get_ASCII1 ;odbierz kolejny bajt
      cmp #$0a       ;czy to koniec linii?
      beq Get_ASCII1 ;
           
      sta bufor_1+1
      
      lda bufor_1
      ldy bufor_1+1
      jsr Proc_ASCII
      clc
      rts
exit
      sec
      rts
;------------------------------------------------------------                            
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
      RTS      
;------------------------------------------------------------ 
;WYDRUK LICZBY HEX
; LDX #>LICZBA LUB LDA #LICZBA
; LDY #<LICZBA     JSR HEX1
; JSR HEX
;------------------------------------------------------------ 
HEX      
         CLC
         PHA
         TXA
         JSR HEX1
         PLA
HEX1     
         CLC
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
LOGER
			LDX #$00
PROC_LOAD1		
			LDA $0200,X
			STA $2000,X
			INX
			BNE PROC_LOAD1
			RTS

			;.word $00
			;.FILL 20
			
;----------------------------------------------------------
;
; The following commands can used:
;
;	 A - Assemble a mnemonics line into machine code.
;        A <adrese> <command> [<operand>]
;    C - Compare two memory aeras and displays the difference.
;        C <start adress> <end adress> <start adress for comparing>
;    D - Disassemble a machine code line into mnemonics.
;        D [<start adress > [<end adress>]]
;    F - Fill up a memory aera with the given byte.
;        F <start adress> <end adress> <Byte>
;    G - Go to the memory adress, also start a machine code program at the inputed memory adress.
;        G <adress>
;    H - Hunt a memory aera - Durchsucht Speicherbereich nach einen bestimmten Wert und zeigte alle gefundenen Speicherstellen an
;        H <start adress> <end adresse> <datas> (datas are hexadecimal numbers seperated with empty spaces and strings seperated with the prefix apostrophe (').)
;    J - Only C128: Jump to subroutine - Springt in ein Maschinenspracheunterprogramm an der angegebenen Adresse. Im Unterschied zu G gelangt nach Ausführung des Befehls, bei der Rückkehr aus dem Unterprogramm mittels RTS, die Kontrolle wieder zum Monitor.
;        J <adress>
;    L - Load a file from disk or datasette into the memory.
;        L "<filename>",<device number ($1-$F)>,<load memory adress at C128>
;    M - Memory is showing in hexadecimal numbers and values.
;        M [<start adress> [<end adress>]] (by using this command without adresses the first 12 lines are shown.)
;    R - Registers is shown again.
;    S - Save the inputed memory aera into a file on disk or datasette.
;        S "<file iname>",<device number>,<start adress>,<end adress+1>
;    T - Transfer) or copy a memory aera into another.
;        T <start adress> <end adress> <destination adress>
;    V - Verify a saved file on disk or datasette with the memory aera.
;        V "<file name>",<device number ($1-$F)>,<start adress at C128>
;    X - eXit TEDMON into BASIC direct mode.
;    > - Modify one until eight bytes in a memory adress (after M command).
;        > <adress> <byte1> <byte2> ... <byte8>
;    . - Works same like the A command
;    ; - Change the register content (after R command)
;    @ - Only C128: Displays the disk drive status or for using floppy commands.
;        @[device number ($8-$F)][,[ floppy command ]]
;        standard device number is 8.
;

* = jmoninit+$FFF
	.BYTE $FF
.END