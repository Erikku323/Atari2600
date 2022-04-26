;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						Rainbow						  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	processor 6502
	
	include "vcs.h"
	include "macro.h"


        seg.u Variables
	org $80
        
temp .byte

SharkY		.byte
SharkX		.byte


Missle0X	.byte
Missle0Y	.byte

SwimmerX		.byte
SwimmerY		.byte


SharkSpritePTR  .word
SwimmerSpritePTR .word






Sharkoffset .byte
Swimmeroffset .byte

Random .byte  ; for random number generation
;;RandomResult .byte
Random2 .byte

Collision .byte


IsSwimmerDead .byte
SharkSpeed  .byte

FrameCount .byte

BackgroundColor .byte
CurrentScreen .byte

lives .byte

SharkDirection .byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Score Display Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Score .byte   ; Needs to be together
Time  .byte

DigitOffsetONE .word 
DigitOffsetTEN .word

ScoreSprite  .byte
TimeSprite	.byte

DigitsPTR        .word





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DIGITS_HEGHT = 5
SharkHeight = 21
SwimmerHeight = 9
MaxX = 180
MinX = 5
MaxY = 94
MinY = 1
AnimationSpeed = 6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Color Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BROWN           = $12
YELLOW          = $1e
ORANGE          = $3e
RED             = $48
GREEN           = $d6
BLUE            = $a4
YELLOW_GREEN    = $c8
PINK            = $4a
Blue		= $8a
Gray            = $04
Sand            = $EA
Skin		= $FA
BLACK           = $00



	seg code
	org $F000


        
        
        
        
	
START:
 	
	CLEAN_START ; macro to clean the memory
        
        
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Init Pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #<SharkUp
        sta SharkSpritePTR
        lda #>SharkUp
        sta SharkSpritePTR+1
        
        lda #<Swimmer0
        sta SwimmerSpritePTR
        lda #>Swimmer0
        sta SwimmerSpritePTR+1
        
        lda #<Digits
        sta DigitsPTR
        lda #>Digits
        sta DigitsPTR+1
        
       
  
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #Blue
        sta CurrentScreen
	
        lda #Gray
        sta COLUP0
        lda #Skin
        sta COLUP1
 

	lda #0 
	sta CXCLR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set PlayField
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #%00000001
        sta CTRLPF
        lda #%11111111
        sta PF0
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Score life ETC..
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		lda #0
		sta Score
                lda #0
		sta Time
		
		lda #3
		sta lives
                
                lda #0
                sta SharkDirection
 	
       
        
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Start a new Frame by turning on VBlank and Vsyn   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	lda #0
        sta SharkY
        lda #50
        sta SharkX
        lda #3
        sta SharkSpeed
        
        
        lda #50
        sta SwimmerY
        lda #25
        sta SwimmerX
        
NextFrame:
	inc Random
        clc
        lda SwimmerX
        cmp #164
        bcs ScreenComplete
        jmp CheckShark
   
ScreenComplete 
	
        lda #20
        sta SwimmerX
        clc
      	sed
	lda Score
        clc
        adc #1
        sta Score
        cld
        ldx CurrentScreen
        inx
        stx CurrentScreen
        
 
        
        
CheckShark       
	lda SharkDirection 
        cmp #1
        beq SharkDirectionUp

SharkDirectionDown
	lda SharkY
        cmp #0
        bpl ResetAudio
        lda IsSwimmerDead
        cmp #1
        bne SetSharkXPos
        


SharkDirectionUp
	lda SharkY
        cmp #100   ; Is shark Off Screen
        bmi ResetAudio 
        lda IsSwimmerDead
        cmp #1
        bne SetSharkXPos
        
        
ResetSwimmer

	clc
      	sed
	lda Time
        clc
        adc #1
        sta Time
        cld

	lda #0
        sta Swimmeroffset
        lda #Skin
        sta COLUP1
        lda #0
        sta IsSwimmerDead
        lda #20
        sta SwimmerX
        lda #50
        sta SwimmerY
       	lda #0
        sta FrameCount
        
SetSharkXPos 
	jsr GetRandomNum
        sta SharkX
        jsr GetRandomNum
        sta NUSIZ0

        
ResetSharkY
	jsr GetRandomNum2
        jsr SetSharkDir


	lda SharkDirection
        cmp #1
        beq SetSharkUp
        
SetSharkDown 
 	lda #100
        sta SharkY
        lda #21
        sta Sharkoffset
        jmp ResetAudio
SetSharkUp        
	
        lda #0
        sta SharkY
	sta Sharkoffset
ResetAudio    
        lda #0
        sta AUDV0
        
        
SetSharkYPos
	lda SharkDirection
       	cmp #1
        bne SharkGoingDown
SharkGoingUp	
	lda SharkY
        clc
        adc SharkSpeed
	sta SharkY
        jmp CollisionChecks
SharkGoingDown
	lda SharkY
        sec
        sbc SharkSpeed
        sta SharkY

        		
        
        
CollisionChecks      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collision Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Has the Player hit the other player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda #%10000000
        bit CXPPMM     	 ;Player and PLayer	
        bne .SharkEats       ;If Collision Reset
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Set Sand and Water COlors
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	;lda #Sand
     ;  sta COLUPF
        lda CurrentScreen
        sta BackgroundColor
    ;COLUBK
        
        jmp CheckLife
        
.SharkEats  
	jsr FlashRed
EndCollisionCheck
	sta CXCLR
        
        jmp CheckLife
        
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FlashRed:
	lda #$34   ;red
        sta BackgroundColor
      
        lda #1
        sta IsSwimmerDead
        
    	lda #27
        sta Swimmeroffset
        
        lda #RED
        sta COLUP1
        
        
        
        
          
        lda #1
        sta AUDV0
        lda #1
        sta AUDF0
        lda #1
        sta AUDC0
        
        rts
        

CheckLife
	lda IsSwimmerDead
        cmp #1
        beq StartVBlank 
        
SwimAnimation
	lda FrameCount
        cmp #AnimationSpeed
        bne InputCheck

	lda #0
        sta FrameCount
	lda Swimmeroffset
        cmp #18
	beq ResetAnimation        
        clc
        adc #9
        sta Swimmeroffset
        jmp CheckRightP0
        
ResetAnimation
	
        lda #0
        sta Swimmeroffset

InputCheck:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Controller Input Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
	
CheckRightP0:
	;lda SwimmerX
        ;cmp #MaxX
        ;bpl CheckLeftP0
	lda #%10000000 ;Are we going right?
        bit SWCHA
        bne CheckLeftP0
        inc SwimmerX
     
     
        

          
CheckLeftP0
	lda SwimmerX   ; Check if Min has been hit
       	cmp #MinX   
        bmi CheckUpP0 ;bmi
        lda #%01000000
        bit SWCHA
        bne CheckUpP0 ;bvs
        dec SwimmerX
        inc Random
          

CheckUpP0
	lda SwimmerY
        cmp #MaxY
        bcs CheckDownP0
	lda #%00010000
        bit SWCHA
        bne CheckDownP0
        inc SwimmerY
        inc Random
        
CheckDownP0
	lda SwimmerY
       	cmp #MinY
        bmi StartVBlank
	lda SwimmerY
	lda #%00100000
        bit SWCHA
        bne StartVBlank
        dec SwimmerY
        inc Random
        

        
        
StartVBlank

	lda #2       ;load 00000010 into A
	sta VBLANK  ; Turn on VBLANK
	sta VSYNC	; Turn on Vsync
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Generate the Three lines of VSYNC                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	sta WSYNC  ;wait synch First scaneline
	sta WSYNC  ;Second Scan line
	sta WSYNC  ; Third Scan line	
	
	lda #0		;load 0 into register A
	sta VSYNC   ; turn of VSYNC
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Let the TIA output the recommended 37 lines of VBLANK ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx #37       ; X = 37
	
LoopVBlank:

	sta WSYNC      ;hit WSYNC and wait for next scaneline
	dex			   ;X--
	bne LoopVBlank ; loop if X != 0	
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set X Pos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda #0 
        sta HMCLR
        lda SharkX
        ldx #0
        jsr SetHorizPos	; set coarse offset 
        lda SwimmerX
        ldx #1
        jsr SetHorizPos
		
	jsr CalcScore
		
        sta WSYNC	; sync w/ scanline
        sta HMOVE	; apply fine offsets
        
        
	
	lda #0
	sta VBLANK		;turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Ready to show Visible lines                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #BLACK
        sta COLUBK
	lda #$0E ;white
	sta COLUPF
	lda #%0000000
	sta CTRLPF
		
		

	lda #0
        sta PF0
        sta PF1
        sta PF0
        sta CTRLPF ; Don't Mirror
        
        lda #$1C
        sta COLUPF ;set score color
       

	ldx #5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Score Board goes here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawScoreBoard:
	ldy DigitOffsetTEN
        lda Digits,Y
        and #$F0
        sta ScoreSprite

	ldy DigitOffsetONE
        lda Digits,Y
        and #$0F
      	ora ScoreSprite  ; Merge the ten and Ones
        sta ScoreSprite
        
        sta WSYNC
        sta PF1
        
        ldy DigitOffsetTEN+1
        lda Digits,Y
        and #$F0
        sta TimeSprite

	ldy DigitOffsetONE+1
        lda Digits,Y
        and #$0F
      	ora TimeSprite  ; Merge the ten and Ones
        sta TimeSprite
       
       
        jsr Sleep12Cycles
        
        sta PF1
  
        
        ldy ScoreSprite
        sta WSYNC
          
   	sty PF1
	inc DigitOffsetTEN
        inc DigitOffsetTEN+1
        inc DigitOffsetONE
        inc DigitOffsetONE+1
       
        
        
        jsr Sleep12Cycles
        
       
   	
	dex 
        sta PF1
       
     
        
        bne DrawScoreBoard
        sta WSYNC











        lda #0
        sta PF1
        sta PF2
        ldx #15
BlackSpace:
	dex
        sta WSYNC
        bne BlackSpace
		
		
        lda BackgroundColor
        sta COLUPF
        sta COLUBK
        
	ldx #80 ;counter for visible scaneline

LoopVisible:


;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawShark	
	txa		; X -> A
        sec		; set carry for subtract
        sbc SharkY	; local coordinate
        cmp #SharkHeight ; in sprite?
        bcc InShark	; yes, skip over next
        lda #0		; not in sprite, load 0
InShark
	clc
        ADC Sharkoffset
	tay		; local coord -> Y
        lda (SharkSpritePTR),y	; lookup color
       ; sta WSYNC         	; sync w/ scanline
        sta GRP0		; store bitmap
DrawSwimmer
	txa		; X -> A	
        sec		; set carry for subtract
        sbc SwimmerY	; local coordinate
        cmp #SwimmerHeight ; in sprite?
        bcc InSwimmer	; yes, skip over next
        lda #0		; not in sprite, load 0
InSwimmer
	clc
        ADC Swimmeroffset
	tay		; local coord -> Y
        lda (SwimmerSpritePTR),y; lookup color
        sta WSYNC         	; sync w/ scanline
        sta GRP1		; store bitmap
	
        
        
        
      
        
        
        

	
	
	sta WSYNC      ; wait for scanline
        
   
	dex 		   ; X--
	bne LoopVisible
		
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Output 30 more VBLANK lines overflow                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #2	   ;turn on VBLANK
	sta VBLANK
		
	ldx #30  ; 30 scanlines
	
LoopOverscan:
	sta WSYNC
	dex
	bne LoopOverscan
        inc FrameCount
	jmp NextFrame
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Score Subs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalcScore:
	ldx #1
Scoreloop:
	lda Score,X ; Calc Timer then Score
        and #$0F ; Mask part of the score remove 10 digit
        sta temp
        
     	ASL   ; 2 left shifts for X 4
        ASL		;(Score*4)+Score
        ADC temp  
        
        sta DigitOffsetONE,X ; holds both score and time one digit
        
        lda Score,X
        and #$F0
        
        lsr
        lsr
       	sta temp   ;;(N/4) + (N/8)
        lsr
        lsr
        adc temp
        sta DigitOffsetTEN,X
        
	dex 
        bpl Scoreloop ; X >= 0
	rts
	
		





SetSharkDir
	lda Random
        cmp #100
        bpl SetDown
SetUp      
	lda #1
        sta SharkDirection
        rts
SetDown
	lda #0
        sta SharkDirection
        
	rts



	
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutine to generate a random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomNum:
	lda Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random
        
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Divide random number by Four to limit
; size and then add 30 com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lsr ;divide by four by doing 2 right shift
        ;lsr
        sta temp
        lda #25
        adc temp
        inc Random
        
        ;Test 50/50

        
        rts


GetRandomNum2:
	lda Random
	asl
	eor Random
	asl
	asl
	eor Random
	asl
	rol Random
        rts
        
Sleep12Cycles
	rts

        
    
        
        
        
SetHorizPos
	sta WSYNC	; start a new line
	sec		; set carry flag
DivideLoop
	sbc #15		; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7		; calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0,x	; fix coarse position
	sta HMP0,x	; set fine offset
	rts		; return to caller        
        
        
        
        

SharkUp
	.byte #%00000000
        .byte #%00111100;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00111100;$04
        .byte #%00111100;$04
        .byte #%01111110;$04
        .byte #%11111111;$04
        .byte #%11111111;$04
        .byte #%11111111;$04
        .byte #%01111110;$04
        .byte #%00111110;$04
        .byte #%00111100;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
;---End Graphics Data---

SharkDown
	.byte #%00000000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00111100;$04
        .byte #%00111110;$04
        .byte #%01111110;$04
        .byte #%11111111;$04
        .byte #%11111111;$04
        .byte #%11111111;$04
        .byte #%01111110;$04
        .byte #%00111100;$04
        .byte #%00111100;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00011000;$04
        .byte #%00111100;$04
        
;---End Graphics Data---

Swimmer0
	.byte #%00000000
        .byte #%00000000;$FC
        .byte #%00100000;$FC
        .byte #%00101100;$FC
        .byte #%00111100;$FC
        .byte #%00101100;$FC
        .byte #%00100000;$FC
        .byte #%00000000;$FA
        .byte #%00000000;$80
Swimmer1
	.byte #%00000000
        .byte #%00111100;$FC
        .byte #%00100000;$FC
        .byte #%00101100;$FC
        .byte #%00111100;$FC
        .byte #%00101100;$FC
        .byte #%00100000;$FC
        .byte #%00100000;$FA
        .byte #%00000000;$80
Swimmer2
	.byte #%00000000
        .byte #%00100000;$FC
        .byte #%00100000;$FC
        .byte #%00101100;$FC
        .byte #%00111100;$FC
        .byte #%00101100;$FC
        .byte #%00100000;$FC
        .byte #%00111100;$FA
        .byte #%00000000;$80
;---End Graphics Data---


SwimmerEaten
	.byte #%00000000
        .byte #%01000000;$42
        .byte #%00000101;$42
        .byte #%00001000;$42
        .byte #%01000000;$42
        .byte #%00000010;$42
        .byte #%00010100;$42
        .byte #%00000000;$42
        .byte #%01000010;$42
        
        
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Character Table
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
        
        
        
        
        
        
        
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM size to exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC      ; Defines origin to $FFFC
    .word START    ; Reset vector at $FFFC (where program starts)
    .word START    ; Interrupt vector at $FFFE (unused by the VCS)
	