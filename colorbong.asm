PLOT 	 equ $F800
TXTPG1 	 equ $C054
CLRHIRES equ $C056	
SPEAKER  equ $C030
CLRMIXED equ $C052		;enable graphics/text mixed mode
SETMIXED equ $C053
CLRTEXT  equ $C050		;enable text-only mode
SETTEXT  equ $C051
STROUT equ $DB3A
SCREEN_BUF equ $400
SCREEN_END equ $07FF
KEY_REG equ $C000
KEY_LATCH equ $C010
COLOR_ADDR equ $30
TEXT_ROW_START equ 
ZEROPAGE_ADDR  equ $7D
SPACE_CHAR equ 32 + 128
LINE_CHAR equ 45 + 128
PLAYER_X equ 0
PLAYER_COLOR equ $66
AI_X equ 39
AI_COLOR equ $22
PADDLE_LENGTH equ 10
BALL_COLOR equ $11
 MAC 	DEBUG_PLOT
	lda #$FF
      	STA COLOR_ADDR
        lda #{2}
        ldy #{1}
        jsr PLOT
 ENDM
	processor 6502
	seg code
	org $803	; starting address

Start
	sta CLRTEXT
        sta SETMIXED
        sta CLRHIRES
        sta TXTPG1
     	jsr Clear_Screen
        jsr Clear_Text
      	jmp Game_Loop
Finish
	debug_plot 22,22
	jmp *

Game_Loop
        lda #0
        jsr Render_Paddle
        lda #1 
        jsr Render_Paddle
        jsr Render_Ball
      	jsr Handle_Player_Input
        lda #0
        beq Game_Loop
Handle_Player_Input
	jsr Get_Key_Pressed
        cmp #'Q
        bne .key_switch_case
        jmp Finish
.key_switch_case
	cmp #'W
        beq .handle_w_key
        cmp #'S
        beq .handle_s_key
.handle_no_key
	lda #0
        sta player_dir
	DEBUG_PLOT 1,2
        rts
.handle_w_key
	lda #1
        sta player_dir
        debug_plot 2,6
        rts
.handle_s_key
	lda #-1
        sta player_dir
        debug_plot 1,5
        rts 


Get_Key_Pressed
	lda KEY_REG
       	bpl .exit_key_pressed
        and #$7F
        sta KEY_LATCH
        rts
.exit_key_pressed
	lda #255
	rts 
Render_Ball
	lda #BALL_COLOR
        sta COLOR_ADDR
        lda ball_pos + 1
        ldy ball_pos
        jsr PLOT
        rts
Render_Paddle
	ldx #PADDLE_LENGTH
        cmp #1
        bne .render_player
.render_ai
	lda ai_pos
        sta ZEROPAGE_ADDR
.render_ai_loop
	lda #AI_COLOR
        sta COLOR_ADDR
        lda ZEROPAGE_ADDR
        ldy #AI_X
        jsr PLOT
       	inc ZEROPAGE_ADDR
        dex
        bne .render_ai_loop
	rts
.render_player
        lda player_pos
        sta ZEROPAGE_ADDR
.render_player_loop
	lda #PLAYER_COLOR
        sta COLOR_ADDR
        lda ZEROPAGE_ADDR
        ldy #PLAYER_X
        jsr PLOT
       	inc ZEROPAGE_ADDR
        dex
        bne .render_player_loop
	rts

Clear_Screen 
	lda #<SCREEN_BUF
        sta ZEROPAGE_ADDR
        lda #>SCREEN_BUF
        sta ZEROPAGE_ADDR+1
        ldy #0
.cs_loop_start
        lda #0
.cs_loop_body
	sta (ZEROPAGE_ADDR),Y
        iny
        bne .cs_loop_body
        lda ZEROPAGE_ADDR
	clc 
        adc #$FF
        sta ZEROPAGE_ADDR
        lda ZEROPAGE_ADDR + 1
        adc #0
        cmp #>SCREEN_END
        sta ZEROPAGE_ADDR+1
        bne .cs_loop_start
.check_low_bits
	lda ZEROPAGE_ADDR
        cmp #<SCREEN_END
        lda #0
        bne .cs_loop_start
        rts
Clear_Text
	lda #LINE_CHAR
        ldy #0
        ldx #40
.text_l1
	sta $0650,Y
        iny
        dex
        bne .text_l1
        ldx #40
        ldy #0
        lda #SPACE_CHAR
.text_l2
	sta $0750,Y
        iny
        dex
        bne .text_l2
        ldx #40
        ldy #0
.text_l3
	sta $06D0,Y
        iny
        dex
        bne .text_l3
        ldx #40
        ldy #0
        lda #LINE_CHAR
.text_l4
	sta $07D0,Y
        iny
        dex
        bne .text_l4
        ldx #40
        ldy #0
        rts
player_pos
	BYTE 23 - (#PADDLE_LENGTH / 2)
player_dir
	BYTE 0
ai_pos
	BYTE 23 - #PADDLE_LENGTH 
ai_dir
	BYTE
ball_dir
	WORD $101
ball_pos
	byte #20
        byte #23
exit_str
	byte "Bye Bye"
        byte $8D
