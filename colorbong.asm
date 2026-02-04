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
TEMP_X equ $D7
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
        lda #PLAYER_COLOR
        sta COLOR_ADDR
        lda #0
        jsr Render_Paddle
        lda #AI_COLOR
        sta COLOR_ADDR
        lda #1
        jsr Render_Paddle
        lda #BALL_COLOR
        sta COLOR_ADDR
        jsr Render_Ball
      	jmp Game_Loop
Finish
	debug_plot 22,22
	jmp *

Game_Loop
        jsr Handle_Ai_paddle
      	jsr Handle_Player_Input
        jsr Handle_Player_Movement
        lda game_tick_counter
        cmp #255
        bne *+7
        jsr Handle_Ball_Movement
        lda #0
        clc
        adc #1
   	sta game_tick_counter
        lda #0
        beq Game_Loop
Handle_Ai_paddle
	lda game_tick_counter
        cmp #70
        beq .handle_ai_paddle_main
        rts
.handle_ai_paddle_main
	lda ball_dir
        bpl *+3
        rts
	lda #00
        sta COLOR_ADDR
        lda #1
        jsr Render_Paddle
	lda ai_pos
        cmp ball_pos + 1
        bcs .move_ai_higher
        beq .ai_no_change
.move_ai_lower
	lda #1
        sta ai_dir
        jmp .move_ai_paddle 
.move_ai_higher
	lda #-1
        sta ai_dir
        jmp .move_ai_paddle
.ai_no_change
	lda #0
        sta ai_dir
.move_ai_paddle
	clc
	adc ai_pos
        sta ai_pos
        cmp #0
       	bcc .out_of_bounds_high
        beq .out_of_bounds_high
        cmp #39 - PADDLE_LENGTH
        bcs .out_of_bounds_low
.draw_ai_paddle
	lda #AI_COLOR
        sta COLOR_ADDR
        lda #1
        jsr Render_Paddle
        rts
.out_of_bounds_high
        lda #0
        sta ai_pos
        beq .draw_ai_paddle
.out_of_bounds_low
	lda #39 - PADDLE_LENGTH
        sta ai_pos
        lda #0 
        beq .draw_ai_paddle
Handle_Ball_Movement
	lda #00
        sta COLOR_ADDR
        jsr Render_Ball
	lda ball_dir + 1
        clc
        adc ball_pos + 1
        sta ball_pos + 1
        cmp #0
        bne *+7
        lda #1
        sta ball_dir + 1
        cmp #39
        bne *+7
        lda #-1
        sta ball_dir + 1
        lda ball_dir
        clc
        adc ball_pos
        sta ball_pos
        cmp #PLAYER_X
        beq .check_player_paddle
        cmp #AI_X
        bcs .check_ai_paddle
.handle_mov_draw
        lda #BALL_COLOR
        sta COLOR_ADDR
        jsr Render_Ball
        rts 
.check_player_paddle
        sta ball_pos
        lda ball_pos + 1
        ldy player_pos
        ldx #PADDLE_LENGTH
        lda #0
.loop_player_paddle
	cpy ball_pos + 1
        beq *+8
        iny
        dex
        beq .handle_ball_exit
        bne .loop_player_paddle 
        cpx #6
        bcc .player_check_middle_and_lower
        lda #1 ;makes top half go up
        sta ball_dir
        sta ball_dir+1
        jmp .handle_mov_draw
.player_check_middle_and_lower
	cpx #5
       	bne .player_col_lower
        lda #0
        sta ball_dir+1
        lda #1
        sta ball_dir
        jmp .handle_mov_draw
.player_col_lower
	lda #1
        sta ball_dir
        lda #-1
        sta ball_dir +1
       	jmp .handle_mov_draw
.check_ai_paddle
        sta ball_pos
        lda ball_pos + 1
        ldy ai_pos
        ldx #PADDLE_LENGTH
        lda #1
.loop_ai_paddle
	cpy ball_pos + 1
        beq *+8
        iny
        dex
        beq .handle_ball_exit
        bne .loop_ai_paddle 
        cpx #6
        bcc .ai_check_middle_and_lower
        lda #1 ;makes top half go up
        sta ball_dir
        lda #-1
        sta ball_dir+1
        jmp .handle_mov_draw
.ai_check_middle_and_lower
	cpx #5
       	bne .ai_col_lower
        lda #0
        sta ball_dir+1
        lda #-1
        sta ball_dir
        jmp .handle_mov_draw
.ai_col_lower
	lda #-1
        sta ball_dir
        sta ball_dir +1
       	jmp .handle_mov_draw
.handle_ball_exit
	cmp #0
        beq .player_missed
.ai_missed
	lda #-1
        sta ball_dir
        lda #0
        sta ball_dir + 1
        jmp .missed_finis
.player_missed
	lda #1
        sta ball_dir
        lda #0
        sta ball_dir + 1
.missed_finis
	lda #20
        sta ball_pos
        lda #23
        sta ball_pos + 1
        jmp .handle_mov_draw

Handle_Player_Movement
	lda player_dir
        bne .handle_movement_body ; if direction is 0 exit routine
        rts
.handle_movement_body        
        clc
        adc player_pos
        cmp #31 ; check low  game bounds
        bne *+3
        rts
        cmp #-1 ; check high game bounds
        bne *+3 
        rts
        sta TEMP_X
	lda #00 		;load black to clear current player pos
        sta COLOR_ADDR
        jsr Render_Paddle
       	lda TEMP_X
        sta player_pos
        lda #PLAYER_COLOR
        sta COLOR_ADDR
        lda #0
        jsr Render_Paddle
        rts
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
        rts
.handle_w_key
	lda #-1
        sta player_dir
        rts
.handle_s_key
	lda #1
        sta player_dir
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
	BYTE #23 - (PADDLE_LENGTH / 2)
player_dir
	BYTE 0
ai_pos
	BYTE 23 - #PADDLE_LENGTH 
ai_dir
	BYTE
ball_dir
	BYTE #-1
        BYTE #0
ball_pos
	byte #20
        byte #23
game_tick_counter ;things happen when it reaches a certian number
	byte #0
