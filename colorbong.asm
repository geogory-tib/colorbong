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
PADDLE_LENGTH equ 8
BALL_COLOR equ $11
TEMP_X equ $D7
SPEAKER equ $C030
ZERO_PAGE_REG equ $06
ZERO_PAGE_REG_2 equ $CE
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
        jsr Render_Bottom_Display
      	jmp Game_Loop
Finish
	debug_plot 22,22
	jmp *

Game_Loop
	lda #1
        cmp score_changed_flag
        bne *+5
        jsr Render_Bottom_Display
	lda game_tick_counter
	cmp #0
        bne *+13
        jsr Handle_Ai_paddle
        jsr Handle_Ball_Movement 
        lda #0
        sta game_tick_counter
        clc
        adc #1
        sta game_tick_counter
      	jsr Handle_Player_Input
        jsr Handle_Player_Movement
        lda #0
        beq Game_Loop
Handle_Ai_paddle
	lda ai_pause
        clc
        adc #1
        cmp #6 ;if the ai_pause value == 6 it skips the routine
        bne *+8
        lda #0
        sta ai_pause 
        rts
        sta ai_pause
	lda ball_dir
        bpl *+3
        rts
	lda #00
        sta COLOR_ADDR
        lda #1
        jsr Render_Paddle
        sta ZEROPAGE_ADDR
	lda ai_pos
        clc 
        adc #(PADDLE_LENGTH / 2) - 1
        cmp ball_pos + 1
        beq .ai_no_change
        bcs .move_ai_higher
        
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
	lda ai_pos
        clc
        adc ai_dir
        sta ai_pos
        cmp #0
       	bcc .out_of_bounds_high
        beq .out_of_bounds_high
        cmp #$FF
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
        bpl *+7
        lda #1
        sta ball_dir + 1
        cmp #39
        bcc *+7
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
        lda #0 ; used to tell which paddle missed the ball 0 for player 1 for AI
.loop_player_paddle
	cpy ball_pos + 1
        beq *+8
        iny
        dex
        beq .leave_player_check
        bne .loop_player_paddle
        jsr Play_Beep
        cpx #6
        bcc .player_check_middle_and_lower
        lda #1 
        sta ball_dir
        lda #-1
        cpx #8
        bcc .skip_higher_vert
        lda #-2
.skip_higher_vert
        sta ball_dir+1
       	jmp .handle_mov_draw
.leave_player_check
	jmp .handle_ball_exit
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
        lda #1
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
        beq .ai_col_dected
        iny
        dex
        beq .handle_ball_exit
        bne .loop_ai_paddle
.ai_col_dected
	jsr Play_Beep
        cpx #6
        bcc .ai_check_middle_and_lower
        lda #-1;makes top half go up
        sta ball_dir
        cpx #8 ;check if it hit the tip
        bne *+4
        lda #-2
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
        lda #1
        sta ball_dir +1
       	jmp .handle_mov_draw
.handle_ball_exit
	cmp #0
        beq .player_missed
.ai_missed
	lda #<player_score_buf
        sta ZEROPAGE_ADDR
        lda #>player_score_buf
        sta ZEROPAGE_ADDR+1
        jsr Inc_Score_counters
	lda #-1
        sta ball_dir
        lda #0
        sta ball_dir + 1
        jmp .missed_finis
.player_missed
	lda #<ai_score_buf
        sta ZEROPAGE_ADDR
        lda #>ai_score_buf
        sta ZEROPAGE_ADDR + 1
        jsr Inc_Score_counters
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
        
Inc_Score_counters ; since actually doing a int to str conversion is reall taxing 
                   ;Im just adding 1 to the ascii values then doing number place managing myself
                   ; since it would be less expensive and easier to do it this way for a score display
        lda #1
        sta score_changed_flag
        ldy #1
        lda (ZEROPAGE_ADDR),Y
        cmp #'9 + 128
        beq .overflow_counter
        lda (ZEROPAGE_ADDR),Y
        clc
        adc #1
        sta (ZEROPAGE_ADDR),y
        rts
.overflow_counter
	lda #'0 + 128
        sta (ZEROPAGE_ADDR),y
        ldy #0
        lda (ZEROPAGE_ADDR),y
        clc
        adc #1
        sta (ZEROPAGE_ADDR),y
        rts
Render_Bottom_Display 
	lda #<$06D0
        sta ZERO_PAGE_REG
        lda #>$06D0
        sta ZERO_PAGE_REG + 1
        lda #<player_score_msg
        sta ZEROPAGE_ADDR
        lda #>player_score_msg
        sta ZEROPAGE_ADDR + 1
        jsr PutStr
       	clc
        adc ZERO_PAGE_REG
        sta ZERO_PAGE_REG
        lda #<player_score_buf
        sta ZEROPAGE_ADDR
        lda #>player_score_buf
        sta ZEROPAGE_ADDR + 1
        jsr PutStr
       	lda #<$0750
        sta ZERO_PAGE_REG
        lda #>$750
        sta ZERO_PAGE_REG + 1
        lda #<ai_score_msg
        sta ZEROPAGE_ADDR
        lda #>ai_score_msg
        sta ZEROPAGE_ADDR + 1
        jsr PutStr
        clc
        adc ZERO_PAGE_REG
        sta ZERO_PAGE_REG
        lda #<ai_score_buf
        sta ZEROPAGE_ADDR
        lda #>ai_score_buf
        sta ZEROPAGE_ADDR + 1
        jsr PutStr
        lda #0
        sta score_changed_flag
        rts
        
PutStr
	ldy 0
        lda 0
.put_str_loop
	lda (ZEROPAGE_ADDR),Y
        beq .put_str_exit
        sta (ZERO_PAGE_REG),Y
        iny
        bne .put_str_loop
     
.put_str_exit
	tya
	rts
        
Play_Beep
	pha
        txa
        pha
        tya
        pha
       	ldy #$40 ;duration
.delay_reset
        ldx #$12 ; pitch 
.beep_loop
        sta SPEAKER
        dex
        bne .beep_loop
        dey
        bne .delay_reset
.beep_exit
        pla
        tay
        pla
        tax
        pla
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
ai_pause ; ai skips a tick every 5 tics
	byte #0
player_score
	byte 0
player_score_msg
	byte 'H + 128, 'U + 128, 'M + 128, 'A + 128, 'N + 128, ': + 128, 0
player_score_buf
	byte '0 + 128
        byte '0 + 128
        byte 0
ai_score
	byte '0 + 128
        byte '0 + 128
	byte 0
ai_score_msg
	byte 'A + 128,'I + 128, ': +128,0 
ai_score_buf
	byte '0 + 128
        byte '0 + 128
        byte 0 
score_changed_flag
	byte 0