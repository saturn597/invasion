; Invasion - a simple NES game.

; Constants affecting game behavior
BADDYCOUNT   = 3
SHOTDELTA    = 2
SPRITECOUNT  = 2

; Game states
PAUSED     = $80

; Gamepad states
PAD_A      = $80
PAD_B      = $40
PAD_SELECT = $20
PAD_START  = $10
PAD_U      = $08
PAD_D      = $04
PAD_L      = $02
PAD_R      = $01


;
; iNES header (needed for emulators)
; see https://wiki.nesdev.com/w/index.php/INES for header format
;

.segment "HEADER"

.byte 'N', 'E', 'S', $1A
.byte $02  ; tell emulator to expect 2 16 KB chunks of code for game
.byte $01  ; tell emulator to expect 1 8 KB chunk of CHR/graphics data
.byte %00000001  ; vertical mirroring/no battery backed memory/no trainer/NROM
.byte $00  ; NROM/not NES 2.0/not playchoice-10/not a vs game
.byte $00  ; assumes 8 KB of PRG RAM
.byte $00  ; rarely used TV system info and reserved bits
.byte $00  ; not part of official spec
.byte $00, $00, $00, $00, $00  ; zero-filled, pads header to 16 bytes


;
; Vectors - addresses for nmi, reset, irq.
;

.segment "VECTORS"

.word nmi
.word reset
.word irq


;
; Tiles - graphics data for sprites.
;
.segment "TILES"
.incbin "tiles.chr"


;
; Reserve memory for OAM (object attribute memory/sprite data).
;

.segment "OAM"
oam:
oamplayer: .res 8
oambaddy:  .res (BADDYCOUNT * 16)
oambullet: .res 4



;
; Zero page memory.
;

.segment "ZEROPAGE"
game_state:    .res 1
gamepad:       .res 1
gamepad_tmp:   .res 1
gamepad_old:   .res 1
player_x:      .res 1
player_y:      .res 1
shot_x:        .res 1
shot_y:        .res 1
baddies:       .res 8

;
; Code segment - the main code for the game.
;

.segment "CODE"

read_gamepad:
  ;; Gamepad process: write 1 and 0 to $4016 to latch current button positions.
  ;; Then read from $4016 to get the state of the "A" button. Read from $4016
  ;; repeatedly to get the rest of the buttons. The button states are always in
  ;; the last bit.
  ;;
  ;; In this subroutine, we want to get all button states into a single byte
  ;; for easy access elsewhere.
  ;;
  ;; Based on: http://nintendoage.com/forum/messageview.cfm?catid=22&threadid=8747
  ;; But with my own tweaks.

  lda #$01
  sta $4016
  lda #$00
  sta $4016
  sta gamepad_tmp  ; use temp variable to avoid NMI-related concurrency issues

  ldx #$08
  :
    lda $4016
    lsr
    rol gamepad_tmp
    dex
    bne :-
  lda gamepad_tmp
  sta gamepad
  rts

read_gamepad_alt:
  ;; An alternate method of reading gamepad for reference.
  ;; From http://forums.nesdev.com/viewtopic.php?t=11151

  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
  :
    pha               ; Push A to stack
    lda $4016         ; Read next button
    and #%00000001    ; Rainwarrior's example uses #%11, not #%01 - why?
    cmp #%00000001    ; If A equals this (or is greater), this sets carry bit.
    pla               ; Pull top byte in stack into A.
    rol               ; Rotate left puts carry into rightmost bit.
    dex
    bne :-            ; Repeat.
  sta gamepad         ; We now have the controller state in a single byte.
  rts

nmi:
  ;; NMI routine. Transfer sprite to PPU and then make updates based on gamepad
  ;; state.

  ; Save registers so we can restore them after nmi routine
  pha
  txa
  pha
  tya
  pha

  lda game_state
  and #PAUSED
  beq :+
    ; restore registers
    pla
    tay
    pla
    tax
    pla

    rti
  :

  ; DMA transfer sprite date to the PPU
  lda #<oam
  sta $2003      ; OAM address. Low byte of address goes here.
  lda #>oam
  sta $4014      ; OAM DMA. High byte goes here, and this starts DMA.

  ; Default sprite palette if player isn't pressing select
  lda gamepad
  and #(PAD_SELECT)
  bne :+
    lda oamplayer + 2
    and #%11111000
    sta oamplayer + 2
    sta oamplayer + 6
  :

  ; respond to button pushes
  lda gamepad
  and #PAD_R
  beq :+
    inc player_x
  :
  lda gamepad
  and #PAD_L
  beq :+
    dec player_x
  :
  lda gamepad
  and #PAD_U
  beq :+
    dec player_y
  :
  lda gamepad
  and #PAD_D
  beq :+
    inc player_y
  :
  lda gamepad
  and #PAD_B
  beq :+
    lda shot_y         ; If shot is already on screen, shot_y will be > 0.
    bne :+             ; In that case, don't start a new shot.

    lda player_y
    sec
    sbc #$08           ; Start it out a bit above the player.
    sta shot_y

    lda player_x
    clc
    adc #$04           ; Set the x location to center over the player.
    sta shot_x
  :
  lda gamepad
  and #PAD_A
  beq :+
    inc player_x
    inc player_x
  :
  lda gamepad
  and #PAD_SELECT
  beq :+
    jsr cycle_palettes
  :

  ; Move shot up and add to OAM if y != 0
  lda shot_y
  beq @after_shot
    sec
    sbc #SHOTDELTA       ; Move shot up
    cmp #SHOTDELTA
    bcc @destroy_shot    ; Destroy shot if it left or is about to leave screen
    sta shot_y           ; Write new y value to shot_y
    sta oambullet        ; Also put it in OAM.
    lda shot_x           ; Put x in OAM as well. (Could probably take this out).
    sta oambullet + 3
    jmp @after_shot
  @destroy_shot:
    ; To destroy shot, write 0 to shot_y since that tells us it's "inactive."
    lda #$00
    sta shot_y
    ; Also, put it off screen in oam.
    lda #$ff
    sta oambullet
    sta oambullet + 3
  @after_shot:

  ; Move baddies
  ldx #$00
  :
    inc baddies + 1, X
    inx
    inx
    cpx #(BADDYCOUNT*2)
    bne :-

  ; put new player positions into oam
  lda player_x
  sta oamplayer + 3
  clc
  adc #$08
  sta oamplayer + 7

  lda player_y
  sta oamplayer
  sta oamplayer + 4

  ; restore registers
  pla
  tay
  pla
  tax
  pla

  rti

cycle_palettes:
  lda oamplayer + 2
  clc
  adc #$01
  and #$03
  sta oamplayer + 2
  lda oamplayer + 6
  and #%11111000
  ora oamplayer + 2
  sta oamplayer + 2
  sta oamplayer + 6
  rts

game_loop:
  ; Toggle game pause state if pause was up but is now down
  jsr read_gamepad
  lda gamepad_old
  eor #PAD_START    ; Negate the pad_start bit in gamepad_old
  and gamepad
  and #PAD_START
  beq @end_pause_toggle
    lda game_state
    eor #PAUSED     ; toggle paused state
    sta game_state

    ; Brighten or darken screen to reflect pause state
    and #PAUSED
    beq @brighten_screen
    @darken_screen:
      lda #%11110000
      sta $2001
      jmp @end_pause_toggle
    @brighten_screen:
      lda #%00010000
      sta $2001
  @end_pause_toggle:

  ; Save current gamepad state for next time
  lda gamepad
  sta gamepad_old

  ; Check for collisions
  ldy #$00
  @collide_baddies:
    tya
    asl
    asl
    asl
    tax
    lda shot_y
    cmp baddies, Y
    bcc @end_collision_check
    ; bullet above
    lda shot_x
    cmp baddies + 1, Y
    bcc @end_collision_check
    ; bullet to right
    lda shot_y
    sec
    sbc #$10
    cmp baddies, Y
    bcs @end_collision_check
    ; bullet vertically aligned
    lda shot_x
    sec
    sbc #$10
    cmp baddies + 1, Y
    bcs @end_collision_check
    ; bullet horizontally aligned, thus collision
    ; Set y coordinate to 0 to indicate the baddy is "destroyed"
    lda #$00
    sta baddies, Y
    ; Also destroy bullet.
    lda #$00
    sta shot_y
    ; Put bullet off screen in oam.
    lda #$ff
    sta oambullet
    sta oambullet + 3

    @end_collision_check:
    iny
    iny
    cpy #(BADDYCOUNT*2)
    bne @collide_baddies

  ; Put baddy data into oam
  ldy #$00
  @baddy_oam_loop:
    ; Each iteration of this loop sets up one baddy.
    ; Each baddy is composed of 4 sprites, so set up oam data for each.

    ; Multiply Y by 8 to figure out our position within oam
    tya
    asl
    asl
    asl
    tax

    ; Baddy vertical position
    lda baddies, Y
    bne @baddy_alive             ; we set y pos to 0 if baddy is destroyed
      lda #$ff                   ; in that case, move all sprites off screen
      sta oambaddy, X
      sta oambaddy + 4, X
      sta oambaddy + 8, X
      sta oambaddy + 12, X
      jmp @end_baddy_oam_loop
    @baddy_alive:
    sta oambaddy, X
    sta oambaddy + 4, X
    clc
    adc #$08
    sta oambaddy + 8, X
    sta oambaddy + 12, X

    ; Baddy horizontal position.
    ; This will be 3 bytes from the vertical position for each sprite.
    lda baddies + 1, Y
    sta oambaddy + 3, X
    sta oambaddy + 11, X
    clc
    adc #$08
    sta oambaddy + 7, X
    sta oambaddy + 15, X

    ; Baddy tiles
    ; This will be 1 byte from the vertical positions.
    lda #$04
    sta oambaddy + 1, X
    lda #$05
    sta oambaddy + 5, X
    lda #$02
    sta oambaddy + 9, X
    lda #$03
    sta oambaddy + 13, X

    ; Baddy attributes (just palettes for now)
    ; This will be 2 bytes from the vertical positions.
    lda #$01
    sta oambaddy + 2, X
    sta oambaddy + 6, X
    sta oambaddy + 10, X
    sta oambaddy + 14, X

    @end_baddy_oam_loop:
    iny
    iny

    cpy #(BADDYCOUNT*2)
    bne @baddy_oam_loop

  jmp game_loop

reset:
  sei              ; disable interrupts
  cld              ; disable decimal mode
  ldx #%01000000
  stx $4017        ; Stop certain APU interrupts.
  ldx #$ff         ; Stack goes $01ff to $0100, starting at $01ff.
  txs              ; S register is stack pointer.
  inx              ; X is now 0.
  stx $2000        ; Disable nmi.
  stx $2001        ; Disable rendering.
  stx $4010        ; More APU settings.

  ; We have to wait for vblank to ensure PPU is ready.
  ; Loop until vblank flag (bit 7 of PPU status) is set. (BIT copies bit 7
  ; at the location given to the negative flag, so can test result with bpl).
  ; Also, vblank bit could be set at reset, so have to clear it first by
  ; reading the PPU status once before starting.
  bit $2002
  :
    bit $2002
    bpl :-

  ; Now set ram to known state.
  ; (All zeroes, except place sprites off screen).
  lda #$00
  ldx #$00
  :
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne :-

  lda #$ff
  ldx #$00
  :
    sta oam, x
    inx
    inx
    inx
    inx
    bne :-

  ; wait for one more vblank
  :
    bit $2002
    bpl :-

  ; Load palettes
  lda $2002   ; Read PPU status to reset address high/low latch
  lda #$3f    ; PPU palettes start at #3f00. Write high byte first.
  sta $2006   ; Write to PPU address register.
  lda #$00    ; Now write low byte to address register.
  sta $2006

  ldx #$00
  :
    lda palettes, x
    sta $2007        ; PPU data register.
    inx
    cpx #$20
    bne :-

  ldx #$00
  :
    lda sprites, x
    sta oamplayer, x
    inx
    cpx #SPRITECOUNT*4
    bne :-

  ; initialize sprite location
  lda oamplayer
  sta player_y
  lda oamplayer+3
  sta player_x

  ; initialize baddies
  ldx #$00
  ldy #$40
  lda #$40
  :
    sty baddies, X
    clc
    adc #$20
    sta baddies + 1, X
    inx
    inx
    cpx #(BADDYCOUNT*2)
    bne :-

  ; Initialize "shot" location
  lda #$00
  sta shot_x
  sta shot_y

  ; And shot palette data.
  lda #$06
  sta oambullet + 1

  lda #%10000000  ; Turn on NMI
  sta $2000       ; PPU controller

  lda #%00010000  ; Enable sprites
  sta $2001       ; PPU mask

  jsr game_loop

irq:
  rti


.segment "RODATA"

palettes:
  .byte $0f,$31,$32,$33,$0f,$35,$36,$37,$0f,$39,$3a,$3b,$0f,$3d,$3e,$0f
  .byte $39,$17,$28,$15, $39,$19,$0c,$15, $0f,$1c,$15,$14, $0f,$13,$25,$24

sprites:
  .byte $80,$00,$00,$80
  .byte $80,$01,$00,$88
