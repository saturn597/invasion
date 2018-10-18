; Invasion - a simple NES game.


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
; Reserve memory for OAM (object attribute memory/sprite data).
;

.segment "OAM"
oam: .res 256


;
; Code segment - the main code for the game.
;

.segment "CODE"

nmi:
  rti


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

  lda #%01000000  ; emphasize greens
  sta $2001

  :
    jmp :-


irq:
  rti
