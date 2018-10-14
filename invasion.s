; Invasion - a simple NES game.


; iNES header (needed for emulators)
; see https://wiki.nesdev.com/w/index.php/INES for header format

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


