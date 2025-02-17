﻿; SKTP (Sidekick64 transfer protocol) client
; for Commodore 64/+4 with WiC64
; Copyright (C) 2023 Henning Pingel
; wic-library and +4 adaptation by SukkoPera 2024
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Assembler used: ACME Cross-Assembler
; https://sourceforge.net/projects/acme-crossass/

!if PLUS4 {
    !addr SCREEN_RAM = $0c00        ; Up to $0ae7 (?)
    !addr COLOR_RAM = $0800         ; FIXME

    CHROUT = $ffd2
    GETIN = $ffe4

    BACKGROUND = $ff15
    BORDER = $ff19
} else {
    !addr SCREEN_RAM = $0400        ; Up to $07e7
    !addr COLOR_RAM = $d800         ; Up to $dbe7

    CHROUT = $ffd2
    GETIN = $ffe4

    BACKGROUND = $d020
    BORDER = $d021
}

; Clear the screen
!macro clear_screen {
    lda #$93                         ; This works on both C64 and +4
    jsr CHROUT
}

; Print a null-terminated string (Max length = 254)
!macro printStr .msg {
    ldy #0
-   lda .msg,y
    beq +
    jsr CHROUT
    iny
    jmp -
+
}

CHARS_PER_LINE = 40

; Color values for poking into Color RAM
!if PLUS4 {
; +4 has a palette of 128 colors (121 different), bits 6..4 control the luminance, while 3..0 control the color
; These are the KERNAL values, taken from memory location $0113
COLOR_BLACK = $00               ; CTRL + 1
COLOR_WHITE = $71               ; CTRL + 2
COLOR_RED = $32                 ; CTRL + 3
COLOR_CYAN = $63                ; CTRL + 4
COLOR_PURPLE = $44              ; CTRL + 5
COLOR_GREEN = $35               ; CTRL + 6
COLOR_BLUE = $46                ; CTRL + 7
COLOR_YELLOW = $77              ; CTRL + 8
COLOR_ORANGE = $48              ; C= + 1
COLOR_BROWN = $29               ; C= + 2

; +4 "exclusive" colors
COLOR_YELLOW_GREEN = $5a        ; C= + 3
COLOR_LIGHT_RED = $6b           ; C= + 4
COLOR_BLUE_GREEN = $5c          ; C= + 5
COLOR_LIGHT_BLUE = $6d          ; C= + 6
COLOR_DARK_BLUE = $2e           ; C= + 7
COLOR_LIGHT_GREEN = $5f         ; C= + 8

; Close matches for other C64 colors (by me)
COLOR_DARK_GREY = $31
COLOR_GREY = $41
COLOR_LIGHT_GREY = $51
} else {
; C64 can only display 16 (mostly horrible) colors
COLOR_BLACK = 0
COLOR_WHITE = 1
COLOR_RED = 2
COLOR_PURPLE = 4
COLOR_GREEN = 5                 ; Oh, you've got green eyes...
COLOR_BLUE = 6                  ; ... Oh, you've got blue eyes...
COLOR_YELLOW = 7
COLOR_ORANGE = 8
COLOR_BROWN = 9
COLOR_CYAN = 3
COLOR_LIGHT_RED = 10
COLOR_DARK_GREY = 11
COLOR_GREY = 12                 ; ... Oh, you've got GREEEEEEEEEY EEEEEEYYYEEEESSS!
COLOR_LIGHT_GREEN = 13
COLOR_LIGHT_BLUE = 14           ; Default char color in BASIC
COLOR_LIGHT_GREY = 15
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!if PLUS4 {
    wic_name = "WiC+4"
} else {
    wic_name = "WiC64"
}

sktp_server = "http://sktpdemo.cafeobskur.de"
;~ sktp_server = "http://localhost"
;~ sktp_server = "http://192.168.2.1"

build_date = "2024-07-17"
client_version = "0.24"

; Communication timeouts (seconds)
REMOTE_TIMEOUT = 30
TRANSFER_TIMEOUT = REMOTE_TIMEOUT

; BASIC launcher
!if PLUS4 {
    * = $1001 ; 10 SYS 4109 ($100d)

    !word nextln, 0     ; second word is line number
    !byte $9e           ; SYS
    !pet "4109"         ; Address (in string format)
    !byte 0             ; End of instruction
nextln:
    !byte 0, 0          ; End of BASIC program

    * = 4109
} else {
    * = $0801 ; 10 SYS 2064 ($0810)
    !byte $0c, $08, $0a, $00, $9e, $20, $32, $30, $36, $34, $00, $00, $00

    * = $0810
}

!addr {
    ;zeropage addresses used
    tmp            = $a6
    data_pointer   = $a7 ; $a7/$a8 adress for data
    data_pointer2  = $a8 ; $a7/$a8 adress for data
    color_pointer  = $a9 ; $a9/$aa adress for data
    color_pointer2 = $aa ; $a9/$aa adress for data
    mpCount        = $ab; multi purpose counter
    ;we are using mpCount for screenMetaRefresh and also
    ;for verticalrepeatscreencode chunk as the refresh chunk
    ;is always near the end of a screen and all vrsc chunks
    ;are already processed by then.

    scratch_zptr = $14      ; word, also uses $15, can be used whenever a scratch pointer is necessary
    next_resp_byte = $fe    ; word, also uses $ff, index of next byte or response to be returned from read_byte
}


;============================================
; Program entry point
;============================================
    jmp start

!src "wic64.h"
!src "wic64.asm"

!if 0 {
debugOutputScreenLength:
    ;prints out remaining screen byte length
    lda sktpScreenLengthH
    jsr printHex
    lda sktpScreenLengthL
    jsr printHex
    lda #','
    jsr CHROUT
    rts
}

setBothColors:
    sta BACKGROUND
    sta BORDER
    rts

printDLURLStuff:
    ; Print "File launching...", etc
    +printStr nodoloMSG

!if DEBUG_DOWNLOAD=1 {
    lda #$0d
    jsr CHROUT
}

    ; We have the URL length handy, so let's just write it in the proper place for the command we'll call later
    lda sktpChunkType
    sta dlurl_start+2

    ; Print URL while copying it to dlurl_netto_start
    ldy #00
-   jsr read_byte
    sta dlurl_netto_start,y
!if DEBUG_DOWNLOAD=1 {
    ;TODO ascii 2 petscii here
;    jsr ascii2screencode
    jsr CHROUT
}
    iny
    dec sktpChunkType
    bne -

    ; Terminate
    lda #00
    sta dlurl_netto_start,y

!if DEBUG_DOWNLOAD=1 {
    ; 2 newlines
    lda #$0d
    jsr CHROUT
    lda #$0d
    jsr CHROUT
}

    ; Print download filename (Note it's not used in any other way)
-   jsr read_byte
!if DEBUG_DOWNLOAD=1 {
    ;TODO ascii 2 petscii here
;    jsr ascii2screencode
    jsr CHROUT
}
    dec sktpChunkLengthL
    bne -
    
    jmp requestDownloadURL          ; Will download and start


;============================================
; Actual program start
;============================================
start:
    +clear_screen
    lda #(COLOR_DARK_GREY)
    jsr setBothColors
    lda #5              ; white font color
    jsr CHROUT
    lda #14             ; switch to lowercase
    jsr CHROUT

    ; Configure function keys so that they produce the same char codes as the C64
!if PLUS4=1 {
configure_fn_keys:
    ldy #8
-   lda #1              ; Length = 1
    sta $055f-1,y
    lda .fnchrs-1,y     ; Actual char
    sta $0567-1,y
    dey
    beq .welcome
    jmp -

                ;      F1   F2   F3   F4   F5   F6   F7   HELP
;~ .fnchrs:        !byte $85, $89, $86, $8A, $87, $8B, $8C, $88
.fnchrs:        !byte $85, $86, $87, $89, $8a, $8b, $8c, $88
}

    ; Show welcome message
.welcome:
    +printStr welcomeMsg

    ; See if WicXX is there
    jsr detect_wic
    bcc renewSessionID
    +printStr wicNotDetectedMsg     ; No, it's not :(
    jmp program_end

renewSessionID:
    ; OK, card found, try to get session ID
    +printStr requestSessIdMsg

	; Offset of the char indicating wait/success/failure
    INDICATOR_OFFSET = CHARS_PER_LINE * 15 + 32
    
    ; start with indicator in yellow color
    lda #(COLOR_YELLOW)
    sta COLOR_RAM + INDICATOR_OFFSET
    
    ; get and store sktp session id
    jsr request_sessionid
    bcc +              				 ; Carry clear if no errors

    ; failure, update indicator color to red and quit
    lda #(COLOR_RED)
    sta COLOR_RAM + INDICATOR_OFFSET
    jmp program_end

    ; ok, update indicator (light green tick)
+   lda #122
    sta SCREEN_RAM + INDICATOR_OFFSET
    lda #(COLOR_LIGHT_GREEN)
    sta COLOR_RAM + INDICATOR_OFFSET

    ; send the command to set the default server to wic
    jsr sendURLPrefixToWic


	+printStr pressAnyKeyMsg
waitkeypressW:
    jsr GETIN
    beq waitkeypressW

sendSKTPRequest:
    jsr sendSKTPCommand
    jsr getresponse
    rts

renewSessionID_trampolin:
    jmp renewSessionID

downloadURL:
    +clear_screen
    lda #(COLOR_DARK_GREY)
    jsr setBothColors
    ;switch to lowercase
    lda #14
    jsr CHROUT

    jsr read_byte ; url length
    sta sktpChunkType
    jsr read_byte ; filename length
    sta sktpChunkLengthL
    jsr read_byte ; save flag (??? Ignored, anyway)
    jmp printDLURLStuff

getresponse:
    lda wic64_response_size+1
    sta sktpScreenLengthH

    lda wic64_response_size
    sta sktpScreenLengthL

    ;lengthdebug
    ;+clear_screen
    ;jsr debugOutputScreenLength

    ;subtract one from length as this is the screen type byte
    lda sktpScreenLengthL
    bne jumpPositive
    dec sktpScreenLengthH
jumpPositive:
    dec sktpScreenLengthL

    ;lengthdebug
    ;jsr debugOutputScreenLength

    ;sktp screen type, first byte of response
    jsr read_byte
    sta sktpScreenType

    ;screen types:
    ;0 = clear screen first
    ;1 = leave previous screen standing and paint over it
    ;2 = file download
    ;3 = session timed out, get new session id
    cmp #2
    beq downloadURL
    cmp #4
    bcs illegalScreenType
    cmp #3
    beq renewSessionID_trampolin		; Too far for branching
    cmp #0
    bne parseChunk
    +clear_screen     ; Clr screen = disable on lengthdebug

    jmp parseChunk

illegalScreenType:
    +clear_screen
    lda #(COLOR_CYAN)
    jsr setBothColors
    ;switch to lowercase
    lda #14
    jsr CHROUT
    ;ERR:
    lda #'e'
    jsr CHROUT
    lda #'r'
    jsr CHROUT
    jsr CHROUT
    lda #':'
    jsr CHROUT
    lda #' '
    jsr CHROUT
    lda #'$'
    jsr CHROUT
    lda sktpScreenType
    jsr printHex
    lda #' '
    jsr CHROUT
    ldy #0
loopErrorMsg:
    lda errormsg_IllegalScreen,y
    cmp #0
    beq endOfErrorMsg
    jsr CHROUT
    iny
    jmp loopErrorMsg
endOfErrorMsg:
    jsr CHROUT
    rts


;jmp debug2
;    lda #"/"
;    jsr CHROUT
;    lda #"s"
;    jsr CHROUT
;    lda #"t"
;    jsr CHROUT
;    lda sktpScreenType
;    ora #%00110000
;    jsr CHROUT
;    lda #"/"
;    jsr CHROUT
;
;:debug2

;sktp chunk type:
;0 = normal,
;1 = repeat,
;2 = screencode,
;3 = meta_refresh,
;4 = colorcharset,
;5 = vertical repeat screencode
;6 = paintbrush,
;7 = tft_image_url
;8 = tft_image_inline


parseChunk:
    ;check if screen buffer is now empty and everything is in place
    ;and we are ready to listen to keypresses
    lda sktpScreenLengthL
    bne isnotempty
    lda sktpScreenLengthH
    bne isnotempty
    ldy #00 ; for delay that works via y
    jmp waitkey; is empty, now wait for user keypress and load next screen

isnotempty:
;    lda #255 ;tmp lengthdebug
;    sta sktpChunkType;tmp lengthdebug

    sec
    jsr read_byte
    ;dont parse chunks > chunk type 6
    sta sktpChunkType;tmp debug
    cmp #07
    bcc foundValidChunkType

    lda #'%'
    jsr CHROUT
    lda sktpChunkType
    jsr printHex
;    ora #%00110000
;    jsr CHROUT
    lda #'%'
    jsr CHROUT

;    jsr read_byte
;    sta sktpChunkType;tmp debug
;    lda sktpChunkType
;    jsr printHex
;;    ora #%00110000
;;    jsr CHROUT
;    lda #"%"
;    jsr CHROUT
;
;    jsr read_byte
;    sta sktpChunkType;tmp debug
;    lda sktpChunkType
;    jsr printHex
;;    ora #%00110000
;;    jsr CHROUT
;    lda #"%"
;    jsr CHROUT

    rts
foundValidChunkType:
    ;this cleanup could be done later after processing a chunk
    lda #00
    sta sktpChunkLengthL
    sta sktpChunkLengthH
    sta sktpChunkScrPosL
    sta sktpChunkScrPosH
    sta sktpChunkColor
    sta sktpNettoChunkLengthL
    sta sktpNettoChunkLengthH
    sta sktpChunkRptCount ;this is used by vertical repeat and paintbrush chunk

    jmp parseSKTPChunk

;============================================


;============================================
handleCharRepeatChunk: ; chunk type #1
;============================================
    ;always 6 bytes
    lda #5
    sta sktpNettoChunkLengthL
    jsr prepareStuff

    jsr read_byte ; char
    jsr ascii2screencode
    tax
    ;check if reverse flag is set, if so, modify screencode values
    lda sktpChunkColor
    cmp #128
    bcc endreversetest5
    txa
    adc #127
    tax
endreversetest5:
    ldy #00
charRepeat:
    txa
    sta (data_pointer),y
    tax
    lda sktpChunkColor
    sta (color_pointer),y
    iny
    cpy sktpChunkLengthL
    bne charRepeat
    jmp endOfChunkReached

;============================================
handleMetaRefreshChunk:
;============================================
    ;always 2 bytes
    lda #1
    sta sktpNettoChunkLengthL
    jsr read_byte ; char
;    jmp endOfChunkReached; disable refresh for now

    asl; workaround to have a longer waiting time
;    asl; workaround to have a longer waiting time
;    asl; workaround to have a longer waiting time

    sta mpCount
    jmp endOfChunkReached

;============================================
parseSKTPChunk:
;============================================
;waitkeypress:
;    jsr GETIN
;    beq waitkeypress

    lda sktpChunkType
    cmp #0
    beq handleNormalChunk
    cmp #1
    beq handleCharRepeatChunk
    cmp #2
    beq handleScreenCodeChunk
    cmp #3
    beq handleMetaRefreshChunk
    jmp parseSKTPChunkPart2

;============================================
handleNormalChunk: ; chunk type 0
;============================================
    jsr prepNSCC
chunk0_loop:
    jsr read_byte
    ;convert ascii to screencode
    jsr ascii2screencode
    tax
    ;check if reverse flag is set, if so, modify petscii values
    lda sktpChunkColor
    cmp #128
    bcc endreversetest0
    txa
    adc #127
    jmp render0

;============================================
handleScreenCodeChunk: ; chunk type 2
;============================================
    jsr prepNSCC ; also sets y to zero
chunk2_loop:
    jsr read_byte
    jsr renderCharAndColor ; increases y
    lda sktpChunkLengthH
    cmp #00
    beq processOnlyLowByte2
    ;highbyte is bigger than zero: we care about that first
    cpy #$ff; compare y with value 255
    bne chunk2_loop ; go up and read next byte
    ;finally we have read 255 bytes
    jsr decAndInc
    jmp chunk2_loop

processOnlyLowByte2:
    cpy sktpChunkLengthL ; compare y with the amount of bytes to read
    bne chunk2_loop
    jmp endOfChunkReached

decAndInc:
    dec sktpChunkLengthH

    lda data_pointer
    clc
    adc #255
    sta data_pointer
    bcc noOverflowDAI
    inc data_pointer2
noOverflowDAI:
    lda color_pointer
    clc
    adc #255
    sta color_pointer
    bcc noOverflowDAI2
    inc color_pointer2
noOverflowDAI2:
    inc sktpChunkLengthL; workaround
    ldy #00
    rts

endreversetest0:
    txa
render0:
    jsr renderCharAndColor
    ;is highbyte > zero? than care about that first
    lda sktpChunkLengthH
    cmp #00
    beq processOnlyLowByte0
    cpy #$ff
    bne chunk0_loop
    jsr decAndInc
    jmp chunk0_loop

processOnlyLowByte0:
    cpy sktpChunkLengthL
    bne chunk0_loop
    jmp endOfChunkReached

prepNSCC:
    jsr prepareStuff
    lda sktpChunkLengthH
    sta sktpNettoChunkLengthH
    ;always 5 chars + stringlenth
    lda #04
    clc
    adc sktpChunkLengthL
    sta sktpNettoChunkLengthL
    bcc noOverflow
    inc sktpNettoChunkLengthH
noOverflow:
    ldy #00
    rts

renderCharAndColor:
;    sta $0400+120,y
    sta (data_pointer),y
    lda sktpChunkColor
    sta (color_pointer),y
;    sta $d800+120,y
    iny
    rts

;============================================
handleColorCharsetChunk: ; chunk type #4
;============================================
    ;always 4 bytes
    lda #3
    sta sktpNettoChunkLengthL
    jsr readByteAndDecrease
    sta BACKGROUND
    jsr readByteAndDecrease
    sta BORDER
    jsr read_byte
    cmp #01
    beq switch2Lowercase
    lda #142 ;switch to uppercase
    jmp setCase
switch2Lowercase:
    lda #14 ;switch to lowercase
setCase:
    jsr CHROUT
    jmp endOfChunkReached

;============================================
handlePaintbrushChunk: ; chunk type 6
;============================================
    jsr prepareStuff
    ;always 6 chars + chars
    lda #05
    clc
    adc sktpChunkLengthL
    sta sktpNettoChunkLengthL
;    lda #00
;    clc
    lda sktpChunkLengthH ; sloppy but should always be 0
    sta sktpNettoChunkLengthH
    ;netto : this is used to calculate if we
    ;have reached the end of the whole sktp screen

    ;sktpChunkGap is already set up (through sktpChunkColor)

    ;after moving the sktpChunkColor value we can now put the gap
    ;value into sktpChunkColor aka sktpChunkGap
    ;(paintbrush chunk has color values as its content)
    jsr read_byte ; this is the gap value
    sta sktpChunkRptCount
    sta sktpChunkType ; backup repeat value

    ;the two bytes sktpChunkScrPos* can be used as helper vars
    ;here to backup the values of sktpChunkLength*
    ;sktpChunkScrPos* is needed to set up the color_pointer*
    ;and afterwards it is unused
    ;the backup means that we always know how many bytes a paintbrush
    ;chunk is wide
    lda sktpChunkLengthL
    sta sktpChunkScrPosL
    lda sktpChunkLengthH
    sta sktpChunkScrPosH

    ;TODO: only do this backup if repeat count > 0
    ;in case there is a repeat count > 0 we need to be able
    ;to find the color content again, therefore we need to
    ;backup the color_pointer stuff. As we don't need the
    ;data_pointer in the paintbrush chunk we can use it
    ;for this purpose
    lda color_pointer
    sta data_pointer
    lda color_pointer2
    sta data_pointer2

    ldy #0 ; y is used to iterate over the content bytes
    jmp handlePaintbrushChunkLoop

;============================================
parseSKTPChunkPart2:
;============================================
    cmp #4
    beq handleColorCharsetChunk
    cmp #5
    beq handleVerticalRepeatScreencode
    cmp #6
    beq handlePaintbrushChunk
    ;7 = tft_image_url
    ;8 = tft_image_inline
    lda #"$"
    jsr CHROUT
    rts

;============================================

handlePaintbrushChunkLoop:
    jsr read_byte
    sta (color_pointer),y
    tax ; backup color value in x
    ;backup color_pointer
    lda color_pointer2
    sta data_pointer2
    lda color_pointer
    sta data_pointer
pbCheckForRepeat:
    lda sktpChunkRptCount
    cmp #0
    ;is repeat count > zero? than care about that
    beq pbEndOfRepeat
    dec sktpChunkRptCount
    lda color_pointer
    clc
    adc sktpChunkScrPosL ; this is the length of the paintbrush chunk (ignoring the high byte)
    ;FIXME: care for high byte!!!!
    sta color_pointer
    bcc bpnovfl2
    inc color_pointer2
bpnovfl2:
    clc
    adc sktpChunkGap
    sta color_pointer
    bcc bpnovfl3
    inc color_pointer2
bpnovfl3:
    txa
    sta (color_pointer),y
    tax
    jmp pbCheckForRepeat

pbEndOfRepeat:
    ;restore color pointer from backups
    lda data_pointer
    sta color_pointer
    lda data_pointer2
    sta color_pointer2
    lda sktpChunkType ; restore repeat value from backup
    sta sktpChunkRptCount

    iny

    ;is highbyte > zero? than care about that first
    lda sktpChunkLengthH
    cmp #00
    beq pbProcessOnlyLowByte
    ;from now on we have the hassle to care for the high byte
    ;this means printing out 255 bytes of color values
    cpy #$ff
    bne handlePaintbrushChunkLoop
    dec sktpChunkLengthH
    lda color_pointer
    clc
    adc #255
    sta color_pointer
    bcc noOverflowPB
    inc color_pointer2
noOverflowPB:
    inc sktpChunkLengthL; workaround
    ldy #00
    jmp handlePaintbrushChunkLoop

pbProcessOnlyLowByte:
    cpy sktpChunkLengthL
    bne handlePaintbrushChunkLoop
    jmp endOfChunkReached

;============================================
handleVerticalRepeatScreencode: ; chunk type #5
;============================================
    jsr prepareStuff
    ;length 6 + chars
    lda #5
    clc
    adc sktpChunkLengthL
    sta sktpNettoChunkLengthL

    jsr read_byte ; repeat count
    sta sktpChunkRptCount
    ldx #00 ; x iterates the bytes read from http
VrscProcessNextByte:
    ldy #00 ; y iterates the times one byte was printed out
    stx mpCount
    jsr read_byte ; one char of possibly more
    jmp handleVerticalRepeatScreencodeLoop
VrscPostProcess:
    ldx mpCount
    inx
    stx mpCount
    cpx sktpChunkLengthL
    beq VRSCEnd
    jsr screenAndColorRAMAddress ;reset the addresses to default
    lda data_pointer
    adc mpCount
    sta data_pointer
    bcc noOverflowVRD2
    inc data_pointer2
noOverflowVRD2:
    lda color_pointer
    adc mpCount
    sta color_pointer
    bcc noOverflowVRD3
    inc color_pointer2
noOverflowVRD3:
    jmp VrscProcessNextByte

VRSCEnd:
    ldx #00
    stx mpCount; reset it so that it doesn't go wild for meta refreshes
    jmp endOfChunkReached

handleVerticalRepeatScreencodeLoop:
    ; in accu we have that byte to display
    sta (data_pointer),y
    tax
    lda sktpChunkColor
    sta (color_pointer),y
    iny
    cpy sktpChunkRptCount
    beq VrscPostProcess
    lda data_pointer
    clc
    adc #$27 ; add 39, not 40, as y also increases
    sta data_pointer
    bcc noOverflowVRD
    inc data_pointer2
noOverflowVRD:
    lda color_pointer
    clc
    adc #$27 ; add 39, not 40, as y also increases
    sta color_pointer
    bcc noOverflowVRC
    inc color_pointer2
noOverflowVRC:
    txa
    jmp handleVerticalRepeatScreencodeLoop


;============================================
;helper for colorCharset chunk
;============================================
readByteAndDecrease:
;============================================
    jsr read_byte
    tay
    dey
    tya
    rts

;============================================
endOfChunkReached:
;============================================
    ;lengthdebug

    ;lda sktpChunkType
    ;jsr PrintLowNibble
    ;lda #":"
    ;jsr CHROUT
    ;lda #"("
    ;jsr CHROUT
    ;lda sktpNettoChunkLengthH
    ;beq nettoLowByte

    ;jsr printHex
;~ nettoLowByte:
;    lda sktpNettoChunkLengthL
;    jsr printHex
;    lda #")"
;    jsr CHROUT

    ;subtract length of complete chunk from screen
    lda sktpNettoChunkLengthH
    beq .gohere ;if high byte is zero don't change high byte of screenlength
    lda sktpScreenLengthH
    clc
    sbc sktpNettoChunkLengthH
    sta sktpScreenLengthH
    inc sktpScreenLengthH ; workaround - test this line with petscii slideshow and arena/foyer
    ;~ clc
;    lda #"*"
;    jsr CHROUT

.gohere:
    lda sktpScreenLengthL
    clc
    sbc sktpNettoChunkLengthL
    bcs .notundernull
    tax
;    lda #"-"
;    jsr CHROUT
    txa
    dec sktpScreenLengthH
.notundernull:
    sta sktpScreenLengthL

    ;lengthdebug
    ;jsr debugOutputScreenLength


;waitkeypress2:
;    jsr GETIN
;    beq waitkeypress2
;    lda #"="
;    jsr CHROUT

    jsr parseChunk ;parse the next chunk

end:
    cli
    lda #$00

;============================================
waitkey:
;============================================
    lda mpCount
    cmp #00
    beq noMetaRefreshActive

    ;lda mpCount
    ;jsr PrintHiNibble
    ;lda mpCount
    ;jsr PrintLowNibble
    ;lda #"/"
    ;jsr CHROUT

    iny
    cpy #$ff
    bne noMetaRefreshActive ; count to 255 first
    lda mpCount
    dec mpCount
    ldy #00
    lda mpCount
    cmp #00
    beq triggerMetaRefresh
    jmp noMetaRefreshActive ; it is active but we
    ;want to detect human keypresses in between too

;delayDecreasempCount:
;    jmp noMetaRefreshActive

triggerMetaRefresh:
    jsr GETIN
    bne leaveWaitLoop
    lda #00 ; 00 key to send as char for refresh
    jmp leaveWaitLoop
noMetaRefreshActive:
    jsr GETIN
    beq waitkey

leaveWaitLoop:
    tax
    jsr getHiNibbleHex
    sta sktp_key+1          ; Skip initial '!'
    ;jsr CHROUT
    txa
    jsr getLowNibbleHex
    sta sktp_key+2
    ;jsr CHROUT
    jmp sendSKTPRequest

;    lda #"*"
;    jsr CHROUT
    jmp program_end

;============================================
screenAndColorRAMAddress:
;============================================
    ;set zero page values
    ;memory low bytes
    lda sktpChunkScrPosL
    sta data_pointer
    sta color_pointer
    ;screen memory starts at SCREEN_RAM
    lda sktpChunkScrPosH
    clc
    adc #(SCREEN_RAM >> 8)
    sta data_pointer2
    ;color memory starts at COLOR_RAM
    lda sktpChunkScrPosH
    adc #(COLOR_RAM >> 8)
    sta color_pointer2
    rts

;============================================
prepareStuff: ; reads four bytes
;============================================
    jsr read_byte
    sta sktpChunkLengthL
    jsr read_byte
    sta sktpChunkScrPosL
    jsr read_byte
    tax
    and #%00000011
    sta sktpChunkScrPosH
    txa
    lsr
    lsr
    lsr
    lsr
    sta sktpChunkLengthH

;    jsr printHex
;    lda #'/'

    jsr read_byte
    sta sktpChunkColor ;in case of paintbrush chunk this contains the gap value
    jsr screenAndColorRAMAddress
    ;byte counter
;    lda sktp

;    jsr showDebugInfo

    ;lda sktpChunkType
    ;cmp #01
    ;bne continue
    ;lda sktpChunkColor
    ;cmp #01
    ;bne continue
    ;lda sktpChunkLengthL
    ;cmp #01
    ;bne continue
    ;sta sktpChunkScrPosL
    ;cmp #01
    ;bne continue
    ;sta sktpChunkScrPosH
    ;cmp #01
    ;bne continue
    ;sta sktpChunkColor
    ;cmp #01
    ;bne continue
    ;jmp waitkey

continue:
    rts

!if 0 {
;============================================
showDebugInfo:
;============================================

    lda #13
    jsr CHROUT
    lda #'c'
    jsr CHROUT
    lda #'t'
    jsr CHROUT
    lda sktpChunkType
    ora #%00110000
    jsr CHROUT
    lda #'/'
    jsr CHROUT
    lda #'c'
    jsr CHROUT
    lda #'l'
    jsr CHROUT

    lda sktpChunkLengthL
    jsr printHex
    lda #'/'
    jsr CHROUT
    lda #'h'
    jsr CHROUT

    lda sktpChunkScrPosH
    jsr printHex
    lda #'/'
    jsr CHROUT
    lda #'l'
    jsr CHROUT

    lda sktpChunkScrPosL
    jsr printHex
    lda #'/'
    jsr CHROUT

    lda #'c'
    jsr CHROUT
    lda #'o'
    jsr CHROUT
    lda sktpChunkColor
    jsr printHex
    lda #'/'
    jsr CHROUT

    rts
}

;--------------------------------

printHex:
    pha
    jsr getHiNibbleHex
    jsr CHROUT
    pla
    jsr getLowNibbleHex
    jsr CHROUT
    rts

getHiNibbleHex:
    and #%11110000
    lsr
    lsr
    lsr
    lsr
    ora #%00110000
    cmp #58
    bcc .hiNibbleDone
    adc #06
.hiNibbleDone
    rts

getLowNibbleHex:
    and #%00001111
    ora #%00110000
    cmp #58
    bcc .lowNibbleDone
    adc #06
.lowNibbleDone
    rts


;============================================
; WicXX interaction routines
;============================================

;--------------------------
detect_wic:
;--------------------------
    +wic64_detect
    ;~ bcs device_not_present       FIXME
    bcs ++
    bne +

    +wic64_execute cmd_set_timeout
    +wic64_set_timeout TRANSFER_TIMEOUT         ; Set default WiC <-> Computer interaction timeout
    clc
    jmp ++

    ; Some error happened, report back through carry flag
+   sec

++  rts

;--------------------------

; This macro is used in the routines below to calculate the size of the commands to be sent to the WiC
; Assumption is that all sizes are < 256
!macro calc_payload_size .cmd {
    ; We need to calculate the payload size
    ldy #$00
-   iny
    lda .cmd+4,y            ; Skip header
    bne -                   ; Keep counting until the zero byte
    sty .cmd+2              ; Save it inside the command
}

;--------------------------
requestDownloadURL:
;--------------------------
    ; We don't need to call +calc_payload_size dlurl_start, URL size was already updated before jumping here
    +wic64_execute cmd_set_remote_timeout
    +wic64_load_and_run dlurl_start, REMOTE_TIMEOUT
    rts                     ; I strongly doubt we'll ever get back here ;)

;--------------------------
request_sessionid:
;--------------------------
    ; Calculate param size
    +calc_payload_size sess_command

    ; Now run the command and get the reply
    +wic64_execute sess_command, response
    bcs ++
    bne +

    ; Copy session ID to its place - we only expect 26 bytes (<255, so hi byte is irrelevant)
    ldx wic64_response_size
    ldy #0
-   lda response,y
    sta cmd_default_url_sess,y
    iny
    dex
    bne -

    clc
    jmp ++

    ; Some error happened, report back through carry flag
+   sec

++  rts

;--------------------------
sendURLPrefixToWic:
;--------------------------
    +calc_payload_size cmd_default_server

    +wic64_execute cmd_default_server, response
    bcs ++
    bne +

    clc
    jmp ++

+   sec

++  rts

;--------------------------
sendSKTPCommand:
;--------------------------
    +calc_payload_size sktp_command
    +wic64_execute cmd_set_remote_timeout
    +wic64_execute sktp_command, response
    bcs ++
    beq +
    sec
    jmp ++

    ; Command successful, init the response byte counter
+   lda #0
    sta next_resp_byte
    sta next_resp_byte+1
    clc

++  rts

;--------------------------
read_byte:
;--------------------------
    ; We need to fetch the next_resp_byte'th byte of the response
    clc
    lda #<response          ; Low byte first
    adc next_resp_byte
    sta scratch_zptr
    lda #>response
    adc next_resp_byte+1
    sta scratch_zptr+1
    ldx #0
    lda (scratch_zptr,x)

    inc next_resp_byte
    bne +
    inc next_resp_byte+1

+   rts


;============================================
; UTILITY ROUTINES
;============================================

;--------------------------------------------
ascii2screencode:
;--------------------------------------------
    cmp #160   ;uppercase on mixed case
    bcs conv
    cmp #96    ;uppercase on mixed case
    bcs uppconv
    cmp #64    ;lowercase on mixed case
    bcs conv
    jmp noconv
conv:
    sec
    sbc #$20
uppconv:
    sec
    sbc #$20
noconv:
    clc
    rts


;============================================
; DATA AREA
;============================================

; This command sets the server-side transfer timeout to use for the next request, i.e. it sets the number of seconds the
; WiC64 will wait for the C64 to continue a transfer before assuming that the transfer has timed out.
; The server-side timeout value will be reset to the default value of one second after the request following this
; request has been served, regardless of whether it was served successfully. This means that the server-side timeout
; needs to be set before each request that requires a custom setting.
; This is only required in case you are sending a request payload in discrete chunks and need more time on the C64 side
; to prepare the next chunk of data, for example when reading the data from disk or generating it programatically.
cmd_set_timeout:        !byte "R", WIC64_SET_TRANSFER_TIMEOUT, $01, $00, TRANSFER_TIMEOUT	; <seconds>

; This command sets the server-side remote request timeout to use for the next request, i.e. it sets the number of
; seconds the WiC64 will wait for a remote server to serve a request before assuming that the request has timed out.
; The server-side request timeout value will be reset to the default value of five seconds after the request following
; this request has been served, regardless of whether it was served successfully. This means that the server-side
; timeout needs to be set before each request that requires a custom setting.
; This is required if the remote server needs more time to serve the request, for example when generating the response
; dynamically. Note that you will most likely also need to increase the client side timeout in this case, so that not
; only the WiC64 is more patient with the remote server, but the C64 is more patient when waiting for the response from
; the WiC64 as well.    
cmd_set_remote_timeout: !byte "R", WIC64_SET_REMOTE_TIMEOUT, $01, $00, REMOTE_TIMEOUT	    ; <seconds>

sktp_command:           !byte "R", WIC64_HTTP_GET, $00, $00         ; '!' means "url set with WIC64_SET_SERVER"
sktp_key:               !text "!", "&r", 0

cmd_default_server:     !byte "R", WIC64_SET_SERVER, $00, $00       ; <string-size-l>, <string-size-h>, <string>
cmd_default_url:        !text sktp_server

                        !text "/sktp.php?s="
cmd_default_url_sess:   !text "12345678901234567890123456"          ; This will be updated by request_sessionid
cmd_default_url_parm:   !text "&k=", 0

sess_command:           !byte "R", WIC64_HTTP_GET, $00, $00         ; <url-size-l>, <url-size-h>, <url>...
sess_url:               !text sktp_server
!if PLUS4 {
                        !text "/sktp.php?session=new&type=264&username=wic64test&f=wic&v="
} else {
                        !text "/sktp.php?session=new&type=64&username=wic64test&f=wic&v="
}
sess_version:           !text client_version
sess_end:               !byte 0

; Max response size is capped at the size of this buffer, of course
response:               !fill 2048

sktpChunk:
sktpChunkType:          !text $00
sktpChunkLengthL:       !text $00
sktpChunkLengthH:       !text $00
sktpChunkScrPosL:       !text $00
sktpChunkScrPosH:       !text $00
sktpChunkGap:
sktpChunkColor:         !text $00
sktpChunkRptCount:      !text $00

sktpScreenType:         !text $00
sktpScreenLengthH:      !text $00
sktpScreenLengthL:      !text $00
sktpNettoChunkLengthL:  !text $00
sktpNettoChunkLengthH:  !text $00

						;     1-------9-1-------9-1-------9-1-------9-
errormsg_IllegalScreen: !pet "Illegal Screen Type", 0
welcomeMsg:             !pet "         SKTP client for ", wic_name
                        !pet $0d,$0d,$0d
                        !pet "              Version ", client_version

                        !pet $0d,"          Built on ", build_date

                        !pet $0d,$0d,"   2023-2024 by emulaThor & SukkoPera", $0d, $0d
                        !pet "     github.com/hpingel/sktp-client", $0d, $0d, $0d, $0d
                        !pet " Server: "
                        !pet sktp_server

                        !pet $0d, $0d, $0d, 0
requestSessIdMsg:       !pet "         Requesting Session ID: ?",$0d
                        !pet $0d, $0d, 0

pressAnyKeyMsg          !pet "             Press any key", 0
wicNotDetectedMsg:      !pet "       No ", wic_name
                        !pet " card detected :(", 0

nodoloMSG:              !pet "File launching...", $0d
                        !pet "Please wait!", $0d, $0d, 0

dlurl_start:            !byte "R", WIC64_HTTP_GET, $00, $00         ; <url-size-l>, <url-size-h>, <url>...
dlurl_netto_start:      !text "h", 0
                        !fill 254, 0

;---------------------------------

program_end:
    rts
