
; SKTP (Sidekick64 transfer protocol) client 
; for Commodore 64 with WiC64
; Copyright (C) 2023  Henning Pingel
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
; Assembler used: C64 Studio by Georg Rottensteiner
; https://www.georg-rottensteiner.de/de/c64.html

!to "sktp-v0.18.prg",cbm

*=$0801
  ;SYS 2064
    !byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

data_pointer   = $a7 ; $a7/$a8 adress for data
data_pointer2  = $a8 ; $a7/$a8 adress for data
color_pointer  = $a9 ; $a9/$aa adress for data    
color_pointer2 = $aa ; $a9/$aa adress for data    
mpCount        = $ab; multi purpose counter in zeropage
;we are using mpCount for screenMetaRefresh and also 
;for verticalrepeatscreencode chunk as the refresh chunk
;is always near the end of a screen and all vrsc chunks
;are already processed.

jmp start

welcomeScreen:
    jsr $e544 ;clear screen
    lda #15
    jsr setBothColors
    lda #5 ; white font color
    jsr $ffd2
    ldy #0
loopWelcomeMsg:
    lda welcomeMsg,y
    cmp #0
    beq endOfWelcomeMsg
    jsr $ffd2
    iny
    jmp loopWelcomeMsg
endOfWelcomeMsg:    
    jsr $ffd2
    rts

debugOutputScreenLength:
    ;prints out remaining screen byte length
    ;lda sktpScreenLengthH
    ;jsr PrintHiNibble
    lda sktpScreenLengthH
    jsr PrintLowNibble
    lda sktpScreenLengthL
    jsr PrintHiNibble
    lda sktpScreenLengthL
    jsr PrintLowNibble
    lda #","
    jsr $ffd2
    rts

setBothColors:
    sta $d020
    sta $d021
    rts

printDLURLStuff:

    ldy #0
loopNODOLOMsg:
    lda nodoloMSG,y
    cmp #0
    beq endOfNODOLOMsg
    jsr $ffd2
    iny
    jmp loopNODOLOMsg
endOfNODOLOMsg:    
    jsr $ffd2

printDLURLChar:
    jsr read_byte
;    jsr printstrlp
    jsr $ffd2
    dec sktpChunkType
    bne printDLURLChar

    lda #$0d
    jsr $ffd2
    lda #$0d
    jsr $ffd2

printDLFilenameChar:
    jsr read_byte
;    jsr $ffd2
    dec sktpChunkLengthL
    bne printDLFilenameChar
    
waitkeypressD:
    jsr $ffe4
    beq waitkeypressD
    lda #$87 ;F5 key for back
    jmp leaveWaitLoop; send keypress

start:
    ;switch to lowercase
    lda #14
    jsr $ffd2

    jsr welcomeScreen
   
    
    cld ; no decimal-flag wanted

renewSessionID:
    ;update indicator with red color
    lda #2
    sta $da50

    ;get and store sktp session id
    jsr request_sessionid

    ;update indicator color to yellow
    lda #07
    sta $da50
    
    jsr receive_sessionid
    
    ;update indicator (light green tick)
    lda #122
    sta $0650
    lda #13
    sta $da50
    
waitkeypressW:
    jsr $ffe4
    beq waitkeypressW

sendSKTPRequest:
    
    ; do normal sktp request
    jsr send_string

;  jsr end
    jsr getresponse
    rts



downloadURL:
    jsr $e544     ; Clr screen
    lda #4
    jsr setBothColors
    ;switch to lowercase
    lda #14
    jsr $ffd2

    jsr read_byte ; url length
    sta sktpChunkType
    jsr read_byte ; filename length
    sta sktpChunkLengthL
    jsr read_byte ; save flag 
    jmp printDLURLStuff

        
getresponse:
    jsr setWicToSendDataToC64

    ;lengthdebug
    
    ;jsr $e544     ; Clr screen
    ;lda #13
    ;jsr $ffd2
    ;lda #13
    ;jsr $ffd2
    ;lda #13
    ;jsr $ffd2
    ;lda #13
    ;jsr $ffd2
    ;lda #13;
    ;jsr $ffd2

    ;end of lengthdebug


    jsr read_byte   ;unused dummy byte 

    jsr read_byte
    sta sktpScreenLengthH

    jsr read_byte
    sta sktpScreenLengthL

    ;lengthdebug
    ;jsr debugOutputScreenLength

    ;subtract one from length as this is the screen type byte
    dec sktpScreenLengthL
    ;FIXME: check for negative and change Hbyte

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
    beq renewSessionID
    cmp #0
    bne parseChunk
    jsr $e544     ; Clr screen = disable on lengthdebug
    
    jmp parseChunk
    
illegalScreenType:
    jsr $e544     ; Clr screen
    lda #3
    jsr setBothColors
    ;switch to lowercase
    lda #14
    jsr $ffd2
    ;ERR: 
    lda #"e"
    jsr $ffd2
    lda #"r"
    jsr $ffd2
    jsr $ffd2
    lda #":"
    jsr $ffd2
    lda #" "
    jsr $ffd2
    lda sktpScreenType
    jsr PrintLowNibble
    lda #" "
    jsr $ffd2
    ldy #0
loopErrorMsg:
    lda errormsg_IllegalScreen,y
    cmp #0
    beq endOfErrorMsg
    jsr $ffd2
    iny
    jmp loopErrorMsg
endOfErrorMsg:    
    jsr $ffd2
    rts

    
;jmp debug2    
;    lda #"/"
;    jsr $ffd2
;    lda #"s"
;    jsr $ffd2
;    lda #"t"
;    jsr $ffd2
;    lda sktpScreenType
;    ora #%00110000
;    jsr $ffd2
;    lda #"/"
;    jsr $ffd2    
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
    cmp #00
    bne isnotempty
    lda sktpScreenLengthH
    cmp #00
    bne isnotempty
    ldy#00 ; for delay that works via y
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
    
    lda #"%"
    jsr $ffd2
    lda sktpChunkType
    jsr PrintHiNibble
    lda sktpChunkType
    jsr PrintLowNibble
;    ora #%00110000
;    jsr $ffd2
    lda #"%"
    jsr $ffd2

;    jsr read_byte
;    sta sktpChunkType;tmp debug
;    lda sktpChunkType
;    jsr PrintHiNibble
;    lda sktpChunkType
;    jsr PrintLowNibble
;;    ora #%00110000
;;    jsr $ffd2
;    lda #"%"
;    jsr $ffd2
;
;    jsr read_byte
;    sta sktpChunkType;tmp debug
;    lda sktpChunkType
;    jsr PrintHiNibble
;    lda sktpChunkType
;    jsr PrintLowNibble
;;    ora #%00110000
;;    jsr $ffd2
;    lda #"%"
;    jsr $ffd2

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
    sta sktpChunkRptCount
    
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
    jsr printstrlp
    tax
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
;    jsr $ffe4
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
    jsr printstrlp
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
    jsr prepNSCC
chunk2_loop:
    jsr read_byte
    jsr renderCharAndColor
    ;is highbyte > zero? than care about that first
    lda sktpChunkLengthH
    cmp #00
    beq processOnlyLowByte2
    cpy #$ff
    bne chunk2_loop ; go up and read next byte
    jsr decAndInc
    jmp chunk2_loop
    
processOnlyLowByte2:
    cpy sktpChunkLengthL
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
    cpy #$fe
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
    inc sktpNettoChunkLengthL
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
    sta $d020
    jsr readByteAndDecrease
    sta $d021
    jsr read_byte
    cmp #01
    beq switch2Lowercase
    lda #142 ;switch to uppercase
    jmp setCase
switch2Lowercase:
    lda #14 ;switch to lowercase
setCase:
    jsr $ffd2
    jmp endOfChunkReached

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
    jsr $ffd2
    rts

;============================================
handlePaintbrushChunk: ; chunk type 6
;============================================
    jsr prepareStuff
    ;always 6 chars + chars
    lda #05
    clc
    adc sktpChunkLengthL
    sta sktpNettoChunkLengthL
    lda #00
    clc
    adc sktpChunkLengthH
    sta sktpNettoChunkLengthH

    jsr read_byte ; repeat count
    sta sktpChunkRptCount
    ldy #00
    jmp handlePaintbrushChunkLoop

handlePaintbrushChunkLoop:
    jsr read_byte
    sta (color_pointer),y
;    sta $d800+120,y
    iny
    
    ;is highbyte > zero? than care about that first
    lda sktpChunkLengthH
    cmp #00
    beq pbProcessOnlyLowByte
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
    ;jsr $ffd2
    ;lda #"("
    ;jsr $ffd2
    ;lda sktpNettoChunkLengthH
    ;beq nettoLowByte
    
    ;;jsr PrintHiNibble ; this is not interesting, it should always be 0
    ;lda sktpNettoChunkLengthH
    ;LowNibble
nettoLowByte:
;    lda sktpNettoChunkLengthL
;    jsr PrintHiNibble
;    lda sktpNettoChunkLengthL
;    jsr PrintLowNibble    
;    lda #")"
;    jsr $ffd2

    ;subtract length of complete chunk from screen 
    lda sktpNettoChunkLengthH
    cmp #00
    beq gohere ;if high byte is zero don't change high byte off screenlength
    clc
    sbc sktpNettoChunkLengthH
    sta sktpScreenLengthH
    inc sktpScreenLengthH ; workaround
    clc
;    lda #"*"
;    jsr $ffd2

gohere:
    lda sktpScreenLengthL
    clc
    sbc sktpNettoChunkLengthL
    bcs notundernull
    tax
;    lda #"-"
;    jsr $ffd2
    txa
    dec sktpScreenLengthH
notundernull:
    sta sktpScreenLengthL

    ;lengthdebug
    ;jsr debugOutputScreenLength    
    

;waitkeypress2:
;    jsr $ffe4
;    beq waitkeypress2
;    lda #"="
;    jsr $ffd2

    jsr parseChunk ;parse the next chunk
    
end: 
    lda #$ff       ; Datenrichtung Port B Ausgang
    sta $dd03

    lda $dd00
    ora #$04       ; PA2 auf HIGH = ESP im Empfangsmodus
    sta $dd00
    
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
    ;jsr $ffd2

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
    jsr $ffe4
    bne leaveWaitLoop
    lda #00 ; 00 key to send as char for refresh
    jmp leaveWaitLoop
noMetaRefreshActive:
    jsr $ffe4
    beq waitkey

leaveWaitLoop:
    tax
    jsr getHiNibbleHex
    sta sktp_key
    ;jsr $ffd2
    txa
    jsr getLowNibbleHex
    sta sktp_key+1
    ;jsr $ffd2   
    jmp sendSKTPRequest

;    lda #"*"
;    jsr $ffd2   
    jmp end2

;============================================
screenAndColorRAMAddress:
;============================================
    ;set zero page values
    ;memory low bytes
    lda sktpChunkScrPosL
    sta data_pointer
    sta color_pointer
    ;screen memory starts at $0400
    lda sktpChunkScrPosH
    clc
    adc #4
    sta data_pointer2
    ;color memory starts at $d800
    lda sktpChunkScrPosH
    adc startColorRAM
    sta color_pointer2
    rts

;============================================
prepareStuff:
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

;    jsr PrintHiNibble
;    lda sktpChunkLengthH
;    jsr PrintLowNibble
;    lda #"/"
    
    jsr read_byte
    sta sktpChunkColor
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


;============================================
showDebugInfo:
;============================================

    lda #13
    jsr $ffd2
    lda #"c"
    jsr $ffd2
    lda #"t"
    jsr $ffd2
    lda sktpChunkType
    ora #%00110000
    jsr $ffd2
    lda #"/"
    jsr $ffd2    
    lda #"c"
    jsr $ffd2
    lda #"l"
    jsr $ffd2
    
    lda sktpChunkLengthL
    jsr PrintHiNibble
    lda sktpChunkLengthL
    jsr PrintLowNibble
    lda #"/"
    jsr $ffd2
    lda #"h"
    jsr $ffd2

    lda sktpChunkScrPosH
    jsr PrintHiNibble
    lda sktpChunkScrPosH
    jsr PrintLowNibble
    lda #"/"
    jsr $ffd2
    lda #"l"
    jsr $ffd2

    lda sktpChunkScrPosL
    jsr PrintHiNibble
    lda sktpChunkScrPosL
    jsr PrintLowNibble
    lda #"/"
    jsr $ffd2

    lda #"c"
    jsr $ffd2
    lda #"o"
    jsr $ffd2
    lda sktpChunkColor
    jsr PrintLowNibble
    lda #"/"
    jsr $ffd2
    
    rts


;--------------------------------

PrintHiNibble:
    jsr getHiNibbleHex
     jsr $ffd2
    rts

getHiNibbleHex:
    and #%11110000
    lsr
    lsr
    lsr
    lsr
    ora #%00110000
    cmp #58
    bcc hiNibbleDone
    adc #06
:hiNibbleDone
    rts

PrintLowNibble:
    jsr getLowNibbleHex
    jsr $ffd2
    rts

getLowNibbleHex:
    and #%00001111
    ora #%00110000
    cmp #58
    bcc lowNibbleDone
    adc #06
:lowNibbleDone
    rts

receive_sessionid:
    jsr setWicToSendDataToC64

    jsr read_byte   ;; Dummy Byte - 

    jsr read_byte
    tay

    jsr read_byte
    tax
    ;we only expect 26 bytes (<255, so y is irrelevant) 
    ldy #00
recsess_goread:
    jsr read_byte
;    jsr $ffd2
    sta sktp_url_sess,y
    iny 
    dex
    cpx #00
    bne recsess_goread
    rts

setWicToExpectDataFromC64:
    ;we want to send a command to the wic
    lda $dd02
    ora #$04
    sta $dd02     ; Datenrichtung Port A PA2 auf Ausgang
    lda #$ff      ; Datenrichtung Port B Ausgang
    sta $dd03
    lda $dd00
    ora #$04      ; PA2 auf HIGH = ESP im Empfangsmodus
    sta $dd00
    rts

setWicToSendDataToC64:
    ;we want to receive http data from the wic
    lda #$00       ; Datenrichtung Port B Eingang
    sta $dd03
    lda $dd00
    and #251      ; PA2 auf LOW = ESP im Sendemodus
    sta $dd00 
    rts
    
    
;--------------------------

request_sessionid:
    jsr setWicToExpectDataFromC64

    ;send command string to wic
    ldy #$04
sess_countstring:  
    iny
    lda sess_command,y
    
    cmp #$00
    bne sess_countstring
    sty sess_command+1       ; String länge ermitteln und in das Kommand schreiben
    ldy #$00
sess_string_next:
    iny
    lda sess_command-1,y
    ;debug ausgabe
    ;jsr $ffd2
    jsr write_byte
    cpy sess_command+1
    bne sess_string_next
    ;lda #13
    ;jsr $ffd2
    rts

;--------------------------

send_string:
    jsr setWicToExpectDataFromC64
    ;send command string to wic
    ldy #$04
countstring:    
    iny
    lda command,y
    cmp #$00
    bne countstring
    sty command+1       ; String länge ermitteln und in das Command schreiben
    ldy #$00
string_next:
    iny
    lda command-1,y
    ;jsr $ffd2
    jsr write_byte
    cpy command+1
    bne string_next
    ;lda #13
    ;jsr $ffd2
    rts

;--------------------------
write_byte:

    sta $dd01        ; Bit 0..7: Userport Daten PB 0-7 schreiben

dowrite:
    lda $dd0d
    nop
    nop
    nop
    nop
    and #$10        ; Warten auf NMI FLAG2 = Byte wurde gelesen vom ESP
    beq dowrite
    rts
;--------------------------

read_byte:
   
doread:
    lda $dd0d
    nop
    nop
    nop
    nop
    and #$10        ; Warten auf NMI FLAG2 = Byte wurde gelesen vom ESP
    beq doread
    
    lda $dd01 
    sta $02
    rts

;--------------------------------------------
;ascii to screencode

printstrlp:
    cmp #160         ;uppercase on mixed case
    bcs conv
    cmp #96         ;uppercase on mixed case
    bcs uppconv
    cmp #64         ;lowercase on mixed case
    bcs conv
    cmp #32         ;' ' character
    beq noconv
    cmp #33         ;: character
    beq noconv
    cmp #58         ;: character
    beq noconv
    cmp #42         ;* character
    beq noconv
    cmp #48         ;numbers 0-9
    jmp noconv
conv:
    secv
    sbc #$20
uppconv:
    sec
    sbc #$20
noconv:
    clc
    rts
numconv:
    bcc noconv
    jmp conv

;---------------------------------


startColorRAM: !text $d8
 
command:         !text "W",$00,$00,$01
sktp_url:        !text "http://sktpdemo.cafeobskur.de/sktp.php?s="
sktp_url_sess:   !text "12345678901234567890123456"
sktp_url_parm:   !text "&k="
sktp_key:        !text "&r",0

sess_command:    !text "W",$00,$00,$01
sess_url:        !text "http://sktpdemo.cafeobskur.de/sktp.php?session=new&sktpv=3&type=64&username=wic64test&f=wic",0

sktpChunk:
sktpChunkType:    !text $00
sktpChunkLengthL: !text $00
sktpChunkLengthH: !text $00
sktpChunkScrPosL: !text $00
sktpChunkScrPosH: !text $00
sktpChunkColor:   !text $00
sktpChunkRptCount:!text $00

sktpScreenType:   !text $00
sktpScreenLengthH:!text $00
sktpScreenLengthL:!text $00
sktpNettoChunkLengthL  :!text $00
sktpNettoChunkLengthH  :!text $00

errormsg_IllegalScreen: !text "illegal screen type",0
welcomeMsg:             !text "         sktp CLIENT FOR wIc64",$0d,$0d,$0d
                        !text "              vERSION 0.18",$0d
                        !text "          bUILT ON 2023-08-27",$0d,$0d
                        !text "           2023 BY EMULAtHOR",$0d
                        !text $0d,$0d,$0d
                        !text " sERVER: http://sktpdemo.cafeobskur.de",$0d
                        !text $0d,$0d,$0d
                        !text "        rEQUESTING SESSION ID : ?",$0d
                        !text $0d,$0d
                        !text "             pRESS ANY KEY",0

nodoloMSG:              !text $0d,$0d, "tHE FILE LAUNCH FEATURE IS NOT YET",$0d,"IMPLEMENTED.",$0d
                        !text "pRESS ANY KEY TO RETURN TO THE PREVIOUS PAGE.",$0d,$0d,0

end2:
  rts