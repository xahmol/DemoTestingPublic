; VDC Demo routines testing

	; System addresses
	SCROLY					= $D011		; Vertical smooth scrolling and control register
	CLKRATE					= $D030		; Processor clock rate: 0 = 1 MHZ, 1 = 2Mhz
    VDC_ADDRESS_REGISTER    = $D600     ; VDC Adress register address
    VDC_DATA_REGISTER       = $D601     ; VDC Data register address
    BANKCONFIG              = $FF00     ; MMU bank configuration
    RESET                   = $FF3D     ; System reset
	SWAPPER					= $FF5F		; SWAPPER kernal call: switch between 40 and 80 columns
	DLCHR					= $FF62		; DLCHR kernal call for resetting VDC charsets
	CINT					= $FF81		; CINT kernal call for initialize screen editor
	BSOUT					= $FFD2		; BSOUT kernal call for printing a character
    GETIN                   = $FFE4     ; GETIN kernal call for reading keyboard buffer
	MODE					= $D7		; Active screen flag, check bit 7: if set->80 column
    IRQ_LB                  = $314      ; IRQ pointer low byte
    IRQ_HB                  = $315      ; IRQ pointer high byte

    ; VDC registers
    VDC_DISPLAYSTARTHIGH    = $0C       ; VDC Register Display start address high byte
    VDC_DISPLAYSTARTLOW     = $0D       ; VDC Register Display start address low byte 
    VDC_UPDATEADRESSHIGH    = $12       ; VDC Register Update Address High Byte
    VDC_UPDATEADRESSLOW     = $13       ; VDC Register Update Address Low Byte
    VDC_ATTRIBUTESTARTHIGH  = $14       ; VDC Register Attribute start address high byte
    VDC_ATTRIBUTESTARTLOW   = $15       ; VDC Register Attribute start address low byte
    VDC_CHARARCTERWIDTH     = $16       ; VDC Register Character width
    VDC_BLOCKCOPY           = $18    	; VDC Register for block copy or block write mode
    VDC_HOR_SMOOTHSCROLL    = $19       ; VDC Register for horizontal smooth scroll
    VDC_ADDRESSINCREMENT    = $1B       ; VDC Register for address increment per row of characters
    VDC_WORDCOUNT           = $1E       ; VDC Register for word count
    VDC_DATA                = $1F       ; VDC data register
    VDC_FGBGCOLOR           = $1A       ; VDC Foreground/Background color
    VDC_BCOPY_SOURCE_HB     = $20       ; VDC Register for block copy source address high byte
    VDC_BCOPY_SOURCE_LB     = $21       ; VDC Register for block copy source address low byte

    ; Zero page addresses
    ZP1                     = $FA       ; First zero page address used
    ZP2                     = $FB       ; Second zero page address used
    ZPSID1                  = $FD       ; First ZP address reserved for SID music
    ZPSID2                  = $FE       ; Second ZP address reserved for SID music

    ; Asset addresses
    SCREENHB                = $50       ; High byte of screen address
    SCREENLB                = $00       ; Low byte of screen address
    SWAPMEM_HB              = $60       ; High byte of swapp address

    ; SID addresses
    SIDINIT                 = $1000
    SIDPLAY                 = $1003

    ; Segment to store code in
    .segment    "CODE"

    ; Load address
    .word $3000

; ------------------------------------------------------------------------------------------
; Main routine
; ------------------------------------------------------------------------------------------

    ; Set memory config to full bank 0 memory with IO
    lda #$0E                            ; Load config for bank 0 with kernal
    sta BANKCONFIG
    
    ; Check if active screen is 40 or 80 column
	lda MODE							; Check display mode
	bmi vdcmode							; Branch if 80 column

    ; Show message in 40 column mode
	lda #<switchvdcmessage				; Low byte of text address
	sta ZP1								; Store low byte in zero page
	lda #>switchvdcmessage				; Low byte of text address
	sta ZP2								; Store low byte in zero page
	jsr textout							; Print message
	jsr SWAPPER							; Switch to 80 column mode

vdcmode:
    ; Set fast mode
    lda #$01                            ; Load 1 to enable fast mode
    sta CLKRATE                         ; Set mode

	; Set VDC background color
    ldx #VDC_FGBGCOLOR
    lda bgcolor
    jsr VDC_Write

    ; Smooth scroll screen in
    lda #SCREENHB                       ; Load high byte of screen address
    sta Smooth_src_txt_hb               ; Store as input variable
    clc                                 ; Clear carry
    adc #$08                            ; Add 8 to get at attribute high byte
    sta Smooth_src_att_hb               ; Store as input variable
    lda #SCREENLB                       ; Load low byte
    sta Smooth_src_txt_lb               ; Store as input variable
    sta Smooth_src_att_lb               ; Store as input variable
    lda #$01                            ; Load 0 for smooth scroll in
    sta Smooth_inorout                  ; Store variable
    jsr Smoothscroll_init               ; Start smooth scroll effect

    ; Scroll out left
    jsr waitkey                         ; Wait for key
    lda #$00                            ; Set fill flag as 0
    sta Scroll_fill                     ; Store to variable
    lda #$05                            ; Set number of lines to 5
    sta Scroll_lines                    ; Store to variable
    lda #$00                            ; Store 0 as startline
    sta Scroll_startline                ; Store to variable
    lda #$50                            ; Load 80 characters as counter
    sta Scroll_counter                  ; Store to variable
    jsr Scroll_init                     ; Start scroll effect 

    ; Scroll in left
    jsr waitkey                         ; Wait for key
    lda #$01                            ; Set fill flag as 1
    sta Scroll_fill                     ; Store to variable
    lda #$05                            ; Set number of lines to 5
    sta Scroll_lines                    ; Store to variable
    lda #$00                            ; Store 0 as startline
    sta Scroll_startline                ; Store to variable
    lda #SCREENLB                       ; Load source low byte
    sta Scroll_src_txt_lb               ; Store to variable
    sta Scroll_src_att_lb               ; Store to variable
    lda #SCREENHB                       ; Load source low byte
    sta Scroll_src_txt_hb               ; Store to variable
    sta Scroll_src_att_hb               ; Store to variable
    clc                                 ; Clear carry
    lda Scroll_src_att_hb               ; Load attribute low byte
    adc #$08                            ; Add 8 pages and carry for attribute memory
    sta Scroll_src_att_hb               ; Store again
    lda #$50                            ; Load 80 characters as counter
    sta Scroll_counter                  ; Store to variable
    jsr Scroll_init                     ; Start scroll effect 

    ; Fade out
    jsr waitkey                         ; Wait for key
    lda #$03                            ; Load 3 for color index max for fade out
    sta Fade_counter                    ; Set counter
    lda #$00                            ; Load 0 for start line
    sta Fade_startline                  ; Set parameter
    lda #$05                            ; Load 5 for 5 lines
    sta Fade_lines                      ; Store to variable
    lda #$02                            ; Load 2 for fade out
    sta Fade_inorout                    ; Set fade inorout flag for fade out
    jsr Fade_init                       ; Start fade effect

    ; Fade in
    jsr waitkey                         ; Wait for key
    lda #$00                            ; Load 0 for start coior of fade in
    sta Fade_counter                    ; Set counter
    lda #$00                            ; Load 0 for VDC high byte and low byte
    sta Fade_VDC_hb                     ; Store high byte
    sta Fade_VDC_lb                     ; Store low byte
    lda #$05                            ; Load 5 for 5 lines
    sta Fade_lines                      ; Store to variable
    lda #SCREENLB                       ; Load low byte of source
    sta Fade_source_lb                  ; Store to variable
    clc                                 ; Clear carry
    lda #SCREENHB                       ; Load high byte of source
    adc #$08                            ; Add 8 pages and carry for attribute memory
    sta Fade_source_hb                  ; Store to variable
    lda #$01                            ; Load 1 for fade in
    sta Fade_inorout                    ; Set fade inorout flag for fade in
    jsr Fade_init                       ; Start fade effect

    ; End and reset
    jsr waitkey                         ; Wait for a keypress
    jsr resetSID                        ; Mute SID

    ; Disable fast mode
    lda #$00                            ; Load 1 to enable fast mode
    sta CLKRATE                         ; Set mode
    jmp RESET                           ; Perform reset as SID is at BASIC location, so cannot return to BASIC
    rts

; Effects handler

; ------------------------------------------------------------------------------------------
Effects_do:
; ------------------------------------------------------------------------------------------

    ; Calculate effects
    jsr Scroll_left_calc                ; Calculate scroll effects
    jsr Fade_calc                       ; Calculate fade effects
    
    ; Wait for VBlank
    jsr VDC_Wait_vblank                 ; Wait for VBlank

    ; Play SID frame
    jsr SIDPLAY                         ; Play a SID frame

    ; Add delay to move past first lines already retrieved
    ldx #$0F                            ; Load delay value high byte
    ldy #$ff                            ; Load delay value low byte
effects_do_delayloop:
    dey                                 ; Decrease delay low byte counter
    bne effects_do_delayloop            ; Loop until zero
    dex                                 ; Decrease delay high byte counter
    bne effects_do_delayloop            ; Loop until zero

    ; Show effects
    jsr Fade_show                       ; Fade if enabled
    jsr Scroll_left_show                ; Scroll left if enabled
    jsr Smoothscroll_do                 ; Smooth scroll screen to left if enabled

    ; Wait for no VBlank
    jsr VDC_Wait_no_vblank              ; Wait for VBlank
    jsr Smoothscroll_updatescreenaddr   ; Smooth scroll update screen addresses

    rts

; SID routines

; ------------------------------------------------------------------------------------------
resetSID:
; ------------------------------------------------------------------------------------------

	; reset SID state
    ldx     #$18
    lda     #$00
rst1:    
    sta     $d400,x
    dex
    bpl     rst1

    lda     #$08
    sta     $d404
    sta     $d40b
    sta     $d412 
    ldx    #$03
rst2:       
    bit     $d011
    bpl     *-3
    bit     $d011
    bmi     *-3
    dex
    bpl     rst2

    lda     #$00
    sta     $d404
    sta     $d40b
    sta     $d412
    lda     #$00
    sta     $d418
	rts

; Scroll routines

; ------------------------------------------------------------------------------------------
Scroll_init:
; Function to initialize scroll
; ------------------------------------------------------------------------------------------

    ; Calculate endline
    lda Scroll_startline                ; Load startline
    clc                                 ; Clear carry
    adc Scroll_lines                    ; Add number of lines
    sta Scroll_endline                  ; Store end line
    
    ; Set length in bytes
    ldy Scroll_lines                    ; Load number of lines
    lda length_lb,y                     ; Get low byte from table
    sta Scroll_length_lb                ; Store in variable
    lda length_hb,y                     ; Get high byte from table
    sta Scroll_length_hb                ; Store in variable

    ; Set VDC address
    ldx Scroll_startline                ; Load start line
    lda text_lb,x                       ; Get low byte from table
    sta Scroll_VDC_lb                   ; Store in variable
    lda length_hb,x                     ; Get high byte from table
    sta Scroll_VDC_hb                   ; Store in variable

    ; Enable scroll
    lda #$01                            ; Load 1 to set enable flag
    sta Scroll_enable                   ; Set flag
    rts

; ------------------------------------------------------------------------------------------
Scroll_increase_address:
; Function to increase source addresses
; ------------------------------------------------------------------------------------------

    lda Scroll_src_txt_tmp_lb           ; Load low byte
    clc                                 ; Clear carry
    adc #$50                            ; Add 80 for next line
    sta Scroll_src_txt_tmp_lb           ; Store again
    lda Scroll_src_txt_tmp_hb           ; Load low byte
    adc #$00                            ; Add 0 with carry
    sta Scroll_src_txt_tmp_hb           ; Store again
    lda Scroll_src_att_tmp_lb           ; Load low byte
    clc                                 ; Clear carry
    adc #$50                            ; Add 80 for next line
    sta Scroll_src_att_tmp_lb           ; Store again
    lda Scroll_src_att_tmp_hb           ; Load low byte
    adc #$00                            ; Add 0 with carry
    sta Scroll_src_att_tmp_hb           ; Store again
    rts

; ------------------------------------------------------------------------------------------
Scroll_left_calc:
; Function to calculate scroll selected lines one position left
; Input:    Scroll_enable       =   1=enable scroll effect, 0=disable
;           Scroll_fill         =   1=fill from source (scroll in), 0 =clear (scroll out)
;           Scroll_VDC_lb       =   Low byte VDC address (upperleft corner of scroll area)
;           Scroll_VDC_hb       =   High byte VDC address (upperleft corner of scroll area)
;           Scroll_src_txt_lb   =   Low byte of source text address
;           Scroll_src_txt_hb   =   High byte of source text address
;           Scroll_src_att_lb   =   Low byte of source attribute address
;           Scroll_src_att_hb   =   High byte of source attribute address
;           Scroll_lines        =   Number of lines to scroll
;           Scroll_counter      =   Number of positions to scroll left
;
; Note: Scroll full lines only.
; Addresses should be at multiples of 80 for leftmost character of start line
; ------------------------------------------------------------------------------------------
   
    ; Check if scroll is enabled
    lda Scroll_enable                   ; Load scroll enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne scroll_left_calc_start          ; If zero then go to end of routine
    rts

scroll_left_calc_start:

    ; Copy VDC text area to scroll to swap memory area one character moved
    lda Scroll_VDC_lb                   ; Load low byte
    sta VDC_addrl                       ; Store as parameter
    lda Scroll_VDC_hb                   ; Load high byte
    sta VDC_addrh                       ; Store as parameter
    clc                                 ; Clear carry
    lda VDC_addrl                       ; Load low byte
    adc #$01                            ; Add one to go one character to the right
    sta VDC_addrl                       ; Store again
    lda VDC_addrh                       ; Load high byte
    adc #$00                            ; Add zero with carry
    sta VDC_addrh                       ; Store again
    lda #$00                            ; Load 0 as low byte for destination swap area
    sta VDC_destl                       ; Store as parameter
    lda #$10                            ; Load $10 as high byte for destination swap area
    sta VDC_desth                       ; Store as parameter
    lda Scroll_length_hb                ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Scroll_length_lb                ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_MemCopy                     ; Perform copy to swap area

    ; Copy VDC attribute area to scroll to swap memory area one character moved
    lda Scroll_VDC_lb                   ; Load low byte
    sta VDC_addrl                       ; Store as parameter
    lda Scroll_VDC_hb                   ; Load high byte
    sta VDC_addrh                       ; Store as parameter
    clc                                 ; Clear carry
    lda VDC_addrl                       ; Load low byte
    adc #$01                            ; Add one to go one character to the right
    sta VDC_addrl                       ; Store again
    lda VDC_addrh                       ; Load high byte
    adc #$08                            ; Add $08 with carry to get at attribute space
    sta VDC_addrh                       ; Store again
    lda #$00                            ; Load 0 as low byte for destination swap area
    sta VDC_destl                       ; Store as parameter
    lda #$18                            ; Load $18 as high byte for destination swap area
    sta VDC_desth                       ; Store as parameter
    lda Scroll_length_hb                ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Scroll_length_lb                ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_MemCopy                     ; Perform copy to swap area

    ; Set source addresses
    lda Scroll_src_txt_lb               ; Load text source low byte
    sta Scroll_src_txt_tmp_lb           ; Store as temporary
    lda Scroll_src_txt_hb               ; Load text source low byte
    sta Scroll_src_txt_tmp_hb           ; Store as temporary
    lda Scroll_src_att_lb               ; Load text source low byte
    sta Scroll_src_att_tmp_lb           ; Store as temporary
    lda Scroll_src_att_hb               ; Load text source low byte
    sta Scroll_src_att_tmp_hb           ; Store as temporary

    ldy Scroll_startline                ; Set Y counter at startline

scroll_left_fill_loop:
    ; Set VDC address
    lda lastchar_lb,Y                   ; Load low byte
    sta VDC_addrl                       ; Store as variable
    lda lastchar_hb,Y                   ; Load high byte
    sta VDC_addrh                       ; Store as variable

    ; Check for clear or fill
    lda Scroll_fill                     ; Load flag
    cmp #$01                            ; Compare with 1 to check if fill flag is set
    beq scroll_left_fill_do

    ; Fill with empty character
    lda #$20                            ; Load 20 for space
    sta VDC_value                       ; Store as variable
    jsr VDC_Poke                        ; Poke character
    lda lastchar_hb_att,Y               ; Load high byte
    sta VDC_addrh                       ; Store as variable
    lda #$00                            ; Load 0 for black
    sta VDC_value                       ; Store as variable
    jsr VDC_Poke                        ; Poke attribute
    jmp scroll_left_nextline            ; Jump to next line label

scroll_left_fill_do:
    ; Fill with source character and attribute
    lda Scroll_src_txt_tmp_lb           ; Load low byte
    sta scroll_left_automod_txt+1       ; Store at auto modify pointer
    lda Scroll_src_txt_tmp_hb           ; Load low byte
    sta scroll_left_automod_txt+2       ; Store at auto modify pointer
scroll_left_automod_txt:
    lda scroll_left_automod_txt+1       ; Load value from automodify address
    sta VDC_value                       ; Store as variable
    jsr VDC_Poke                        ; Poke character
    lda lastchar_hb_att,Y               ; Load high byte
    sta VDC_addrh                       ; Store as variable
    lda Scroll_src_att_tmp_lb           ; Load low byte
    sta scroll_left_automod_att+1       ; Store at auto modify pointer
    lda Scroll_src_att_tmp_hb           ; Load low byte
    sta scroll_left_automod_att+2       ; Store at auto modify pointer
scroll_left_automod_att:
    lda scroll_left_automod_att+1       ; Load value from automodify address
    sta VDC_value                       ; Store as variable
    jsr VDC_Poke                        ; Poke character

scroll_left_nextline:
    ; Increase addresses for one line
    jsr Scroll_increase_address         ; Increase addresses
    iny                                 ; Increase Y counter
    cpy Scroll_endline                  ; Compare to end line
    bne scroll_left_fill_loop           ; Loop until end line is reached
    rts

; ------------------------------------------------------------------------------------------
Scroll_left_show:
; Function to show calculated scroll
; ------------------------------------------------------------------------------------------

    ; Check if scroll is enabled
    lda Scroll_enable                   ; Load scroll enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne scroll_left_show_start          ; If zero then go to end of routine
    rts

scroll_left_show_start:
   
    ; Copy back moved text from swap to screen
    lda #$00                            ; Load 0 as low byte for source swap area
    sta VDC_addrl                       ; Store as parameter
    lda #$10                            ; Load $10 as high byte for source swap area
    sta VDC_addrh                       ; Store as parameter
    lda Scroll_VDC_lb                   ; Load low byte for VDC destination
    sta VDC_destl                       ; Store as parameter
    lda Scroll_VDC_hb                   ; Load high byte for VDC destination
    sta VDC_desth                       ; Store as parameter
    lda Scroll_length_hb                ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Scroll_length_lb                ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_MemCopy                     ; Perform copy from swap area

    ; Copy back moved attributes from swap to screen
    lda #$00                            ; Load 0 as low byte for source swap area
    sta VDC_addrl                       ; Store as parameter
    lda #$18                            ; Load $10 as high byte for source swap area
    sta VDC_addrh                       ; Store as parameter
    lda Scroll_VDC_lb                   ; Load low byte for VDC destination
    sta VDC_destl                       ; Store as parameter
    lda Scroll_VDC_hb                   ; Load high byte for VDC destination
    clc                                 ; Clear carry
    adc #$08                            ; Add $08 for attribute space
    sta VDC_desth                       ; Store as parameter
    lda Scroll_length_hb                ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Scroll_length_lb                ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_MemCopy                     ; Perform copy from swap area

    ; Increase source addresses
    lda Scroll_src_txt_lb               ; Load low byte
    clc                                 ; Clear carry
    adc #$01                            ; Add one
    sta Scroll_src_txt_lb               ; Store again
    lda Scroll_src_txt_hb               ; Load low byte
    adc #$00                            ; Add one
    sta Scroll_src_txt_hb               ; Store again
    lda Scroll_src_att_lb               ; Load low byte
    clc                                 ; Clear carry
    adc #$01                            ; Add one
    sta Scroll_src_att_lb               ; Store again
    lda Scroll_src_att_hb               ; Load low byte
    adc #$00                            ; Add one
    sta Scroll_src_att_hb               ; Store again

    ; Decrease counter
    dec Scroll_counter                  ; Decrease counter
    bne scroll_left_end                 ; If not yet zero go to end
    lda #$00                            ; Load 0 to clear scroll enable flag
    sta Scroll_enable                   ; Disable scroll

scroll_left_end:
    rts                                 ; Return

; Fade routines

; ------------------------------------------------------------------------------------------
Fade_init:
; Function to initialize Fade
; ------------------------------------------------------------------------------------------
   
    ; Set length in bytes
    ldy Fade_lines                      ; Load number of lines
    lda length_lb,y                     ; Get low byte from table
    sta Fade_length_lb                  ; Store in variable
    lda length_hb,y                     ; Get high byte from table
    sta Fade_length_hb                  ; Store in variable

    ; Set VDC address
    ldx Fade_startline                  ; Load start line
    lda text_lb,x                       ; Get low byte from table
    sta Fade_VDC_lb                     ; Store in variable
    lda att_hb,x                        ; Get high byte from table
    sta Fade_VDC_hb                     ; Store in variable

    ; Enable Fade
    lda #$01                            ; Load 1 to set enable flag
    sta Fade_enable                     ; Set flag
    rts

; ------------------------------------------------------------------------------------------
Fade_calc:
; Function to fade in (Fade_enable=1) or out (Fade_enable=2)
; Input:    Fade_counter = 0 for fade in, 3 for fade out
;           Fade_VDC_hb and Fade_VDC_lb = VDC start address (text)
;           Fade_lines = number of lines
; ------------------------------------------------------------------------------------------
   
    ; Check if fade is enabled
    lda Fade_enable                     ; Load Fade enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne fade_start                      ; If zero then go to end of routine
    rts

fade_start:
    ; Copy VDC attribute area to fade to swap memory area
    lda Fade_VDC_lb                     ; Load low byte
    sta VDC_addrl                       ; Store as parameter
    lda Fade_VDC_hb                     ; Load high byte
    sta VDC_addrh                       ; Store as parameter
    lda #$00                            ; Load 0 as low byte for destination swap area
    sta VDC_destl                       ; Store as parameter
    lda #SWAPMEM_HB                     ; Load high byte for destination swap area
    sta VDC_desth                       ; Store as parameter
    lda Fade_length_hb                  ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Fade_length_lb                  ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_CopyVDCToMem                ; Perform copy to swap area

    ; Set address pointers
    lda #$00                            ; Load 0 as low byte for destination swap area
    sta ZP1                             ; Store at ZP
    lda #SWAPMEM_HB                     ; Load high byte for destination swap area
    sta ZP2                             ; Store at ZP
    lda Fade_source_lb                  ; Load low byte
    sta fade_source_color+1             ; Auto modify pointer in code
    lda Fade_source_hb                  ; Load low byte
    sta fade_source_color+2             ; Auto modify pointer in code
    lda Fade_length_hb                  ; Load high byte of length
    sta Fade_pages_tmp                  ; Store to page counter

fade_loop_Y:
    ; Set charachter counter:
    lda Fade_pages_tmp                  ; Load high byte of length
    cmp #$00    						; Check if this is the last page
	bne fade_notyetlastpage			    ; Branch to 'not yet last page' if not equal
    ldy Fade_length_lb                  ; Set Y counter at legth low byte
    jmp fade_loop_X                     ; Jump to last page label
fade_notyetlastpage:
    ldy #$ff                            ; Set Y index at full page

fade_loop_X:
    ; Check for counter is 5 which is last stage of fade in
    ldx Fade_counter                    ; Load counter to X
    cpx #$04                            ; Compare to 4
    bne fade_next_color

    ; Change to source color
fade_source_color:
    lda fade_source_color+1,y           ; Load source from auto modify address
    jmp fade_store_color                ; Jump to store color

    ; Change to fade color
fade_next_color:
    ; Load present attribute and clear of color data
    lda (ZP1),y                         ; Read present attribute
    and #$F0                            ; Clear lower four bits to clear of color data
    ldx Fade_counter                    ; Load counter to X
    ora Fade_colors,X                   ; Load color with X index and set color bits

fade_store_color:
    ; Store color
    sta (ZP1),y                         ; Save back to swap

    ; Decrease Y counter
    dey                                 ; Decrease Y counter
    cpy #$ff                            ; Check if past 0
    bne fade_loop_X                     ; Loop until Y counter past zero

    ; Increase pointer high bytes
    inc ZP2                             ; Increase swap memory high byte
    inc fade_source_color+2             ; Increase source memory high byte
    dec Fade_pages_tmp                  ; Decrease length page counter
    lda Fade_pages_tmp                  ; Load page counter
    cmp #$ff                            ; Check if all pages are done
    bne fade_loop_Y                     ; Repeat loop until page counter is zero
    rts

; ------------------------------------------------------------------------------------------
Fade_show:
; Function to fade in (Fade_enable=1) or out (Fade_enable=2)
; Input:    Fade_counter = 0 for fade in, 3 for fade out
;           Fade_VDC_hb and Fade_VDC_lb = VDC start address (text)
;           Fade_lines = number of lines
; ------------------------------------------------------------------------------------------

    ; Check if fade is enabled
    lda Fade_enable                     ; Load Fade enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne fade_show_start                 ; If zero then go to end of routine
    rts

fade_show_start:
    ; Copy back to VDC memory
    lda #$00                            ; Load 0 as low byte for destination swap area
    sta VDC_addrl                       ; Store as parameter
    lda #SWAPMEM_HB                     ; Load high byte for destination swap area
    sta VDC_addrh                       ; Store as parameter
    lda Fade_VDC_lb                     ; Load low byte
    sta VDC_destl                       ; Store as parameter
    lda Fade_VDC_hb                     ; Load high byte
    sta VDC_desth                       ; Store as parameter
    lda Fade_length_hb                  ; Load high byte of length to copy
    sta VDC_tmp1                        ; Store as parameter
    lda Fade_length_lb                  ; Load high byte of length to copy
    sta VDC_tmp2                        ; Store as parameter
    jsr VDC_CopyMemToVDC                ; Perform copy to swap area

    ; Check if fade is set as fade in
    lda Fade_inorout                    ; Load Fade inorout flag
    cmp #$01                            ; Compare to one to check if set as in
    bne fade_out                        ; If zero then go to end of routine

    ; Increase color counter for fade in
    inc Fade_counter                    ; Increase color counter
    lda Fade_counter                    ; Load counter to check
    cmp #$05                            ; Compare to 5 to see if end is reached
    beq fade_disable                    ; Branch to fade disable if at maxium counter
    rts                                 ; Return

fade_out:
    ; Decrease color counter for fade out
    dec Fade_counter                    ; Decrease color counter
    lda Fade_counter                    ; Load counter to check
    cmp #$ff                            ; Compare to see if end is reached
    beq fade_disable                    ; Branch to fade disable if at maxium counter
    rts                                 ; Return

fade_disable:
    ; Disable fade
    lda #$00                            ; Load 0 to clear flag
    sta Fade_enable                     ; Store flag
    sta Fade_counter                    ; Reset counter
    rts

; Full screen smooth scroll routines

; ------------------------------------------------------------------------------------------
Smoothscroll_copy_out:
; Function to copy screen lines to prepare virtual screen for fade out
; ------------------------------------------------------------------------------------------

    ; Copy attributes

    ; Set number of lines
    ldy #$18                            ; Load 24 to start at last line

Smoothscroll_copyout_copyattr:
    
    ; Set destination address of line copy
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_att_hb,Y                 ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_txt_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Set the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY    				; Load $18 for register 24 (block copy mode) in X	
	lda #$80			        		; Set copy bit
	jsr VDC_Write						; Write VDC

	; Set source address of line copy
	ldx #VDC_BCOPY_SOURCE_HB			; Load $20 for register 32 (block copy source) in X	
	lda att_hb,Y			        	; Load high byte of source in A
	jsr VDC_Write						; Write VDC
	inx                                 ; Load $21 for register 33 (block copy source) in X	
	lda text_lb,Y		        		; Load low byte of source in A
	jsr VDC_Write						; Write VDC

    ; Set number of bytes to copy at one line
    ldx #VDC_WORDCOUNT    				; Load $1E for register 30 (word count) in X
    lda #$50                            ; Load 80 for 80 characters
    jsr VDC_Write						; Write VDC

    ; Set destination address for clear attributes
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_swp_att_hb,Y             ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_swp_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Store character to write in data register 31
	ldx #VDC_DATA       				; Load $1f for register 31 (VDC data) in X	
	lda #$00    			        	; Load 0 as attribute value
	jsr VDC_Write						; Write VDC

    ; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X	
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC

	; Store lenth in data register 30
	ldx #VDC_WORDCOUNT					; Load $1f for register 30 (word count) in X	
	lda #$50    			        	; Load 80 for one line
	jsr VDC_Write						; Write VDC

    ; Decrease counter and loop until last line is done
    dey                                 ; Decrease Y counter
    cpy #$ff                            ; Check if Y past zero
    bne Smoothscroll_copyout_copyattr   ; Loop until last line is done

    ; Copy text

    ; Set number of lines
    ldy #$18                            ; Load 24 to start at last line

Smoothscroll_copyout_copytxt:
    
    ; Set destination address of line copy
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_txt_hb,Y                 ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_txt_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Set the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY    				; Load $18 for register 24 (block copy mode) in X	
	lda #$80			        		; Set copy bit
	jsr VDC_Write						; Write VDC

	; Set source address of line copy
	ldx #VDC_BCOPY_SOURCE_HB			; Load $20 for register 32 (block copy source) in X	
	lda length_hb,Y			        	; Load high byte of source in A
	jsr VDC_Write						; Write VDC
	inx                                 ; Load $21 for register 33 (block copy source) in X	
	lda text_lb,Y		        		; Load low byte of source in A
	jsr VDC_Write						; Write VDC

    ; Set number of bytes to copy at one line
    ldx #VDC_WORDCOUNT    				; Load $1E for register 30 (word count) in X
    lda #$50                            ; Load 80 for 80 characters
    jsr VDC_Write						; Write VDC

    ; Set destination address for clear text
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_swp_txt_hb,Y             ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_swp_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Store character to write in data register 31
	ldx #VDC_DATA       				; Load $1f for register 31 (VDC data) in X	
	lda #$20    			        	; Load 32 as character for space
	jsr VDC_Write						; Write VDC

    ; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X	
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC

	; Store lenth in data register 30
	ldx #VDC_WORDCOUNT					; Load $1f for register 30 (word count) in X	
	lda #$50    			        	; Load 80 for one line
	jsr VDC_Write						; Write VDC

    ; Decrease counter and loop until last line is done
    dey                                 ; Decrease Y counter
    cpy #$00                            ; Check if Y is zero (last line does not need copying)
    bne Smoothscroll_copyout_copytxt    ; Loop until last line is done
    rts

; ------------------------------------------------------------------------------------------
Smoothscroll_copysourcein:
; Function to copy source screen to prepare virtual screen for fade in
; ------------------------------------------------------------------------------------------

    ; Set couunter for copying characters in the line
    lda #$50                            ; Set counter at 80 characters
    sta Smooth_charcounter              ; Store as counter

Smoothscroll_copyin_charloop:
    ; Set destination address for character source
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_swp_txt_hb,Y             ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_swp_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

smoothin_char:
    lda smoothin_char+1                 ; Load source character
    ldx #VDC_DATA                       ; Load $1f for register 31 (VDC data) in X
    jsr VDC_Write

    ; Set destination address for attribute source
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_swp_att_hb,Y             ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_swp_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

smoothin_att:
    lda smoothin_att+1                  ; Load source character
    ldx #VDC_DATA                       ; Load $1f for register 31 (VDC data) in X
    jsr VDC_Write

    ; Increase pointers and decrease counter until ine is done
    inc smoothin_char+1                 ; Increase low byte of char pointer
    bne smoothin_attpoint               ; Branch if not yet zero
    inc smoothin_char+2                 ; Increase high byte of char pointer if needed
smoothin_attpoint:
    inc smoothin_att+1                  ; Increase low byte of char pointer
    bne smoothin_deccounter             ; Branch if not yet zero
    inc smoothin_att+2                  ; Increase high byte of char pointer if needed
smoothin_deccounter:
    dec Smooth_charcounter              ; Decrease character counter
    bne Smoothscroll_copyin_charloop    ; Loop until zero
    rts

; ------------------------------------------------------------------------------------------
Smoothscroll_copy_in:
; Function to prepare screen lines to prepare virtual screen for fade in
; ------------------------------------------------------------------------------------------

    ; Set number of lines
    ldy #$18                            ; Load 24 to start at last line

    ; Set address pointer in zero-page for source data
	lda Smooth_src_txt_lb   	    	; Obtain low byte in A
	sta smoothin_char+1					; Store low byte in pointer for char
    lda Smooth_src_att_lb               ; Obtain low byte in A
    sta smoothin_att+1	                ; Store low byte in pointer for att
	lda Smooth_src_txt_hb				; Obtain high byte in A
	sta smoothin_char+2					; Store high byte in pointer for char
    lda Smooth_src_att_hb               ; Obtain high byte in A     
    sta smoothin_att+2					; Store high byte in pointer for att

Smoothscroll_copyin_lineloop:
    jsr Smoothscroll_copysourcein       ; Copy source data
    
    ; Set destination address for clear attributes
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_att_hb,Y                 ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_txt_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Store character to write in data register 31
	ldx #VDC_DATA       				; Load $1f for register 31 (VDC data) in X	
	lda #$00    			        	; Load 0 as attribute value
	jsr VDC_Write						; Write VDC

    ; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X	
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC

	; Store lenth in data register 30
	ldx #VDC_WORDCOUNT					; Load $1f for register 30 (word count) in X	
	lda #$50    			        	; Load 80 for one line
	jsr VDC_Write						; Write VDC

    ; Set destination address for clear text
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X
    lda double_txt_hb,Y                 ; Load high byte of destination from table
    jsr VDC_Write						; Write VDC
    inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda double_txt_lb,Y      			; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

    ; Store character to write in data register 31
	ldx #VDC_DATA       				; Load $1f for register 31 (VDC data) in X	
	lda #$20    			        	; Load 32 as character for space
	jsr VDC_Write						; Write VDC

    ; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X	
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC

	; Store lenth in data register 30
	ldx #VDC_WORDCOUNT					; Load $1f for register 30 (word count) in X	
	lda #$50    			        	; Load 80 for one line
	jsr VDC_Write						; Write VDC

    ; Decrease counter and loop until last line is done
    dey                                 ; Decrease Y counter
    cpy #$00                            ; Check if Y is zero (last line does not need copying)
    bne Smoothscroll_copyin_lineloop    ; Loop until last line is done
    rts

; ------------------------------------------------------------------------------------------
Smoothscroll_init:
; Function to initialize smooth scroll
; ------------------------------------------------------------------------------------------

    ; Load and compare flag to set fade in our out
    lda Smooth_inorout                  ; Load flag
    cmp #$00                            ; Compare to zero for fade out
    bne smoothscroll_copyin             ; Branch if fade in

    ; Scroll in
    jsr Smoothscroll_copy_out           ; Preoare for fade out
    jmp smoothscroll_prepscreen         ; Jump to prepare screen

smoothscroll_copyin:
    jsr Smoothscroll_copy_in            ; Preoare for fade in

smoothscroll_prepscreen:
    ; Set virtual screen width to 160 instead of 80
    ldx #VDC_ADDRESSINCREMENT           ; Load 27 for VDC register 27
    lda #$50                            ; Load 80 for setting an address increment of 80
    jsr VDC_Write                       ; Write to VDC

    ; Set attribute address to $1000
    ldx #VDC_ATTRIBUTESTARTHIGH         ; Load register number 20 for high byte of attribute address
    lda #$10                            ; Load 10 for high byte of $1000
    jsr VDC_Write                       ; Write to VDC
    inx                                 ; Load register number 21 for low byte of attribute address
    lda #$00                            ; Load 0 for low byte of $1000
    jsr VDC_Write                       ; Write to VDC

    ; Set parameters for smooth scroll
    lda #$50                            ; Load 80 for 80 characters
    sta Smooth_charcounter              ; Store parameter
    lda #$00                            ; Load 0 for first character
    sta Smooth_charcounter_inv          ; Store parameter
    lda #$07                            ; Reset pixelcounter
    sta Smooth_pixelcounter             ; Store parameter
    lda #$01                            ; Load 1 to set enable flag
    sta Smooth_enable                   ; Store parameter
    rts

; ------------------------------------------------------------------------------------------
Smoothscroll_do:
; Function to update smooth scroll registers
; ------------------------------------------------------------------------------------------

    ; Check if smoot scroll is enabled
    lda Smooth_enable                   ; Load Fade enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne smoothscroll_start              ; If zero then go to end of routine
    rts

smoothscroll_start:

    ; Update number of pixels scrolled
    lda Smooth_pixelcounter             ; Load cpounter of pixels scrolled
    ora #$40                            ; Set attributes
    ldx #VDC_HOR_SMOOTHSCROLL           ; Load 25 for VDC horizontal smooth scroll register
    jsr VDC_Write                       ; Write to VDC

    ; Increase pixel counter
    dec Smooth_pixelcounter             ; Decrease pixel counter
    beq smoothscroll_endofcharacter     ; Branch if end of character is reached
    rts                                 ; Return

smoothscroll_endofcharacter:
    ; Increase character offset
    lda #$07                            ; Load 0 to reset pixel counter
    sta Smooth_pixelcounter             ; Store parameter
    inc Smooth_charcounter_inv          ; Increase character offset
    dec Smooth_charcounter              ; Check if final character reached
    beq smoothscroll_disable            ; Branch if final character reached
    rts

smoothscroll_disable:
    ; Disable scroll effect
    lda #$00                            ; Load 0 to set flag
    sta Smooth_enable                   ; Set flag to 0 to disable smooth scroll

    ; Set virtual screen width back to 80
    ldx #VDC_ADDRESSINCREMENT           ; Load 27 for VDC register 27
    lda #$00                            ; Load 0 for no increment
    jsr VDC_Write                       ; Write to VDC

    ; Set attribute address to $0800 again
    ldx #VDC_ATTRIBUTESTARTHIGH         ; Load register number 20 for high byte of attribute address
    lda #$08                            ; Load 10 for high byte of $1000
    jsr VDC_Write                       ; Write to VDC
    inx                                 ; Load register number 21 for low byte of attribute address
    lda #$00                            ; Load 0 for low byte of $1000
    jsr VDC_Write                       ; Write to VDC

    ; Check if scrool in or out
    lda Smooth_inorout                  ; Load in or out flag
    cmp #$00                            ; Compare to zero for scroll out
    bne smoothscroll_in                 ; If not equal, branch to scroll in

    ; Clear screen
    jsr VDC_Clearscreen                 ; Clear the screen
    rts

smoothscroll_in:
    ; Copy screen to normal screenwitdh
    lda #$00                            ; Load low byte of screen address
    sta VDC_addrl                       ; Store as input variable
    lda #SCREENHB                       ; Load high byte of screen address
    sta VDC_addrh                       ; Store as input variable
    lda #SCREENLB                       ; Load 0 as low and high byte of VDC address
    sta VDC_destl                       ; Store as input variable
    sta VDC_desth                       ; Store as input variable
    lda #$0F                            ; Set 16 pages for text plus attributes
    sta VDC_tmp1                        ; Store as input variable
    lda #$FF                            ; Load 255 for 256 characters
    sta VDC_tmp2                        ; Store as input variable
    jsr VDC_CopyMemToVDC                ; Copy to VDC routine
    rts

; ------------------------------------------------------------------------------------------
Smoothscroll_updatescreenaddr:
; Function to update screen addresses with offset
; ------------------------------------------------------------------------------------------

    ; Check if smoot scroll is enabled
    lda Smooth_enable                   ; Load Fade enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne smoothscroll_upd_start          ; If zero then go to end of routine
    rts

    ; Check if pixel counter is already at zero
    lda Smooth_pixelcounter             ; Load pixelcounter
    cmp #$00                            ; Compare to zero to check if set
    beq smoothscroll_upd_start          ; If not zero then go to end of routine
    rts

smoothscroll_upd_start:
    ; Set display addresses with offset
    lda #$00                            ; Load 0
    ldx #VDC_DISPLAYSTARTHIGH           ; Load VDC register for display start address high byte
    jsr VDC_Write                       ; Write to VDC
    inx                                 ; Load VDC register for display start address low byte
    lda Smooth_charcounter_inv          ; Load character offset     
    jsr VDC_Write                       ; Write to VDC
    lda #$10                            ; Load $10
    ldx #VDC_ATTRIBUTESTARTHIGH         ; Load VDC register for attribute start address high byte
    jsr VDC_Write                       ; Write to VDC
    inx                                 ; Load VDC register for attribute start address low byte
    lda Smooth_charcounter_inv          ; Load character offset 
    jsr VDC_Write                       ; Write to VDC
    rts

; VDC routines

; ------------------------------------------------------------------------------------------
VDC_ReadRegister:
; Function to read a VDC register
; Input:	VDC_regadd = register number
; Output:	VDC_regval = read value
; ------------------------------------------------------------------------------------------

	ldx VDC_regadd                     ; Load register address in X
	jsr VDC_Read						; Read VDC
	sta VDC_regval                     ; Load A to return variable
    rts

; ------------------------------------------------------------------------------------------
VDC_WriteRegister:
; Function to write a VDC register
; Input:	VDC_regadd = register numnber
;			VDC_regval = value to write
; ------------------------------------------------------------------------------------------

    ldx VDC_regadd                     ; Load register address in X
	lda VDC_regval				        ; Load register value in A
	jsr VDC_Write						; Write VDC
    rts

; ------------------------------------------------------------------------------------------
VDC_Poke:
; Function to store a value to a VDC address
; Input:	VDC_addrh = VDC address high byte
;			VDC_addrl = VDC address low byte
;			VDC_value = value to write
; ------------------------------------------------------------------------------------------

    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_addrh                       ; Load high byte of address in A
	jsr VDC_Write						; Write VDC
	inx		    						; Increase X for register 19 (VDC RAM address low)
	lda VDC_addrl      				    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    				    ; Load $1f for register 31 (VDC data) in X	
	lda VDC_value       				; Load value to write in A
	jsr VDC_Write						; Write VDC
    rts

; ------------------------------------------------------------------------------------------
VDC_Peek:
; Function to read a value from a VDC address
; Input:	VDC_addrh = VDC address high byte
;			VDC_addrl = VDC address low byte
; Output:	VDC_value = read value
; ------------------------------------------------------------------------------------------

    ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_addrh      				    ; Load high byte of address in A
	jsr VDC_Write						; Write VDC
	inx					    			; Increase X for register 19 (VDC RAM address low)
	lda VDC_addrl	    	    		; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    					; Load $1f for register 31 (VDC data) in X	
	jsr VDC_Read						; Read VDC
	sta VDC_value			        	; Load A to return variable
    rts

; ------------------------------------------------------------------------------------------
VDC_Read:
; Function to do a VDC read and wait for ready status
; Input:	X = register number
; Output:	A = read value
; ------------------------------------------------------------------------------------------

	stx VDC_ADDRESS_REGISTER            ; Store X in VDC address register
notyetreadyread:						; Start of wait loop to wait for VDC status ready
	bit VDC_ADDRESS_REGISTER            ; Check status bit 7 of VDC address register
	bpl notyetreadyread                 ; Continue loop if status is not ready
	lda VDC_DATA_REGISTER               ; Load data to A from VDC data register
	rts

; ------------------------------------------------------------------------------------------
VDC_Write:
; Function to do a VDC read and wait for ready status
; Input:	X = register number
; 			A = value to write
; ------------------------------------------------------------------------------------------

	stx VDC_ADDRESS_REGISTER            ; Store X in VDC address register
notyetreadywrite:						; Start of wait loop to wait for VDC status ready
	bit VDC_ADDRESS_REGISTER            ; Check status bit 7 of VDC address register
	bpl notyetreadywrite                ; Continue loop if status is not ready
	sta VDC_DATA_REGISTER               ; Store A to VDC data
	rts

; ------------------------------------------------------------------------------------------
VDC_MemCopy:
; Function to copy memory from one to another position within VDC memory
; Input:	VDC_addrh = high byte of source address
;			VDC_addrl = low byte of source address
;			VDC_desth = high byte of destination address
;			VDC_destl = low byte of destination address
;			VDC_tmp1 = high byte of length
;			VDC_tmp2 = low byte of length
;           Set length as number of bytes minus 1 (so zero based)
; ------------------------------------------------------------------------------------------

loopmemcpy:
	; Hi-byte of the destination address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_desth      				    ; Load high byte of dest in A
	jsr VDC_Write						; Write VDC

	; Lo-byte of the destination address to register 19
	inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda VDC_destl       				; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

	; Set the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY    				; Load $18 for register 24 (block copy mode) in X	
	lda #$80			        		; Set copy bit
	jsr VDC_Write						; Write VDC

	; Hi-byte of the source address to block copy source register 32
	ldx #VDC_BCOPY_SOURCE_HB			; Load $20 for register 32 (block copy source) in X	
	lda VDC_addrh			        	; Load high byte of source in A
	jsr VDC_Write						; Write VDC
	
	; Lo-byte of the source address to block copy source register 33
	inx                                 ; Load $21 for register 33 (block copy source) in X	
	lda VDC_addrl		        		; Load low byte of source in A
	jsr VDC_Write						; Write VDC
	
	; Number of bytes to copy
	ldx #VDC_WORDCOUNT    				; Load $1E for register 30 (word count) in X
	lda VDC_tmp1		        		; Load page counter in A
	cmp #$00    						; Check if this is the last page
	bne notyetlastpage			        ; Branch to 'not yet last page' if not equal
	lda VDC_tmp2		        		; Set length in last page
    clc                                 ; Clear carry
    adc #$01                            ; Add one
	jmp lastpage		        		; Goto last page label
notyetlastpage:							; Label for not yet last page
	lda #$00    						; Set length for 256 bytes
lastpage:								; Label for jmp if last page
	jsr VDC_Write						; Write VDC

	; Decrease page counter and loop until last page
	inc VDC_desth		        		; Increase destination address page counter
	inc VDC_addrh		        		; Increase source address page counter
	dec VDC_tmp1		        		; Decrease page counter
    lda VDC_tmp1		        		; Load page counter in A
	cmp #$ff    						; Check if this is the last page
	bne loopmemcpy				        ; Repeat loop until page counter is zero
    rts

; ------------------------------------------------------------------------------------------
VDC_Clearscreen:
; Function to clear VDC screen
; ------------------------------------------------------------------------------------------

    ; Set parameters for clearing characters
    lda #$00                            ; Load 0 for low and high byte of destination start address
    sta VDC_desth                       ; Store high byre
    sta VDC_destl                       ; Store low byte
    lda #$08                            ; Load 8 for 8 pages
    sta VDC_tmp1                        ; Store as counter

loopclearscreenchar:
	; Hi-byte of the destination address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_desth      				    ; Load high byte of dest in A
	jsr VDC_Write						; Write VDC

	; Lo-byte of the destination address to register 19
	inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda VDC_destl       				; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

	; Store character to write in data register 31
	ldx #VDC_DATA   					; Load $1f for register 31 (VDC data) in X	
	lda #$20       			        	; Load $20 for space
	jsr VDC_Write						; Write VDC

	; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X	
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC
	
	; Number of bytes to copy
	ldx #VDC_WORDCOUNT    				; Load $1E for register 30 (word count) in X
	lda #$00    		        		; Load 0 for full page
	jsr VDC_Write						; Write VDC

	; Decrease page counter and loop until last page
	inc VDC_desth		        		; Increase destination address page counter
	inc VDC_addrh		        		; Increase source address page counter
	dec VDC_tmp1		        		; Decrease page counter
    lda VDC_tmp1		        		; Load page counter in A
	cmp #$ff    						; Check if this is the last page
	bne loopclearscreenchar			    ; Repeat loop until page counter is zero

    ; Set parameters for clearing attributes
    lda #$08                            ; Load 8 for high byte of destination start address
    sta VDC_desth                       ; Store high byre
    lda #$00                            ; Load 0 for low byte of destination start address
    sta VDC_destl                       ; Store low byte
    lda #$08                            ; Load 8 for 8 pages
    sta VDC_tmp1                        ; Store as counter

loopclearscreenattr:
	; Hi-byte of the destination address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_desth      				    ; Load high byte of dest in A
	jsr VDC_Write						; Write VDC

	; Lo-byte of the destination address to register 19
	inx                     			; Load $13 for register 19 (VDC RAM address high) in X	
	lda VDC_destl       				; Load low byte of dest in A
	jsr VDC_Write						; Write VDC

	; Store character to write in data register 31
	ldx #VDC_DATA   					; Load $1f for register 31 (VDC data) in X	
	lda #$20       			        	; Load $0 for black
	jsr VDC_Write						; Write VDC

	; Clear the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY					; Load $18 for register 24 (block copy mode) in X		
	lda #$00				        	; Load 0 in A
	jsr VDC_Write						; Write VDC
	
	; Number of bytes to copy
	ldx #VDC_WORDCOUNT    				; Load $1E for register 30 (word count) in X
	lda #$00    		        		; Load 0 for full page
	jsr VDC_Write						; Write VDC

	; Decrease page counter and loop until last page
	inc VDC_desth		        		; Increase destination address page counter
	inc VDC_addrh		        		; Increase source address page counter
	dec VDC_tmp1		        		; Decrease page counter
    lda VDC_tmp1		        		; Load page counter in A
	cmp #$ff    						; Check if this is the last page
	bne loopclearscreenattr			    ; Repeat loop until page counter is zero
    rts


; ------------------------------------------------------------------------------------------
VDC_CopyMemToVDC:
; Function to copy memory from VDC memory to standard memory
; Input:	VDC_addrh = high byte of source address
;			VDC_addrl = low byte of source address
;			VDC_desth = high byte of VDC destination address
;			VDC_destl = low byte of VDC destination address
;			VDC_tmp1 =  high byte of length
;			VDC_tmp2 =  low byte of length
;           Set length as number of bytes minus 1 (so zero based)
; ------------------------------------------------------------------------------------------

	; Set address pointer in zero-page
	lda VDC_addrl				    	; Obtain low byte in A
	sta ZP1								; Store low byte in pointer
	lda VDC_addrh						; Obtain high byte in A
	sta ZP2								; Store high byte in pointer

	; Hi-byte of the source VDC address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_desth		        	    ; Load high byte of address in A
	jsr VDC_Write						; Write VDC

	; Low-byte of the source VDC address to register 19
	inx 								; Increase X for register 19 (VDC RAM address low)
	lda VDC_destl     			        ; Load low byte of address in A
	jsr VDC_Write						; Write VDC

	; Start of copy loop
	ldy #$00    						; Set Y as counter on 0
	
	; Read value and store at VDC address
copyloopm2v:							; Start of copy loop
	lda (ZP1),Y						    ; Load source data
	ldx #VDC_DATA    					; Load $1f for register 31 (VDC data) in X
	jsr VDC_Write						; Write VDC

	; Increase source address (VDC auto increments)
	inc ZP1								; Increment low byte of source address
	bne nextm2v1						; If not yet zero, branch to next label
	inc ZP2								; Increment high byte of source address
nextm2v1:								; Next label
	dec VDC_tmp2						; Decrease low byte of length
	lda VDC_tmp2						; Load low byte of length to A
	cmp #$ff							; Check if below zero
	bne copyloopm2v						; Continue loop if not yet below zero
	dec VDC_tmp1						; Decrease high byte of length
	lda VDC_tmp1						; Load high byte of length to A
	cmp #$ff							; Check if below zero
	bne copyloopm2v						; Continue loop if not yet below zero
	rts

; ------------------------------------------------------------------------------------------
VDC_CopyVDCToMem:
; Function to copy memory from VDC memory to standard memory
; Input:	VDC_addrh = high byte of VDC source address
;			VDC_addrl = low byte of VDC source address
;			VDC_desth = high byte of destination address
;			VDC_destl = low byte of destination address
;			VDC_tmp1 =  high byte of length
;			VDC_tmp2 =  low byte of length
;           Set length as number of bytes minus 1 (so zero based)
; ------------------------------------------------------------------------------------------

	; Set address pointer in zero-page and STAVEC vector
	lda VDC_destl						; Obtain low byte in A
	sta ZP1								; Store low byte in pointer
	lda VDC_desth						; Obtain high byte in A
	sta ZP2								; Store high byte in pointer

	; Start of copy loop
	ldy #$00    						; Set Y as counter on 0

copyloopv2m:							; Start of copy loop

	; Hi-byte of the source VDC address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda VDC_addrh		        		; Load high byte of address in A
	jsr VDC_Write						; Write VDC

	; Low-byte of the source VDC address to register 19
	inx 								; Increase X for register 19 (VDC RAM address low)
	lda VDC_addrl      				    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	
	; Read VDC value and store at destination address
	ldx #VDC_DATA    					; Load $1f for register 31 (VDC data) in X
	jsr VDC_Read						; Read VDC
	sta (ZP1),y							; Store in target memory

	; Increase VDC source address and target memory address
	inc ZP1								; Increment low byte of target address
	bne nextv2m1						; If not yet zero, branch to next label
	inc ZP2								; Increment high byte of target address
nextv2m1:								; Next label
	inc VDC_addrl						; Increment low byte of VDC address
	bne nextv2m2						; If not yet zero, branch to next label
	inc VDC_addrh						; Increment hight byte of VDC address
nextv2m2:								; Next label
	dec VDC_tmp2						; Decrease low byte of length
	lda VDC_tmp2						; Load low byte of length to A
	cmp #$ff							; Check if below zero
	bne copyloopv2m						; Continue loop if not yet below zero
	dec VDC_tmp1						; Decrease high byte of length
	lda VDC_tmp1						; Load high byte of length to A
	cmp #$ff							; Check if below zero
	bne copyloopv2m						; Continue loop if not yet below zero
    rts

; VBlank routines

; ------------------------------------------------------------------------------------------
VDC_Wait_vblank:
; Wait for vblank, ensure no irq happens while waiting, and also restore the irq flag
; to ensure we do not clear it if it was already set before we are called...
; ------------------------------------------------------------------------------------------
    
    php
    sei
    lda #%00100000
loop_wait_vblank:
    bit VDC_ADDRESS_REGISTER
    beq loop_wait_vblank
    plp
    rts

; ------------------------------------------------------------------------------------------
VDC_Wait_no_vblank:
; Wait for no vblank
; ------------------------------------------------------------------------------------------
    lda #%00100000
loop_wait_novblank:
    bit VDC_ADDRESS_REGISTER
    bne loop_wait_novblank
    rts

; Generic routines

; ------------------------------------------------------------------------------------------
textout:
; Function to print a text to screen
; Input:	ZP1 = low byte of text address
; 			ZP2 = high byte of text address
; ------------------------------------------------------------------------------------------

	ldy #$00							; Set index to zero
textout_loop:
	lda (ZP1),y							; Get character
	beq textout_end						; Text end?
	jsr BSOUT							; Print char
	iny									; Next char
	jmp textout_loop					; Loop
textout_end:
	rts									; Return

; ------------------------------------------------------------------------------------------
waitkey:
; Function to wait for a key press
; ------------------------------------------------------------------------------------------
loop_waitkey:                       
    jsr Effects_do                      ; Perform effects
    jsr GETIN                           ; Check keyboard input               
    beq loop_waitkey                    ; Loop until key press
    rts

; ------------------------------------------------------------------------------------------
waitkey_noeffects:
; Function to wait for a key press
; ------------------------------------------------------------------------------------------
loop_waitkey_ne:                       
    jsr GETIN                           ; Check keyboard input               
    beq loop_waitkey_ne                 ; Loop until key press
    rts

; ------------------------------------------------------------------------------------------
setmmuconfig:
; Function to safeguard old MMU config and set new one.
; Input:    MMU_config = desired MMU config
; Output:   MMU_tmp = old MMU config to be safeguarded
; ------------------------------------------------------------------------------------------
    lda BANKCONFIG                      ; Load present config
    sta MMU_tmp                         ; Safeguard to temporary storage
    lda MMU_config                      ; Load desired config
    sta BANKCONFIG                      ; Set loaded desired config
    rts

; ------------------------------------------------------------------------------------------
restoremmuconfig:
; Function to safeguard old MMU config and set new one.
; Input:    MMU_config = desired MMU config
; Output:   MMU_tmp = old MMU config to be safeguarded
; ------------------------------------------------------------------------------------------
    lda MMU_tmp                         ; Load safeguarded MMU config
    sta BANKCONFIG                      ; Set loaded safeguarded config
    rts

; Variables
bgcolor:
    .byte 0                             ; Background color
MMU_config:
    .res    1
MMU_tmp:
    .res    1
VDC_regadd:
	.res	1
VDC_regval:
	.res	1
VDC_addrh:
	.res	1
VDC_addrl:
	.res	1
VDC_desth:
	.res	1
VDC_destl:
	.res	1
VDC_strideh:
	.res	1
VDC_stridel:
	.res	1
VDC_value:
	.res	1
VDC_tmp1:
	.res	1
VDC_tmp2:
	.res	1
VDC_tmp3:
	.res	1
VDC_tmp4:
	.res	1
SIDIRQtmp1:
    .res    1
SIDIRQtmp2:
    .res    1
Scroll_enable:
    .byte   0
Scroll_fill:
    .byte   0
Scroll_VDC_lb:
    .res    1
Scroll_VDC_hb:
    .res    1
Scroll_src_txt_lb:
    .res    1
Scroll_src_txt_hb:
    .res    1
Scroll_src_att_lb:
    .res    1
Scroll_src_att_hb:
    .res    1
Scroll_length_lb:
    .res    1
Scroll_length_hb:
    .res    1
Scroll_counter:
    .res    1
Scroll_lines:
    .res    1
Scroll_startline:
    .res    1
Scroll_endline:
    .res    1
Scroll_VDC_tmp_lb:
    .res    1
Scroll_VDC_tmp_hb:
    .res    1
Scroll_src_txt_tmp_lb:
    .res    1
Scroll_src_txt_tmp_hb:
    .res    1
Scroll_src_att_tmp_lb:
    .res    1
Scroll_src_att_tmp_hb:
    .res    1
Smooth_enable:
    .byte   0
Smooth_inorout:
    .byte   0
Smooth_src_txt_lb:
    .res    1
Smooth_src_txt_hb:
    .res    1
Smooth_src_att_lb:
    .res    1
Smooth_src_att_hb:
    .res    1
Smooth_charcounter:
    .res    1
Smooth_charcounter_inv:
    .res    1
Smooth_pixelcounter:
    .res    1
Fade_enable:
    .byte   0
Fade_inorout:
    .res    1
Fade_VDC_lb:
    .res    1
Fade_VDC_hb:
    .res    1
Fade_source_lb:
    .res    1
Fade_source_hb:
    .res    1
Fade_lines:
    .res    1
Fade_startline:
    .res    1
Fade_counter:
    .res    1
Fade_length_lb:
    .res    1
Fade_length_hb:
    .res    1
Fade_pages_tmp:
    .res    1

; Fade colors
Fade_colors:
    .byte $00,$01,$0E,$0F

; Text
switchvdcmessage:
    .byte "switch to 80 column mode to view.",0

; Tables

length_lb:
    .byte $FF
    .byte $4F
    .byte $9F
    .byte $EF
    .byte $3F
    .byte $8F
    .byte $DF
    .byte $2F
    .byte $7F
    .byte $CF
    .byte $1F
    .byte $6F
    .byte $BF
    .byte $0F
    .byte $5F
    .byte $AF
    .byte $FF
    .byte $4F
    .byte $9F
    .byte $EF
    .byte $3F
    .byte $8F
    .byte $DF
    .byte $2F
    .byte $7F

length_hb:
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $01
    .byte $01
    .byte $01
    .byte $02
    .byte $02
    .byte $02
    .byte $03
    .byte $03
    .byte $03
    .byte $04
    .byte $04
    .byte $04
    .byte $05
    .byte $05
    .byte $05
    .byte $05
    .byte $06
    .byte $06
    .byte $06
    .byte $07
    .byte $07

text_lb:
    .byte $00
    .byte $50
    .byte $A0
    .byte $F0
    .byte $40
    .byte $90
    .byte $E0
    .byte $30
    .byte $80
    .byte $D0
    .byte $20
    .byte $70
    .byte $C0
    .byte $10
    .byte $60
    .byte $B0
    .byte $00
    .byte $50
    .byte $A0
    .byte $F0
    .byte $40
    .byte $90
    .byte $E0
    .byte $30
    .byte $80

att_hb:
    .byte $08
    .byte $08
    .byte $08
    .byte $08
    .byte $09
    .byte $09
    .byte $09
    .byte $0A
    .byte $0A
    .byte $0A
    .byte $0B
    .byte $0B
    .byte $0B
    .byte $0C
    .byte $0C
    .byte $0C
    .byte $0D
    .byte $0D
    .byte $0D
    .byte $0D
    .byte $0E
    .byte $0E
    .byte $0E
    .byte $0F
    .byte $0F

swap_txt_hb:
    .byte $10
    .byte $10
    .byte $10
    .byte $10
    .byte $11
    .byte $11
    .byte $11
    .byte $12
    .byte $12
    .byte $12
    .byte $13
    .byte $13
    .byte $13
    .byte $14
    .byte $14
    .byte $14
    .byte $15
    .byte $15
    .byte $15
    .byte $15
    .byte $16
    .byte $16
    .byte $16
    .byte $17
    .byte $17

swap_att_hb:
    .byte $18
    .byte $18
    .byte $18
    .byte $18
    .byte $19
    .byte $19
    .byte $19
    .byte $1A
    .byte $1A
    .byte $1A
    .byte $1B
    .byte $1B
    .byte $1B
    .byte $1C
    .byte $1C
    .byte $1C
    .byte $1D
    .byte $1D
    .byte $1D
    .byte $1D
    .byte $1E
    .byte $1E
    .byte $1E
    .byte $1F
    .byte $1F

lastchar_lb:
    .byte $4F
    .byte $9F
    .byte $EF
    .byte $3F
    .byte $8F
    .byte $DF
    .byte $2F
    .byte $7F
    .byte $CF
    .byte $1F
    .byte $6F
    .byte $BF
    .byte $0F
    .byte $5F
    .byte $AF
    .byte $FF
    .byte $4F
    .byte $9F
    .byte $EF
    .byte $3F
    .byte $8F
    .byte $DF
    .byte $2F
    .byte $7F
    .byte $CF

lastchar_hb:
    .byte $10
    .byte $10
    .byte $10
    .byte $11
    .byte $11
    .byte $11
    .byte $12
    .byte $12
    .byte $12
    .byte $13
    .byte $13
    .byte $13
    .byte $14
    .byte $14
    .byte $14
    .byte $14
    .byte $15
    .byte $15
    .byte $15
    .byte $16
    .byte $16
    .byte $16
    .byte $17
    .byte $17
    .byte $17


lastchar_hb_att:
    .byte $18
    .byte $18
    .byte $18
    .byte $19
    .byte $19
    .byte $19
    .byte $1A
    .byte $1A
    .byte $1A
    .byte $1B
    .byte $1B
    .byte $1B
    .byte $1C
    .byte $1C
    .byte $1C
    .byte $1C
    .byte $1D
    .byte $1D
    .byte $1D
    .byte $1E
    .byte $1E
    .byte $1E
    .byte $1F
    .byte $1F
    .byte $1F

double_txt_lb:
    .byte $00
    .byte $A0
    .byte $40
    .byte $E0
    .byte $80
    .byte $20
    .byte $C0
    .byte $60
    .byte $00
    .byte $A0
    .byte $40
    .byte $E0
    .byte $80
    .byte $20
    .byte $C0
    .byte $60
    .byte $00
    .byte $A0
    .byte $40
    .byte $E0
    .byte $80
    .byte $20
    .byte $C0
    .byte $60
    .byte $00

double_txt_hb:
    .byte $00
    .byte $00
    .byte $01
    .byte $01
    .byte $02
    .byte $03
    .byte $03
    .byte $04
    .byte $05
    .byte $05
    .byte $06
    .byte $06
    .byte $07
    .byte $08
    .byte $08
    .byte $09
    .byte $0A
    .byte $0A
    .byte $0B
    .byte $0B
    .byte $0C
    .byte $0D
    .byte $0D
    .byte $0E
    .byte $0F

double_att_hb:
    .byte $10
    .byte $10
    .byte $11
    .byte $11
    .byte $12
    .byte $13
    .byte $13
    .byte $14
    .byte $15
    .byte $15
    .byte $16
    .byte $16
    .byte $17
    .byte $18
    .byte $18
    .byte $19
    .byte $1A
    .byte $1A
    .byte $1B
    .byte $1B
    .byte $1C
    .byte $1D
    .byte $1D
    .byte $1E
    .byte $1F

double_swp_lb:
    .byte $50
    .byte $F0
    .byte $90
    .byte $30
    .byte $D0
    .byte $70
    .byte $10
    .byte $B0
    .byte $50
    .byte $F0
    .byte $90
    .byte $30
    .byte $D0
    .byte $70
    .byte $10
    .byte $B0
    .byte $50
    .byte $F0
    .byte $90
    .byte $30
    .byte $D0
    .byte $70
    .byte $10
    .byte $B0
    .byte $50

double_swp_txt_hb:
    .byte $00
    .byte $00
    .byte $01
    .byte $02
    .byte $02
    .byte $03
    .byte $04
    .byte $04
    .byte $05
    .byte $05
    .byte $06
    .byte $07
    .byte $07
    .byte $08
    .byte $09
    .byte $09
    .byte $0A
    .byte $0A
    .byte $0B
    .byte $0C
    .byte $0C
    .byte $0D
    .byte $0E
    .byte $0E
    .byte $0F

double_swp_att_hb:
    .byte $10
    .byte $10
    .byte $11
    .byte $12
    .byte $12
    .byte $13
    .byte $14
    .byte $14
    .byte $15
    .byte $15
    .byte $16
    .byte $17
    .byte $17
    .byte $18
    .byte $19
    .byte $19
    .byte $1A
    .byte $1A
    .byte $1B
    .byte $1C
    .byte $1C
    .byte $1D
    .byte $1E
    .byte $1E
    .byte $1F





