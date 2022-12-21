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
    VDC_UPDATEADRESSHIGH    = $12       ; VDC Register Update Address High Byte
    VDC_UPDATEADRESSLOW     = $13       ; VDC Register Update Address Low Byte
    VDC_BLOCKCOPY           = $18    	; VDC Register for block copy or block write mode
    VDC_WORDCOUNT           = $1E       ; VDC Register for word count
    VDC_BCOPY_SOURCE_HB     = $20       ; VDC Register for block copy source address high byte
    VDC_BCOPY_SOURCE_LB     = $21       ; VDC Register for block copy source address low byte
    VDC_DATA                = $1F       ; VDC data register
    VDC_FGBGCOLOR           = $1A       ; VDC Foreground/Background color

    ; Zero page addresses
    ZP1                     = $FA       ; First zero page address used
    ZP2                     = $FB       ; Second zero page address used
    ZP3                     = $FC       ; Third zero page address used
    ZP4                     = $FF       ; Fourth zero page address used
    ZPSID1                  = $FD       ; First ZP address reserved for SID music
    ZPSID2                  = $FE       ; Second ZP address reserved for SID music

    ; Asset addresses
    SCREENHB                = $50       ; High byte of screen address
    SCREENLB                = $00       ; Low byte of screen address

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
    jsr waitkey                         ; Wait for a key
	jsr SWAPPER							; Switch to 80 column mode

vdcmode:
    ; Set fast mode
    lda #$01                            ; Load 1 to enable fast mode
    sta CLKRATE                         ; Set mode

	; Set VDC background color
    ldx #VDC_FGBGCOLOR
    lda bgcolor
    jsr VDC_Write

    ; Start interrupt routines
    jsr IRQ_init                        ; Start interrupt routines

    ; Copy screen to VDC
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

    ; Scroll out left
    jsr waitkey                         ; Wait for key
    lda #$00                            ; Set fill flag as 0
    sta Scroll_fill                     ; Store to variable
    lda #$05                            ; Set number of lines to 5
    sta Scroll_lines                    ; Store to variable
    lda #$00                            ; Load 0 for VDC high and low byte
    sta Scroll_tmp_addr_lb              ; Store to variable
    sta Scroll_tmp_addr_hb              ; Store to variable
    lda #$50                            ; Load 80 characters as counter
    sta Scroll_counter                  ; Store to variable
    lda #$01                            ; Load one to set flag
    sta Scroll_enable                   ; Set flag to enable scroll  

    ; Scroll in left
    jsr waitkey                         ; Wait for key
    lda #$01                            ; Set fill flag as 1
    sta Scroll_fill                     ; Store to variable
    lda #$05                            ; Set number of lines to 5
    sta Scroll_lines                    ; Store to variable
    lda #$00                            ; Load 0 for VDC high and low byte
    sta Scroll_tmp_addr_lb              ; Store to variable
    sta Scroll_tmp_addr_hb              ; Store to variable
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
    lda #$01                            ; Load one to set flag
    sta Scroll_enable                   ; Set flag to enable scroll

    ; Fade out
    jsr waitkey                         ; Wait for key
    lda #$03                            ; Load 3 for color index max for fade out
    sta Fade_counter                    ; Set counter
    lda #$00                            ; Load 0 for VDC high byte and low byte
    sta Fade_VDC_hb                     ; Store high byte
    sta Fade_VDC_lb                     ; Store low byte
    lda #$05                            ; Load 5 for 5 lines
    sta Fade_lines                      ; Store to variable
    lda #$02                            ; Load 2 for fade out
    sta Fade_enable                     ; Set flag to enable fade out

    ; Fade in
    jsr waitkey                         ; Wait for key
    lda #$00                            ; Load 3 for color index max for fade out
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
    lda #$01                            ; Load 1 for fade out
    sta Fade_enable                     ; Set flag to enable fade in

    ; End and reset
    jsr waitkey                         ; Wait for a keypress
    jsr IRQ_stop                        ; Stop interrupt routines
    jsr resetSID                        ; Mute SID

    ; Disable fast mode
    lda #$00                            ; Load 1 to enable fast mode
    sta CLKRATE                         ; Set mode
    jmp RESET                           ; Perform reset as SID is at BASIC location, so cannot return to BASIC
    rts

; IRQ routines

; ------------------------------------------------------------------------------------------
IRQ_init:
; ------------------------------------------------------------------------------------------

    ; init SID
	jsr SIDINIT							; Jump to SID init routine

    ; set IRQ pointer in $314/$315
    sei									; Stop interupts
	lda IRQ_LB							; Load old IRQ low byte
	sta SIDIRQtmp1						; Store at temp location
	sta doneIRQ+1						; Auto modify JMP to old IRQ
    lda #<IRQ_main 				    	; Load new IRQ low byte
	sta IRQ_LB							; Store new IRQ low byte
	lda IRQ_HB							; Load old IRQ high byte
	sta SIDIRQtmp2						; Store at temp location
	sta doneIRQ+2						; Auto modify JMP to old IRQ
    lda #>IRQ_main				    	; Load new IRQ high byte
	sta IRQ_HB							; Store new IRQ high byte
    cli									; Re-enable interupts
    rts       							; Return

; ------------------------------------------------------------------------------------------
IRQ_stop:
; ------------------------------------------------------------------------------------------

    ; restore IRQ vector to kernel interrupt routine
    sei									; Stop interupts
	ldx SIDIRQtmp1						; Load low byte of old IRQ
	ldy SIDIRQtmp2						; Load high byte of old IRQ
	stx IRQ_LB							; Store low byte
	sty IRQ_HB							; Store high byte
    cli 								; Enable interupts
	jsr resetSID						; Reset SID to mute sounds
    rts 								; Return

; ------------------------------------------------------------------------------------------
IRQ_main:
; ------------------------------------------------------------------------------------------

    ; Set MMU config
    lda #$0E                            ; Load config for bank 0 with kernal
    sta MMU_config                      ; Store in variable
    jsr setmmuconfig                    ; Safeguard old and set new MMU config

    ; Play SID frame
    jsr SIDPLAY                         ; Play a SID frame

    ; Perform scroll effects
    jsr Scroll_left                     ; Scroll left if enabled

    ; Perform fade effects
    jsr Fade                            ; Fade if enabled

    ; Restore old MMU config
    jsr restoremmuconfig                ; Restore old MMU config

doneIRQ:
    jmp $FA65                           ; Jump to old IRQ (will be automodified)

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
Scroll_decrease_address:
; Function to decrease temp address by one
; ------------------------------------------------------------------------------------------

    sec                                 ; Set carry for subtraction
    lda Scroll_tmp_addr_lb              ; Load low byte
    sbc #$01                            ; Decrease by one
    sta Scroll_tmp_addr_lb              ; Save again
    lda Scroll_tmp_addr_hb              ; Load high byte
    sbc #$00                            ; Decrease with carry
    sta Scroll_tmp_addr_hb              ; Save again
    rts

; ------------------------------------------------------------------------------------------
Scroll_increase_address:
; Function to decrease temp address by one
; ------------------------------------------------------------------------------------------

    clc                                 ; Clear carry for addition
    lda Scroll_tmp_addr_lb              ; Load low byte
    adc #$01                            ; Increase by one
    sta Scroll_tmp_addr_lb              ; Save again
    lda Scroll_tmp_addr_hb              ; Load high byte
    adc #$00                            ; Increase with carry
    sta Scroll_tmp_addr_hb              ; Save again
    rts

; ------------------------------------------------------------------------------------------
Scroll_decrease_line:
; Function to decrease temp address by one
; ------------------------------------------------------------------------------------------

    sec                                 ; Set carry for subtraction
    lda Scroll_tmp_addr_lb              ; Load low byte
    sbc #$50                            ; Decrease by 80 for one line
    sta Scroll_tmp_addr_lb              ; Save again
    lda Scroll_tmp_addr_hb              ; Load high byte
    sbc #$00                            ; Decrease with carry
    sta Scroll_tmp_addr_hb              ; Save again
    rts

; ------------------------------------------------------------------------------------------
Scroll_increase_line:
; Function to decrease temp address by one
; ------------------------------------------------------------------------------------------

    ; VDC address
    clc                                 ; Clear carry for addition
    lda Scroll_tmp_addr_lb              ; Load low byte
    adc #$50                            ; Increase by 80 for one line
    sta Scroll_tmp_addr_lb              ; Save again
    lda Scroll_tmp_addr_hb              ; Load high byte
    adc #$00                            ; Increase with carry
    sta Scroll_tmp_addr_hb              ; Save again  
    rts

; ------------------------------------------------------------------------------------------
Scroll_plot_blank:
; Function to plot blank char if fill is not set
; ------------------------------------------------------------------------------------------

    ; Text
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
	lda Scroll_tmp_addr_hb              ; Load high byte of address in A
	jsr VDC_Write						; Write VDC
	inx		    						; Increase X for register 19 (VDC RAM address low)
	lda Scroll_tmp_addr_lb  		    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    				    ; Load $1f for register 31 (VDC data) in X	
	lda #$20               				; Load 32 to plot space in A
	jsr VDC_Write						; Write VDC

    ; Attribute
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
	lda Scroll_tmp_addr_hb              ; Load high byte of address in A
    clc                                 ; Clear carry
    adc #$08                            ; Add 8 pages for attribute memory
	jsr VDC_Write						; Write VDC
	inx		    						; Increase X for register 19 (VDC RAM address low)
	lda Scroll_tmp_addr_lb  		    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    				    ; Load $1f for register 31 (VDC data) in X	
	lda #$00               				; Load 0 to plot black in A
	jsr VDC_Write						; Write VDC

    rts

; ------------------------------------------------------------------------------------------
Scroll_plot_fill:
; Function to plot new char from source if fill is set
; ------------------------------------------------------------------------------------------

    ; Set source address pointer
    lda Scroll_tmp_src_txt_lb           ; Load low byte of text source address
    sta scroll_plot_fill_text+1         ; Auto modify address pointer
    lda Scroll_tmp_src_txt_hb           ; Load low byte of text source address
    sta scroll_plot_fill_text+2         ; Auto modify address pointer
    lda Scroll_tmp_src_att_lb           ; Load low byte of agttribute source address
    sta scroll_plot_fill_attr+1         ; Auto modify address pointer
    lda Scroll_tmp_src_att_hb           ; Load low byte of attribute source address
    sta scroll_plot_fill_attr+2         ; Auto modify address pointer
    
    ; Text
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
	lda Scroll_tmp_addr_hb              ; Load high byte of address in A
	jsr VDC_Write						; Write VDC
	inx		    						; Increase X for register 19 (VDC RAM address low)
	lda Scroll_tmp_addr_lb  		    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    				    ; Load $1f for register 31 (VDC data) in X
scroll_plot_fill_text:	
	lda scroll_plot_fill_text+1     	; Load value from auto modifying address
	jsr VDC_Write						; Write VDC

    ; Attribute
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
	lda Scroll_tmp_addr_hb              ; Load high byte of address in A
    clc                                 ; Clear carry
    adc #$08                            ; Add 8 pages for attribute memory
	jsr VDC_Write						; Write VDC
	inx		    						; Increase X for register 19 (VDC RAM address low)
	lda Scroll_tmp_addr_lb  		    ; Load low byte of address in A
	jsr VDC_Write						; Write VDC
	ldx #VDC_DATA    				    ; Load $1f for register 31 (VDC data) in X
scroll_plot_fill_attr:	
	lda scroll_plot_fill_attr+1    		; Load value from auto modifying address
	jsr VDC_Write						; Write VDC
    rts


; ------------------------------------------------------------------------------------------
Scroll_copy_left:
; Function to perform scroll copy via swap memory
; ------------------------------------------------------------------------------------------

    ; Set destination in swap memory
    ; Hi-byte of the destination address to register 18
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
    lda Scroll_tmp_addr_hb              ; Load destination high byte
    clc                                 ; Clear carry
    adc #$10                            ; Add $10 to get to swap VDC memory
	jsr VDC_Write						; Write VDC

    ; Lo-byte of the destination address to register 19
	ldx #VDC_UPDATEADRESSLOW			; Load $13 for register 19 (VDC RAM address low) in X	
	lda Scroll_tmp_addr_lb              ; Store at temporary location
	jsr VDC_Write						; Write VDC

    ; Set the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY  				; Load $18 for register 24 (block copy mode) in X	
	lda #$80			        		; Load prepared value with bit 7 set in A
	jsr VDC_Write						; Write VDC

    ; Set source in active screen
    ; Increase source address by one to scroll left
    jsr Scroll_increase_address         ; Increase address to get from second char as source

    ; Hi-byte of the source address to block copy source register 32
	ldx #VDC_BCOPY_SOURCE_HB	    	; Load $20 for register 32 (block copy source) in X	
	lda Scroll_tmp_addr_hb 	        	; Load high byte of source in A
	jsr VDC_Write						; Write VDC
	
	; Lo-byte of the source address to block copy source register 33
	ldx #VDC_BCOPY_SOURCE_LB	    	; Load $21 for register 33 (block copy source) in X	
	lda Scroll_tmp_addr_lb        		; Load low byte of source in A
	jsr VDC_Write						; Write VDC

    ; Number of bytes to copy
	ldx #VDC_WORDCOUNT   				; Load $1E for register 30 (word count) in X
	lda #$4F    		        		; Set length at one line minus 1, so 79
	jsr VDC_Write						; Write VDC

    ; Decrease again
    jsr Scroll_decrease_address         ; Decrease address to get back at original address

    ; Set destination in active screen
    ; Hi-byte of the destination address to register 18
    ldx #VDC_UPDATEADRESSHIGH           ; Load $12 for register 18 (VDC RAM address high) in X	
    lda Scroll_tmp_addr_hb              ; Load destination high byte
	jsr VDC_Write						; Write VDC

    ; Lo-byte of the destination address to register 19
	ldx #VDC_UPDATEADRESSLOW			; Load $13 for register 19 (VDC RAM address low) in X	
	lda Scroll_tmp_addr_lb              ; Store at temporary location
	jsr VDC_Write						; Write VDC

    ; Set the copy bit (bit 7) of register 24 (block copy mode)
	ldx #VDC_BLOCKCOPY  				; Load $18 for register 24 (block copy mode) in X	
	lda #$80			        		; Load prepared value with bit 7 set in A
	jsr VDC_Write						; Write VDC

    ; Set source in swap memory
    ; Hi-byte of the source address to block copy source register 32
	ldx #VDC_BCOPY_SOURCE_HB	    	; Load $20 for register 32 (block copy source) in X	
	lda Scroll_tmp_addr_hb 	        	; Load high byte of source in A
    clc                                 ; Clear carry
    adc #$10                            ; Add $10 to get to swap VDC memory
	jsr VDC_Write						; Write VDC
	
	; Lo-byte of the source address to block copy source register 33
	ldx #VDC_BCOPY_SOURCE_LB	    	; Load $21 for register 33 (block copy source) in X	
	lda Scroll_tmp_addr_lb        		; Load low byte of source in A
	jsr VDC_Write						; Write VDC

    ; Number of bytes to copy
	ldx #VDC_WORDCOUNT   				; Load $1E for register 30 (word count) in X
	lda #$4F    		        		; Set length at one line minus 1, so 79
	jsr VDC_Write						; Write VDC

    rts

; ------------------------------------------------------------------------------------------
Scroll_left:
; Function to scroll selected lines one position left
; ------------------------------------------------------------------------------------------
   
    ; Check if scroll is enabled
    lda Scroll_enable                   ; Load scroll enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne scroll_left_start               ; If zero then go to end of routine
    rts

scroll_left_start:
    ; Set parameters
    ldy Scroll_lines                    ; Set counter for number of lines
    lda Scroll_VDC_lb      				; Load low byte of dest in A
    sta Scroll_tmp_addr_lb              ; Store at temporary location
    lda Scroll_VDC_hb      				; Load high byte of dest in A
    sta Scroll_tmp_addr_hb              ; Store at temporary location

    ; Check if fill
    lda Scroll_fill                     ; Load fill flag
    cmp #$00                            ; Compare with zero to check if flag is set
    beq loop_scroll_left                ; Go to start of scroll loop if not set

    ; Set source address for fill parameters
    lda Scroll_src_txt_lb               ; Load text low byte
    sta Scroll_tmp_src_txt_lb           ; Store at temporary location
    lda Scroll_src_txt_hb               ; Load text high byte
    sta Scroll_tmp_src_txt_hb           ; Store at temporary location
    lda Scroll_src_att_lb               ; Load attribute low byte
    sta Scroll_tmp_src_att_lb           ; Store at temporary location
    lda Scroll_src_att_hb               ; Load attribute high byte
    sta Scroll_tmp_src_att_hb           ; Store at temporary location


loop_scroll_left:
    ; Copy text memory
    jsr Scroll_copy_left                ; Perform scroll copy

    ; Copy attribute memory
    clc                                 ; Clear carry
    lda Scroll_tmp_addr_hb              ; Load high byte of work address
    adc #$08                            ; Add $08 to get to attribute memory
    sta Scroll_tmp_addr_hb              ; Store back
    jsr Scroll_copy_left                ; Perform scroll copy
    sec                                 ; Set carry
    lda Scroll_tmp_addr_hb              ; Load high byte of work address
    sbc #$08                            ; Decrease $08 to get back to text memory
    sta Scroll_tmp_addr_hb              ; Store back

    ; Next line
    jsr Scroll_increase_line            ; Set next line address

    ; Fill or clear last char of previous line
    jsr Scroll_decrease_address         ; Decrease by one to get last char of previous line

    ; Check if fill
    lda Scroll_fill                     ; Load fill flag
    cmp #$00                            ; Compare with zero to check if flag is set
    beq scroll_left_fillblank           ; Go to fill blank label if not set

    jsr Scroll_plot_fill                ; Fill from source

    ; Increase text source address
    clc                                 ; Clear carry for addition
    lda Scroll_tmp_src_txt_lb           ; Load low byte
    adc #$50                            ; Increase by 80 for one line
    sta Scroll_tmp_src_txt_lb           ; Save again
    lda Scroll_tmp_src_txt_hb           ; Load high byte
    adc #$00                            ; Increase with carry
    sta Scroll_tmp_src_txt_hb           ; Save again

    ; Increase attribute source address
    clc                                 ; Clear carry for addition
    lda Scroll_tmp_src_att_lb           ; Load low byte
    adc #$50                            ; Increase by 80 for one line
    sta Scroll_tmp_src_att_lb           ; Save again
    lda Scroll_tmp_src_att_hb           ; Load high byte
    adc #$00                            ; Increase with carry
    sta Scroll_tmp_src_att_hb           ; Save again
    jmp scroll_left_nextline

scroll_left_fillblank:
    jsr Scroll_plot_blank               ; Plot blank chars

scroll_left_nextline:
    ; Decrease line counter
    jsr Scroll_increase_address         ; Increase address again
    dey                                 ; Decrease line counter in Y
    bne loop_scroll_left                ; Loop until counter is zero

    ; Check if fill
    lda Scroll_fill                     ; Load fill flag
    cmp #$00                            ; Compare with zero to check if flag is set
    beq scroll_left_decreasecounter     ; Go to fill blank label if not set

    ; Increase source addresses by one character
    clc                                 ; Clear carry
    lda Scroll_src_txt_lb               ; Load text low byte
    adc #$01                            ; Add one
    sta Scroll_src_txt_lb               ; Save again
    lda Scroll_src_txt_hb               ; Load text high byte
    adc #$00                            ; Add 0 with carry
    sta Scroll_src_txt_hb               ; Save again
    clc                                 ; Clear carry
    lda Scroll_src_att_lb               ; Load attribute low byte
    adc #$01                            ; Add one
    sta Scroll_src_att_lb               ; Save again
    lda Scroll_src_att_hb               ; Load attribute high byte
    adc #$00                            ; Add 0 with carry
    sta Scroll_src_att_hb               ; Save again

scroll_left_decreasecounter:
    ; Decrease counter
    dec Scroll_counter                  ; Decrease counter
    bne scroll_left_end                 ; If not yet zero go to end
    lda #$00                            ; Load 0 to clear scroll enable flag
    sta Scroll_enable                   ; Disable scroll

scroll_left_end:
    rts                                 ; Return

; Fade routines

; ------------------------------------------------------------------------------------------
Fade:
; Function to fade in (Fade_enable=1) or out (Fade_enable=2)
; Input:    Fade_counter = 0 for fade in, 3 for fade out
;           Fade_VDC_hb and Fade_VDC_lb = VDC start address (text)
;           Fade_lines = number of lines
; ------------------------------------------------------------------------------------------
   
    ; Check if fade is enabled
    lda Fade_enable                     ; Load scroll enable flag
    cmp #$00                            ; Compare to zero to check if set
    bne fade_start                      ; If zero then go to end of routine
    rts

fade_start:
    ; Set start address	
	lda Fade_VDC_hb                     ; Load high byte of address in A
    clc                                 ; Clear carry
    adc #$08                            ; Add 8 pages for attribute memory
	sta VDC_addrh                       ; Store VDC hb
	lda Fade_VDC_lb     				; Load low byte of address in A
	sta VDC_addrl                       ; Store VDC lb

    ; Set line counter
    lda Fade_lines                      ; Load number of lines
    sta Fade_linenumber                 ; Set line counter

    ; Check for counter is 5 which is last stage of fade in
    ldx Fade_counter                    ; Load counter to X
    cpx #$04                            ; Compare to 4
    bne fade_loop_Y

    ; Set source address
    lda Fade_source_lb                  ; Load low byte
    sta fade_source_color+1             ; Auto modify source address pointer
    lda Fade_source_hb                  ; Load low byte
    sta fade_source_color+2             ; Auto modify source address pointer

fade_loop_Y:
    ; Set charachter counter:
    ldy #$00                            ; Set Y at 0 for X counter

fade_loop_X:
    ; Check for counter is 5 which is last stage of fade in
    ldx Fade_counter                    ; Load counter to X
    cpx #$04                            ; Compare to 4
    bne fade_next_color

    ; Change to source color
fade_source_color:
    lda fade_source_color+1             ; Load source from auto modify address
    jmp fade_store_color                ; Jump to store color

    ; Change to fade color
fade_next_color:
    ; Load present attribute and clear of color data
	jsr VDC_Peek						; Read VDC for present attribute
    lda VDC_value                       ; Read return variable
    and #$F0                            ; Clear lower four bits to clear of color data
    ldx Fade_counter                    ; Load counter to X
    ora Fade_colors,X                   ; Load color with X index and set color bits

fade_store_color:
    ; Store color
    sta VDC_value                       ; Save back to variable
    jsr VDC_Poke                        ; Write to VDC updated attribute

    ; Increase auto modifying source address
    lda fade_source_color+1             ; Load low byte
    clc                                 ; Clear carry
    adc #$01                            ; Increase with carry
    sta fade_source_color+1             ; Store again
    lda fade_source_color+2             ; Load high byte
    adc #$00                            ; Add zero with carry
    sta fade_source_color+2             ; Store again

    ; Increase VDC address
    clc                                 ; Clear carry
    lda VDC_addrl                       ; Load low byte
    adc #$01                            ; Add one with carry
    sta VDC_addrl                       ; Save again
    lda VDC_addrh                       ; Load high byte
    adc #$00                            ; Add zero with carry
    sta VDC_addrh                       ; Save again
    iny                                 ; Increase Y counter
    cpy #$50                            ; Compare to 80 to see if end of line is reached
    bne fade_loop_X                     ; Loop until end of line is reached

    ; Next line
    dec Fade_linenumber                 ; Decrease line counter
    bne fade_loop_Y                     ; Loop until last line has been done

    ; Check if fade is set as fade in
    lda Fade_enable                     ; Load scroll enable flag
    cmp #$01                            ; Compare to zero to check if set
    bne fade_out                        ; If zero then go to end of routine

    ; Increase color counter for fade in
    inc Fade_counter                    ; Increase color counter
    lda Fade_counter                    ; Load counter to check
    cmp #$05                            ; Compare to 4 to see if end is reached
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
VDC_CopyMemToVDC:
; Function to copy memory from VDC memory to standard memory
; Input:	VDC_addrh = high byte of source address
;			VDC_addrl = low byte of source address
;			VDC_desth = high byte of VDC destination address
;			VDC_destl = low byte of VDC destination address
;			VDC_tmp1 =  high byte of length
;			VDC_tmp2 =  low byte of length
; ------------------------------------------------------------------------------------------

	; Set address pointer in zero-page
	lda VDC_addrl				    	; Obtain low byte in A
	sta ZP1								; Store low byte in pointer
	lda VDC_addrh						; Obtain high byte in A
	sta ZP2								; Store high byte in pointer

	; Hi-byte of the source VDC address to register 18
	ldx #VDC_UPDATEADRESSHIGH    		; Load $12 for register 18 (VDC RAM address high) in X	
	lda #$00		        	    	; Load high byte of address in A
	jsr VDC_Write						; Write VDC

	; Low-byte of the source VDC address to register 19
	inx 								; Increase X for register 19 (VDC RAM address low)
	lda #$00     			        	; Load low byte of address in A
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
    jsr GETIN
    beq loop_waitkey
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
Scroll_lines:
    .res    1
Scroll_counter:
    .res    1
Scroll_tmp_addr_lb:
    .res    1
Scroll_tmp_addr_hb:
    .res    1
Scroll_tmp_src_txt_lb:
    .res    1
Scroll_tmp_src_txt_hb:
    .res    1
Scroll_tmp_src_att_lb:
    .res    1
Scroll_tmp_src_att_hb:
    .res    1
Fade_enable:
    .byte   0
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
Fade_linenumber:
    .res    1
Fade_counter:
    .res    1

; Fade colors
Fade_colors:
.byte $00,$01,$0E,$0F

; Text
switchvdcmessage:
.byte "switch to 80 column mode to view.",0