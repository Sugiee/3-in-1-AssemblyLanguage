; operating BUILD CONFIGURATIONS drop down menu in the DEBUG toolbar
; FOR SIMULATIONS with MPLAB SIM: select "Debug" this will switch off delays that take thousands of instructions
; HARDWARE: select "Release" all delays will be on

; Provided code - do not edit  
; This include setups up various configuration bits for the microcontroller
; we are using, and importantly reserves space for variables.
; If you need to use more variables, please place them in VAR.INC, which you
; can find under the "Header Files" folder. The variables listed there will be
; placed in the first memory bank.
; This code has been provided for you to simplify your work, but you should be
; aware that you cannot ignore it.
#include	ECH_1.inc
		
; Place your SUBROUTINE(S) (if any) here ...  
;{ 
ISR	CODE	H'20'
ISR				    ; interrupt for config mode
	bcf	CCP1CON,6	    ; turn off PWM output
	movfw	B'00000000'	    
	movwf	CCPR1L		    ; clear the MSBs for duty cycle 
	movfw	modefinal	   
	sublw	B'00000000'
	btfsc	STATUS,Z	    ; check which mode
	call	v_pwm_config	    ; if pwm, call pwm_config
	movfw	modefinal
	sublw	B'00000010'
	btfsc	STATUS,Z	    ; check which mode
	call	strobe_config	    ; if strobe, call strobe_config
	movfw	modefinal
	sublw	B'00000011'
	btfsc	STATUS,Z	    ; check which mode
	call	lfsr_config	    ; if lfsr, call lfsr_config
	
	bcf	INTCON,1	    ; reset interrupt flag
	bsf	CCP1CON,6	    ; turn on PWM output 
	
	retfie		; replace retfie with your ISR if necessary
;------------------------------------------------------------------------------------------
;------> LUT TABLES - for config <-------
;------------------------------------------------------------------------------------------
LUT1	addwf	PCL,F		; PR2 value for config selection
	retlw	b'11111111'	; less freq
	retlw	b'01111111'
	retlw	b'00111111'
	retlw	b'00011111'	; more freq
	
LUT2	addwf	PCL,F		; store brightness of LD1 and LD2 for config selection
	retlw	b'00000001'
	retlw	b'00000011'
	retlw	b'00111111'
	retlw	b'11111111'

LUT3	addwf	PCL,F		; store speed of strobe/lfsr for config selection
	retlw	b'00011111'
	retlw	b'00111111'
	retlw	b'01111111'
	retlw	b'11111111'
;------------------------------------------------------------------------------------------
;------> MODE AQUISITION <-------
;------------------------------------------------------------------------------------------
mode_select			; ADC aquisition
	BANKSEL ADCON0	
	MOVLW   B'11000001'	;ADC Frc clock,
	MOVWF   ADCON0		;AN0, On	

	CALL    Del_ms		;Acquisiton delay

	BSF	ADCON0,GO	;Start conversion
	BTFSC   ADCON0,GO	;Is conversion done'
	GOTO    $-1		;No, test again

	BANKSEL ADRESH	
	MOVF    ADRESH,W	;Read upper 8 bits
	ANDLW	B'11000000'	;get the 2 MSB
	MOVWF   modestart
	bcf	STATUS, C	; clear carry flag as it is used for rotation
	rrf	modestart,f
	rrf	modestart,f
	rrf	modestart,f
	rrf	modestart,f
	rrf	modestart,f
	rrf	modestart,w	; need to rotate 2 MSBs to 2 LSBs
	movwf	modefinal
	return
;------------------------------------------------------------------------------------------
;------> CONFIG ROUTINES <-------
;------------------------------------------------------------------------------------------
v_pwm_config			; v_pwm configuration
	bsf	LEDs,LD6	; to show 1st config option
	call	SelectB		; user input
	movwf	config11	; store user input
	bcf	LEDs,LD6	
	bsf	LEDs,LD7	; to show 2nd config option
	call	SelectB		; user input
	movwf	config12	; store user input
	bsf	LEDs,LD6
	bsf	LEDs,LD7	; to show 3rd config option
	call	SelectB		; user input
	movwf	config13	; store user input
	bcf	LEDs,LD6
	bcf	LEDs,LD7	; end so clear config counter
	return 
strobe_config
	bsf	LEDs,LD6	; to show 1st config option
	call	SelectB		; user input
	movwf	config21	; store user input
	bcf	LEDs,LD6
	bsf	LEDs,LD7	; to show 2nd config option
	call	SelectB		; user input
	movwf	config22	; store user input
	bcf	LEDs,LD7	; end so clear config counter
	return
lfsr_config
	clrf	LEDs		; reset outputs from previous mode
	bsf	LEDs,LD6	; to show 1st config option
	call	SelectB		; user input
	movwf	config31	; store user input
	return			
;------------------------------------------------------------------------------------------
;------> PWM MODE <-------
;------------------------------------------------------------------------------------------
v_pwm	movlw	B'0000000'
	movwf	LEDs		; clear LEDs
	movfw	config11	   
	call	LUT1
	banksel	PR2
	movwf	PR2		; set PWM period
	
	banksel CCP1CON
	movlw   b'01001100'	
	movwf   CCP1CON		; 01 is full bridge output, xx LSBs, 1100 PWM mode active high
	
	movfw	config12
	call	LUT2
	movwf	dc		; set 1st duty cycle
	
	movfw	config13
	call	LUT2
	movwf	otherdc		; set 2nd duty cycle
	
	movfw	dc
	subwf	otherdc,w	; wreg = dc - otherdc
	btfss	STATUS,C
	goto	dcbigger	; checks for bigger variable, goes to respective loop to set variables 
otherdcbigger			
	movf	otherdc,w	
	movwf	bigdc		; store bigger value in this variable
	movf	dc,w
	movwf	smalldc		; store smaller value in this variable
	goto	v_pwmloop	; start pwm loop
dcbigger
	movfw	dc
	movwf	bigdc		; store bigger value in this variable
	movfw	otherdc
	movwf	smalldc		; store smaller value in this variable
v_pwmloop			; start pwm loop
	movfw	bigdc
	movwf	pwmc
	movlw	B'00100111'
	banksel	CCPR1L		
	movwf	CCPR1L		; input inital duty cycle
decrdc	call	Del_ms		; decr duty cycle and loop till equal to smaller duty cycle
	movfw   pwmc
	movwf   CCPR1L
	decf    pwmc
	movfw   smalldc
	subwf   pwmc,w
	btfss   STATUS,Z	; check if current duty cycle same as smaller duty cycle
	goto    decrdc		; if not same redo loop, if same go to incrdc loop
incrdc	call	Del_ms		; incr duty cycle and loop till equal to first duty cycle
	movfw   pwmc
	movwf   CCPR1L
	incf    pwmc
	movfw   pwmc
	subwf   bigdc,w		
	btfss   STATUS,Z	; check if current duty cycle same as bigger duty cycle
	goto    incrdc		; if not same redo loop, if same exit loop
	
	return
;------------------------------------------------------------------------------------------
;------> STROBE MODE <-------
;------------------------------------------------------------------------------------------
strobe	bcf	CCP1CON,6	; turn off PWM output
foward_loop
	movfw	B'00000000'
	movwf	CCPR1L		; clear the MSBs for duty cycle 
	movfw	config21
	call	LUT3
	movwf	strobedelsize	; set speed of strobe
delay_loop			
	decf    strobedelsize
	call    Del_ms
	call	Del_ms
	movfw   strobedelsize
	sublw   b'00000000'
	btfss   STATUS,Z	; checks if delay value at 0
	goto    delay_loop	; if not 0 redo loop, if 0 exit
	
	movfw	strobe_tracker	
	movwf   LEDs		; output strobe_tracker to LEDs
	
	call	mode_select	; check if mode has changed   
	sublw	B'00000010'	
	btfss	STATUS,Z	; if mode hasnt changed continue with code
	goto	strobe_end	; if mode changed go to end loop for strobe
	
	rlf	strobe_tracker,f    ; move LED to next position
	bcf	strobe_tracker,0    ; clear new shifted bit of LEDs incase its 1
	
	movfw   strobe_tracker	 
	sublw   b'100000000'	
	btfss   STATUS,Z	; check if end of foward loop
	goto    foward_loop	; if end of foward loop go to next section, if not end redo loop 
	
	movfw	config22	; check if foward or reverse mode selected
	sublw	B'01'		; 
	btfss	STATUS,Z	; if reverse mode selected, go to reverse loop
	goto	foward_return	; if not reverse mode selected, go to foward loop
	
	movlw	B'10000000'	
	movwf	strobe_tracker	; fix strobe positioning
	
reverse_loop
	movfw	config21
	call	LUT3
	movwf	strobedelsize	; set speed of strobe
	movfw	strobe_tracker
	movwf   LEDs		; update LED position
	
	call	mode_select	; check if mode has changed 
	sublw	B'00000010'
	btfss	STATUS,Z	; if mode hasnt changed continue with code
	goto	strobe_end	; if mode changed go to end loop for strobe
	
	rrf	strobe_tracker,f    ; move LED to next position
	bcf	strobe_tracker,7    ; clear new shifted bit of LEDs incase its 1
delay_loop2
	decf    strobedelsize	; 
	call    Del_ms
	call	Del_ms
	movfw   strobedelsize
	sublw   b'00000000'
	btfss   STATUS,Z	; checks if delay value at 0
	goto    delay_loop2	; if not 0 redo loop, if 0 exit
	
	movfw   strobe_tracker
	sublw   b'00000001'	
	btfss   STATUS,Z	; check if end of reverse loop 
	goto    reverse_loop	; if end go to strobe_end, if not end go to reverse_loop
strobe_end	
	bsf	CCP1CON,6	; turn off PWM output
	return
foward_return	
	movlw	B'00000001'	
	movwf	strobe_tracker	; reset LED position for new cycle
	return
;------------------------------------------------------------------------------------------
;------> LFSR MODE <-------
;------------------------------------------------------------------------------------------
lfsr	bcf	CCP1CON,6	; turn off PWM output
	movfw	B'00000000'
	movwf	CCPR1L		; clear the MSBs for duty cycle 
	movfw	config31
	call	LUT3
	movwf	lfsrdelsize	; set speed of lfsr
delay_loop3
	decf    lfsrdelsize
	call    Del_ms
	movfw   lfsrdelsize
	sublw   b'00000000'
	btfss   STATUS,Z	; checks if delay value at 0
	goto    delay_loop3	; if not 0 redo loop, if 0 exit
	
	movfw	lfsr_tracker
	movwf	LEDs		; update LEDs with new random number
	movfw   lfsr_tracker	; Move the value of 'lfsr' into the W register
	andlw   0x01		; Perform a bitwise AND operation with the literal value 0x01
	movwf   lsb		; Move the result of the AND operation into 'lsb'
	
	rrf	lfsr_tracker	; start of the next random number calculation
	btfss	lsb,0
	goto	lfsr
	movfw	lfsr_tracker
	xorlw	B'10001100'
	movwf	lfsr_tracker	; end of next random number calculation
	
	bsf	CCP1CON,6	; turn on PWM output
	return
;} end of your subroutines


; Provided code - do not edit  
Main	nop
#include ECH_INIT.inc

; Place your INITIALISATION code (if any) here ...   
;{ ***		***************************************************
; e.g.,		movwf	Ctr1 ; etc
;------------------------------------------------------------------------------------------
;   mode_config initialisation - to enter configuration mode
;------------------------------------------------------------------------------------------
    banksel OPTION_REG
    movlw   b'01000000'
    movwf   OPTION_REG	; Set interrupt edge select to rising edge (OPTION_REG<INTEDG> = 0)

    banksel INTCON
    bsf	    INTCON,GIE	; configure global interrupt enable (INTCON<GIE> = bit 7)
    bsf	    INTCON,INTE	; enable external interrupt 0 (INTE) (INTCON<INTE> = bit 4)
    
    banksel TRISD	
    bcf	    TRISD,7	; make P1D output pin using TRISD-RD7
    
    banksel T2CON	    ; set TMR2 prescale value and enable timer2
    movlw   b'00000100'	    ; bit 2 = timer on/off, bit 1-0 = prescaler
    movwf   T2CON
    
    movlw   b'00000001'	    ; initial config values for each mode
    movwf   config11
    
    movlw   b'00000001'
    movwf   config12
    
    movlw   b'00000010'
    movwf   config13
    
    movlw   b'00000001'
    movwf   config21
    
    movlw   b'00000001'
    movwf   config22
    
    movlw   b'00000000'
    movwf   config31
    
    movlw   B'10101100'	    ; move 172, but can be any non zero value
    movwf   lfsr_tracker
   
    movlw   B'00000001'	    ; start or strobe LED
    movwf   strobe_tracker
    
;} 
; end of your initialisation

MLoop	nop

; place your superloop code here ...  
;{    
    call    mode_select
    
    movfw   modefinal
    sublw   b'00000000'
    btfsc   STATUS,Z	; check result of adc
    call    v_pwm	; call if mode selected
     
    movfw   modefinal
    sublw   b'00000010'
    btfsc   STATUS,Z	; check result of adc
    call    strobe	; call if mode selected
    
    movfw   modefinal
    sublw   b'00000011'
    btfsc   STATUS,Z	; check result of adc 
    call    lfsr	; call if mode selected 
    
;}	
; end of your superloop code

    goto	MLoop

end
