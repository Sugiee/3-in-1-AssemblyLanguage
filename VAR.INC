; list names of your variables one in a row below
; each of them will be allocated a GPR
; and can be referenced by name in your code
; (the assembler will place actual address automatically)
; examples:
;Ctr1	RES	1	; your vaiable, e.g., for counter 1
;Ctr2	RES	1
;------------------------------------------------------------------------------------------
;   mode and config
;------------------------------------------------------------------------------------------
config11    RES	1
config12    RES	1
config13    RES	1
config21    RES	1
config22    RES	1
config31    RES	1
modefinal   RES	1
modestart   RES	1
;------------------------------------------------------------------------------------------
;   v_pwm
;------------------------------------------------------------------------------------------
dc	    RES	1
otherdc	    RES	1
smalldc	    RES	1
bigdc	    RES	1
pwmc	    RES	1
;------------------------------------------------------------------------------------------
;   strobe
;------------------------------------------------------------------------------------------
strobe_tracker	RES	1
strobedelsize	RES	1
strobe_start	RES	1
strobe_last	RES	1
;------------------------------------------------------------------------------------------
;   lfsr
;------------------------------------------------------------------------------------------
start_state	RES	1
lfsr_tracker	RES	1
lsb		RES	1
lfsrdelsize	RES	1
temp		RES	1