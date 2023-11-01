; This assembly program implements a digital thermometer using the LM45 thermistor on the Dragon12 board. The temperature in Celsius is displayed on the LCD screen.
; The ATD result is multiplied with 3/2 and then added 4 for converting to degree Celsius.  
; (X°C × 9/5) + 32 = Y°F
; X°C + 273 = Y(K)
; add subroutine to convert to farenheit and kelvin
; add subroutine to read the status of pushbutton
; 

#include "reg9s12.h"
R1		equ	$1001
R2		equ	$1002
R3		equ	$1003
lcd_dat 	equ	PortK	; LCD data pins (PK5~PK2)
lcd_dir 	equ	DDRK	; LCD data direction port
lcd_E   	equ	$02		; E signal pin
lcd_RS  	equ	$01		; RS signal pin
dot		equ	$2E		; ASCII code for decimal point 
degreeSymbol	equ	223	; ASCII code for degree symbol
letterC		equ	$43
letterF		equ	$46
letterK		equ	$4B

		org	$1000
quotient	ds.b	1
remainder	ds.b	1
sign		ds.b	1
fractionalPart	ds.b	1
tempDisplay	ds.b	8	; output string for LCD
counter		rmb	1	; counter for shifting display
unitletter	rmb	1	; stores the correct letter to display
switches	rmb	1	; stores if switch pressed or not, 1=C, 2=F, 3=K, -1=no button pressed

msg1 	dc.b   	"Temperature:",0
msg2 	dc.b  	"Value:        ",0
msg3	dc.b	"   Christopher Szynwald",0
msg4	dc.b	"   DIP1:C, DIP2:F, DIP3:K",0

	org	$2000	 	
	lds	#$2000 	
	jsr   	openLCD ; initialize the LCD
	ldx   	#msg3	; point to the first line of message
	jsr   	putsLCD	; display in the LCD screen
	ldaa	#$C0	; move to the second row 
	jsr	cmd2LCD	;	"
	ldx   	#msg4	; point to the second line of message
	jsr   	putsLCD

; here add instructions to shift the display to see the whole sentence,
	ldaa 	#255	;initialize counter
	staa	counter	;
	ldaa	#-1	;initialize button variable
	staa	switches
	movb	#$00, DDRH	; set port H as input for pushbuttons

;STARTUP SEQUENCE
startup	ldaa	#$18		;display name and instructions during startup sequence
	jsr	cmd2LCD
	jsr	lcddelay
	dec	counter
	jsr	pressed
	
	ldaa	#0	;checking value of switch
	cmpa	switches	;compare switch value to 0
	bge	startup	;if switch<0 (not pressed yet) continue startup sequence
	bra	begin	;else, switch was pressed (SW3, SW4 or SW5)

;BUTTON PRESSED SUBROUTINE
pressed	brset	PTH,$20,kelvin	;check value of portH pins
	brset	PTH,$40,farenheit	
	brset	PTH,$80,celsius
	bra	done		;if none pressed, switches value remains the same

kelvin	ldaa	#letterK		;for kelvin, switch = 3
	staa	unitletter
	ldaa	#3
	staa	switches
	bra	done

farenheit	ldaa	#letterF		;for farenheit, switch = 2
		staa	unitletter
		ldaa	#2
		staa	switches
		bra	done

celsius	ldaa	#letterC		;for celsius, switch = 1
	staa	unitletter
	ldaa	#1
	staa	switches
	bra	done
done	rts


;BEGIN ADT CONVERSION
begin	movb	#$80,ATD0CTL2	; enable input for A/D conversion
	movb	#$08,ATD0CTL3	; 8-bit resolution, 16-clock for 2nd stage
	movb	#$EB,ATD0CTL4	; conversion frequency = 1MHz  
	bra	H1

H1 	movb	#$05,ATD0CTL5	; chan 5 of ATD0 (use left-justified) 
H2	brclr	ATD0STAT,$80,H2	;	
	
; display on LCD continuously 

;ORDER OF SUBROUTINE CALLS
	jsr	lcdLoop	 		 
	jsr	openAD0
	jsr	pressed	 
	bra	H1		; keep reading the ADC

openAD0 movb	#$E0,ATD0CTL2	; normal ATD operation
	jsr	delay50us 	; wait for 50 us
	movb	#$08,ATD0CTL3	; conversion, then freeze
	movb	#$EB,ATD0CTL4	; 8-bit resolution, 1MHz clock rate
	rts
   
lcdLoop	jsr   	openLCD 	; initialize the LCD
	ldx   	#msg1		; point to the first line of message
	jsr   	putsLCD		; display in the LCD screen
	ldaa	#$C0		; move to the second row 
	jsr	cmd2LCD		;	"
	ldx   	#msg2		; point to the second line of message
	jsr   	putsLCD
	jsr	storeValue
	rts

storeValue	movb	#$20,tempDisplay	; initialize the buffer contents to "  0.0'C" 	 	
		movb	#$20,tempDisplay+1	; " "
		movb	#$30,tempDisplay+2 	; "0"	
		movb	#dot,tempDisplay+3	; "." 	
		movb	#$30,tempDisplay+4 	; "0"	
		movb	#degreeSymbol,tempDisplay+5	; degree character
		movb	unitletter,tempDisplay+6	; letter 'C' 
		movb	#0,tempDisplay+7		; null character
		movb	#$05,ATD0CTL5		; start an ATD conversion sequence
		movb	#0,sign			; initialize sign to positive
		movb	#$30,fractionalPart	; initialize fractional digit to 0
		brclr	ATD0STAT,$80,*		; wait for the conversion to complete
		ldaa	switches		; check status of switches
		cmpa	#1			; 
		beq	c_convert
		cmpa	#2
		beq	f_convert
		cmpa	#3
		beq	k_convert

;CONVERT INTO PROPER UNITS
c_convert	ldd	ADR00H		; read a conversion result
		ldy	#2		; compute result x 3 / 2
		emul			; multiply Y x D
		ldx	#3		; divide by 2
		ediv

		stab	remainder	; save the remainder
		tfr	Y,D		; transfer quotient to A
		adda	#4		; add temperature offset
		bra	here		 

f_convert	ldd	ADR00H
		ldy	#18		; compute result, (2/3) x (9/5) + 32
		emul			; multiply Y x D
		ldx	#15		; divide by 10
		ediv		
		
		lsrd
		stab	remainder	; save the remainder
		tfr	Y,D		; transfer quotient to A
		adda	#38		; add temperature offset (4*(9/5)+32)
		bra	here		 

k_convert	ldd	ADR00H
		ldy	#2
		emul
		ldx	#3
		ediv
			
		stab	remainder	;
		tfr	Y,D		;shift quotient into lower 4 bits in order to fit format
		staa	quotient		;
		ldab	quotient		;
		clra
		addd	#277		;then add temperature offset (273+4)
		bra	here2		;branch directly to ASCII conversion 

;DISPLAYING TEMP 
here		staa	quotient		;conversion sequence for celsius and farenheit
		ldab	quotient
		clra

here2		ldx	#10		; use repeated divide by 10 to separate
		idiv			; integral digits
		addb	#$30
		stab	tempDisplay+2		; save the one's digit
		tfr	X,D		; transfer quotient to D
		tstb			; is quo zero?
		beq	add_fra		; if integral part is 0, then add fraction digit
		ldx	#10		; separate the ten's digit
		idiv
		addb	#$30		; convert and store the ten's digit
		stab	tempDisplay+1
		tfr	X,D		; test hundred's digit
		tstb			; is quotient 0?
		beq	add_fra		; if yes, branch to add_fra
		ldx	#10		; otherwise, seperate the hundred's digit
		idiv
		addb	#$30		; convert and store the hundred's digit
		stab	tempDisplay	; hundreds digit is the largest digit possible for any unit given
		bra	add_fra

;ADD FRACTIONAL PART
add_fra 	ldaa	remainder
		adda	#$30
		staa	fractionalPart
		movb	fractionalPart,tempDisplay+4	; insert fraction digit
		ldaa	sign		; check the sign
		beq	out_it
		movb	#$2D,tempDisplay	; when minus, add minus character
out_it		ldaa	#$C0		; set cursor to 2nd row
		jsr	cmd2LCD
		ldx	#msg2		; clear the 2nd row of the LCD	
		jsr	putsLCD
		ldaa	#$C7		; set LCD cursor position
		jsr	cmd2LCD		;
		ldx	#tempDisplay		; output the temperature string		
		jsr	putsLCD		;
		jsr	delay50us 
		rts


;LCD SCREEN SUBROUTINES
; the command is contained in A when calling this subroutine from main program
cmd2LCD		psha			; save the command in stack
		bclr  	lcd_dat, lcd_RS	; set RS=0 for IR => PTK0=0
		bset  	lcd_dat, lcd_E 	; set E=1 => PTK=1
		anda  	#$F0    	; clear the lower 4 bits of the command
		lsra 			; shift the upper 4 bits to PTK5-2 to the 
		lsra            	; LCD data pins
		oraa  	#$02  		; maintain RS=0 & E=1 after LSRA
		staa  	lcd_dat 	; send the content of PTK to IR 
		nop			; delay for signal stability
		nop			; 	
		nop			;	
		bclr  	lcd_dat,lcd_E   ; set E=0 to complete the transfer

		pula			; retrieve the LCD command from stack
		anda  	#$0F    	; clear the lower four bits of the command
		lsla            	; shift the lower 4 bits to PTK5-2 to the
		lsla            	; LCD data pins
		bset  	lcd_dat, lcd_E 	; set E=1 => PTK=1
		oraa  	#$02  		; maintain E=1 to PTK1 after LSLA
		staa  	lcd_dat 	; send the content of PTK to IR
		nop			; delay for signal stability
		nop			;	
		nop			;	
		bclr  	lcd_dat,lcd_E	; set E=0 to complete the transfer

		ldy	#1		; adding this delay will complete the internal
		jsr	delay50us	; operation for most instructions
		rts

openLCD 	movb	#$FF,lcd_dir	; configure Port K for output
		ldy   	#2		; wait for LCD to be ready
		jsr   	delay100ms	;      "
		ldaa  	#$28            ; set 4-bit data, 2-line display
		jsr   	cmd2lcd         ;       "	
		ldaa  	#$0F            ; turn on display, cursor, and blinking
		jsr   	cmd2lcd         ;       "
		ldaa  	#$06            ; move cursor right (entry mode set instruction)
		jsr   	cmd2lcd         ;       "
		ldaa  	#$01            ; clear display screen and return to home position
		jsr   	cmd2lcd         ;       "
		ldy   	#2              ; wait until clear display command is complete
		jsr   	delay1ms   	;       "
		rts 	

; The character to be output is in accumulator A.
putcLCD		psha                    ; save a copy of the chasracter
		bset  	lcd_dat,lcd_RS	; set RS=1 for data register => PK0=1
		bset  	lcd_dat,lcd_E  	; set E=1 => PTK=1
		anda  	#$F0            ; clear the lower 4 bits of the character
		lsra           		; shift the upper 4 bits to PTK5-2 to the
		lsra            	; LCD data pins
		oraa  	#$03            ; maintain RS=1 & E=1 after LSRA
		staa  	lcd_dat        	; send the content of PTK to DR
		nop                     ; delay for signal stability
		nop                     ;      
		nop                     ;     
		bclr  	lcd_dat,lcd_E   ; set E=0 to complete the transfer

		pula			; retrieve the character from the stack
		anda  	#$0F    	; clear the upper 4 bits of the character
		lsla            	; shift the lower 4 bits to PTK5-2 to the
		lsla            	; LCD data pins
		bset  	lcd_dat,lcd_E   ; set E=1 => PTK=1
		oraa  	#$03            ; maintain RS=1 & E=1 after LSLA
		staa  	lcd_dat		; send the content of PTK to DR
		nop			; delay for signal stability
		nop			;
		nop			;
		bclr  	lcd_dat,lcd_E   ; set E=0 to complete the transfer

		ldy	#1		; wait until the write operation is complete
		jsr	delay50us	; 
		rts

putsLCD		ldaa  	1,X+   		; get one character from the string
		beq   	donePS	; reach NULL character?
		jsr   	putcLCD
		bra   	putsLCD
donePS		rts 


;DELAY SUBROUTINES 
delay1ms 	movb	#$90,TSCR	; enable TCNT & fast flags clear
		movb	#$06,TMSK2 	; configure prescale factor to 64
		bset	TIOS,$01		; enable OC0
		ldd 	TCNT
again0		addd	#375		; start an output compare operation
		std	TC0		; with 50 ms time delay
wait_lp0	brclr	TFLG1,$01,wait_lp0
		ldd	TC0
		dbne	y,again0
		rts

delay100ms 	movb	#$90,TSCR	; enable TCNT & fast flags clear
		movb	#$06,TMSK2 	; configure prescale factor to 64
		bset	TIOS,$01	; enable OC0
		ldd 	TCNT
again1		addd	#37500		; start an output compare operation
		std	TC0		; with 50 ms time delay
wait_lp1	brclr	TFLG1,$01,wait_lp1
		ldd	TC0
		dbne	y,again1
		rts

delay50us 	movb	#$90,TSCR	; enable TCNT & fast flags clear
		movb	#$06,TMSK2 	; configure prescale factor to 64
		bset	TIOS,$01	; enable OC0
		ldd 	TCNT
again2		addd	#15		; start an output compare operation
		std	TC0		; with 50 ms time delay
wait_lp2	brclr	TFLG1,$01,wait_lp2
		ldd	TC0
		dbne	y,again2
		rts


lcddelay	ldab	#$40	;	
delay1		ldx	#$FFFF	;	
delay2		dbne	x,delay2;	
		dbne	b,delay1;	
		rts
  
		end