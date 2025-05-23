***************
* MC3 monitor 1.4.2
* Daniel Tufvesson 2013-2017
* Modified for EVA1 Display Adapter by Robert Offner 2025
*
***************
* DEFINITION OF INTERNAL CPU REGISTERS
PIA1DIR	EQU	$00
PIA1DAT	EQU	$02
PIA2DIR	EQU	$01
PIA2DAT	EQU	$03
TIMECON EQU	$08	TIMER CONTROL AND STAUS REG
COUNTHI EQU     $09	COUNTER HIGH
COUNTLW EQU     $0A	COUNTER LOW
OCPRGHI EQU     $0B	OUTPUT COMPARE REG. HIGH BYTE
OCPRGLW EQU     $0C	OUTPUT COMPARE REG. LOW BYTE
ICPRGHI EQU     $0D	INPUT COMPARE REG. HIGH BYTE
ICPRGLW EQU     $0E	INPUT COMPARE REG. LOW BYTE
ACIAMOD EQU     $10     RATE AND MODE CONTROL REGISTER
ACIASTA EQU     $11     TXD/RXD CONTROL AND STATUS REG.
ACIARXD EQU     $12     RECEIVE DATA REGISTER
ACIATXD EQU     $13     TRANSMIT DATA REGISTER
RAMCONT EQU     $14     RAM CONTROL REGISTER
M6845_0 EQU	$4000
M6845_1 EQU	$4001

***************
* DEFINITION OF VARIABELS (16 Bytes)
*	ORG     $00D5
	ORG     $00DC
*	ORG     $1FD5
*	ORG     $BFD5
STACK   RMB     1       STACK POINTER
        RMB     1       CONDITIONS CODES
        RMB     1       B-ACC
        RMB     1       A-ACC
        RMB     1       X-HIGH
        RMB     1       X-LOW
        RMB     1       P-HIGH
        RMB     1       P-LOW
SP      RMB     2       STACK POINTER
CKSM	RMB	1	CHECKSUM
TEMP	RMB	1	TEMP
XHI	RMB	1	X-TEMP HIGH
XLOW	RMB	1	X-TEMP LOW
XTEMP	RMB	2	X-TEMP

***************
* DEFINITION OF VECTORS (27 Bytes)
* 3 BYTES JMP+ADDR
*	ORG     $00E5 ; does not work, needs to be above $100
*	ORG     $1FE5
	ORG     $BFE5
CONSVEC	RMB	3	CONSOLE STATUS VECTOR
CONOVEC	RMB	3	CONSOLE OUTPUT VECTOR
CONIVEC	RMB	3	CONSOLE INPUT VECTOR
TMOFVEC	RMB	3       TIMER OVER FLOW INTERRUPT VECTOR
TMOCVEC	RMB	3	TIMER OUTPUT COMPARE INTERRUPT VECTOR
TMICVEC	RMB	3       TIMER INPUT CAPTURE INTERRUPT VECTOR
IRQVEC	RMB	3       IRQ INTERRUPT VECTOR
SWIVEC	RMB	3       SWI INTERRUPT VECTOR
NMIVEC	RMB	3	NMI INTERRUPT VECTOR

***************
* ROM BEGIN HERE
	ORG	$E000
* JUMPTABLE
	JMP	RETURN
	JMP 	CONOVEC	'OUTCHAR'
	JMP	INCHAR
	JMP	PDATA
	JMP	OUTHR
	JMP	OUTHL
	JMP	OUT2HS
	JMP	OUT4HS
	JMP	INHEX
	JMP	INBYTE
	JMP	BADDR
	JMP	PCRLF
	JMP	OUTS
	JMP	IORD
	JMP	IOWR

* STRINGS
CRLFTX	FCB	$0D,$0A,$04
PROMPTX	FCB	$0D,$0A
	FCC	"> "
	FCB	$04
HELPTX	FCB	$0D,$0A,$0A
	FCC	"MC3 monitor 1.4.2"
	FCB	$0D,$0A
	FCC	"Daniel Tufvesson 2013-2017"
	FCB	$0D,$0A,$0D,$0A
	FCC	" G  Go (RTI)"
	FCB	$0D,$0A
	FCC	" JM  Jump to address"
	FCB	$0D,$0A
	FCC	" JS  Jump to subroutine"
	FCB	$0D,$0A
	FCC	" L  Load S19 from console"
	FCB	$0D,$0A
	FCC	" MC Memory change"
	FCB	$0D,$0A
	FCC	" MD Memory dump"
	FCB	$0D,$0A
	FCC	" MF Memory fill"
	FCB	$0D,$0A
	FCC	" MW Memory write"
	FCB	$0D,$0A
	FCC	" RR Print contents of stack"
	FCB	$0D,$0A
	FCC	" RC Change stack CC"
	FCB	$0D,$0A
	FCC	" RA Change stack A"
	FCB	$0D,$0A
	FCC	" RB Change stack B"
	FCB	$0D,$0A
	FCC	" RX Change stack X"
	FCB	$0D,$0A
	FCC	" RP Change stack PC"
	FCB	$0D,$0A
	FCC	" RS Change stack pointer"
	FCB	$0D,$0A
	FCC	" RM Reset stack pointer"
	FCB	$0D,$0A
	FCC	" P  Select I/O page"
	FCB	$0D,$0A
	FCC	" T  Single step"
	FCB	$0D,$0A
	FCC	" X  Enter extended ROM"
	FCB	$0D,$0A
	FCC	" D3  Setup 6845 for 32 Characters"
	FCB	$0D,$0A
	FCC	" D4  Setup 6845 for 40 Characters"
	FCB	$0D,$0A
	FCC	" D7  Setup 6845 for 72 Characters"
	FCB	$0D,$0A
	FCC	" D8  Setup 6845 for 80 Characters"
	FCB	$0D,$0A
	FCC	" Sx  Set Bit x of Port B"
	FCB	$0D,$0A
	FCC	" Cx  Clear Bit x of Port B"
	FCB	$0D,$0A,$04
REGTX	FCB     $0D,$0A
	FCC     "CC B  A  X    PC   SP     H I N Z V C"
	FCB     $0D,$0A,$04
DUMPTX	FCB     $0D,$0A
	FCC     "ADDR  0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F"
	FCB	$0D,$0A,$04
LDTX	FCB     $0D,$0A
	FCC     "Load S19 record"
	FCB     $0D,$0A,$04
LFAILTX	FCB     $0D,$0A,$0A
	FCC     "Load fail - Press Y to continue"
	FCB     $0D,$0A,$04
LDOKTX	FCB     $0D,$0A
	FCC     "Load OK"
	FCB     $0D,$0A,$04
STOPTX	FCB     $0D,$0A
	FCC     "STOP"
	FCB	$04
TRAPTX	FCB     $0D,$0A
	FCC     "TRAP"
	FCB	$04
DSP3TX	FCB     $0D,$0A
	FCC     "Init 32 Char"
	FCB	$04
DSP4TX	FCB     $0D,$0A
	FCC     "Init 40 Char"
	FCB	$04
DSP7TX	FCB     $0D,$0A
	FCC     "Init 72 Char"
	FCB	$04
DSP8TX	FCB     $0D,$0A
	FCC     "Init 80 Char"
	FCB	$04

***********
* START FROM RESET
RESET	LDS	#STACK	INIT STACK POINTER
	STS	SP
	JSR	INITVEC	INIT VECTORS
	JSR	SCIINIT	INIT INTERNAL ACIA
	CLRB	#$00
	LDAA	#$FF
	STAA	PIA1DIR	
	STAB	PIA2DIR
	LDAA	#$0F	Clear INVERT, TXTON_OFF, GTEXT, TEXT (P1.0-P1.3), Set CHARSET, T/G, HIGHRES (P1.4-P1.7)
	STAA	PIA1DAT
	STAB	PIA2DAT
*	LDAA	#$00
*	STAA	RAMCONT	DISABLE CPU INTERNAL RAM ### OFFR: Modified for Unicomp2
	LDAA	#$C0
	STAA	RAMCONT	ENABLE CPU INTERNAL RAM
PROMPT	LDX	#PROMPTX
	JSR	PDATA
	JSR	INCHAR
	CMPA	#$0D
	BEQ	PROMPT
	ANDA	#$DF	CONVERT TO UPPER CASE
	CMPA	#'H
	BNE	*+5
	JMP	HELP	PRINT HELP
	CMPA	#'G
	BNE	*+5
	JMP	GO	GOTO USER PROGRAM
	CMPA	#'J
	BNE	*+5
	JMP	MENU_J	Jump Commands
	CMPA	#'R
	BNE	*+5
	JMP	MENU_R	REG/STACK CMDS
	CMPA	#'M
	BNE	*+5
	JMP	MENU_M	MEMORY CMDS
	CMPA	#'D
	BNE	*+5
	JMP	MENU_D	Display Adapter 6845
	CMPA	#'L
	BNE	*+5
	JMP	LOAD	LOAD S19
	CMPA	#'P
	BNE	*+5
	JMP	PAGE	SELECT I/O PAGE
	CMPA	#'C
	BNE	*+5
	JMP	MENU_C	Reset Bits of Port B
	CMPA	#'S
	BNE	*+5
	JMP	MENU_S	Set Bits of Port B
	CMPA	#'T
	BNE	*+5
	JMP	SSTEP	SINGLE STEP
	CMPA	#'X
	BNE	*+5
	JMP	EXROM	JUMP TO EXTENDED ROM
	LDAA	#'?
	JSR	OUTCHAR
	BRA	PROMPT

MENU_J	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	ANDA	#$DF	CONVERT TO UPPER CASE
	CMPA	#'M     
	BNE	*+5
	JMP	JUMP	JUMP TO USER PROGRAM
	CMPA	#'S     
	BNE	*+5
	JMP	JSUB	JSR TO USER PROGRAM
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_M	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	ANDA	#$DF	CONVERT TO UPPER CASE
	CMPA	#'C
	BNE	*+5
	JMP	CHANGE	MEMORY CHANGE
	CMPA	#'W
	BNE	*+5
	JMP	MEMWRT	MEMORY WRITE
	CMPA	#'F
	BNE	*+5
	JMP	MFILL	MEMORY FILL
	CMPA	#'D
	BNE	*+5
	JMP	DUMP	MEMORY DUMP
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_S	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	CMPA	#'0
	BNE	*+5
	JMP	SET_BIT_0
	CMPA	#'1
	BNE	*+5
	JMP	SET_BIT_1
	CMPA	#'2
	BNE	*+5
	JMP	SET_BIT_2
	CMPA	#'3
	BNE	*+5
	JMP	SET_BIT_3
	CMPA	#'4
	BNE	*+5
	JMP	SET_BIT_4
	CMPA	#'5
	BNE	*+5
	JMP	SET_BIT_5
	CMPA	#'6
	BNE	*+5
	JMP	SET_BIT_6
	CMPA	#'7
	BNE	*+5
	JMP	SET_BIT_7
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_C	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	CMPA	#'0
	BNE	*+5
	JMP	CLR_BIT_0
	CMPA	#'1
	BNE	*+5
	JMP	CLR_BIT_1
	CMPA	#'2
	BNE	*+5
	JMP	CLR_BIT_2
	CMPA	#'3
	BNE	*+5
	JMP	CLR_BIT_3
	CMPA	#'4
	BNE	*+5
	JMP	CLR_BIT_4
	CMPA	#'5
	BNE	*+5
	JMP	CLR_BIT_5
	CMPA	#'6
	BNE	*+5
	JMP	CLR_BIT_6
	CMPA	#'7
	BNE	*+5
	JMP	CLR_BIT_7
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_D	JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
*	ANDA	#$DF	CONVERT TO UPPER CASE
	CMPA	#'3
	BNE	*+5
	JMP	SETUP3
	CMPA	#'4
	BNE	*+5
	JMP	SETUP4
	CMPA	#'7
	BNE	*+5
	JMP	SETUP7
	CMPA	#'8
	BNE	*+5
	JMP	SETUP8
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

MENU_R  JSR	INCHAR
	CMPA	#$0D
	BNE	*+5
	JMP	PROMPT
	ANDA	#$DF
	CMPA	#'R
	BNE	*+5
	JMP	PRTREG	REGISTER PRINT
	CMPA	#'A
	BNE	*+5
	JMP	REGACH	REGISTER A CHANGE
	CMPA	#'B
	BNE	*+5
	JMP	REGBCH	REGISTER B CHANGE
	CMPA	#'X
	BNE	*+5
	JMP	REGXCH	REGISTER X CHANGE
	CMPA	#'P
	BNE	*+5
	JMP	REGPCH	REGISTER PC CHANGE
	CMPA	#'C
	BNE	*+5
	JMP	REGCCH	REGISTER CC CHANGE
	CMPA	#'S
	BNE	*+5
	JMP	REGLDS	CHANGE STACK POINTER
	CMPA	#'M
	BNE	*+5
	JMP	REGRS	RESET STACK POINTER
	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

***************
* HELP
HELP	LDX	#HELPTX
	JSR	PDATA
	JMP	PROMPT

***************
* GO
GO      JSR	PCRLF
	LDS     SP
	RTI

***************
* JUMP TO ADDRESS
JUMP	JSR     OUTS
	JSR     BADDR
        BCC	JUMPE	ADDRESS INPUT OK?
	JSR	PCRLF
	LDS     SP
	JMP	0,X	JUMP TO ADDRESS IN X
	BRA 	RETURN
JUMPE	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

***************
* JSR TO ADDRESS
JSUB	JSR     OUTS
	JSR     BADDR
        BCC	JUMPE	ADDRESS INPUT OK?
	JSR	PCRLF
	LDS     SP
	JSR	0,X	JUMP TO ADDRESS IN X
	JMP	PROMPT

***************
* RETURN FROM USER PROGRAM
RETURN	STS	SP
	JMP	PROMPT

***************
* PRINT CONTENTS OF STACK
PRTREG	LDX     #REGTX
	JSR     PDATA
	LDX     SP
	INX
        JSR     OUT2HS  CONDITION CODES
        JSR     OUT2HS  ACC-B
        JSR     OUT2HS  ACC-A
        JSR     OUT4HS  X-REG
        JSR     OUT4HS  P-COUNTER
	LDX     #SP
        JSR     OUT4HS  STACK POINTER

        JSR     OUTS
        LDX     SP
        LDAB    1,X
        LDX     #$06
        ASLB
        ASLB
CCLOOP	JSR     OUTS
        ASLB
        BCS     CCONE
CCZERO  LDAA    #'0
        JSR     OUTCHAR
        JMP     CCEND        
CCONE   LDAA    #'1
        JSR     OUTCHAR

CCEND   DEX
        BNE     CCLOOP
	JMP	PROMPT

***************
* REGISTER CHANGE ROUTINES (A B X PC CC SP)
REGACH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     REGERR
        STAA    3,X
ENDA    JMP     PROMPT
REGBCH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     REGERR
        STAA    2,X
ENDB    JMP     PROMPT
REGXCH  LDAA    #'=
        JSR     OUTCHAR
        JSR     BADDR
        BCC     REGERR
        STX     XHI
        LDX     SP
        LDD     XHI
        STD     4,X
ENDX    JMP     PROMPT
REGPCH  LDAA    #'=
        JSR     OUTCHAR
        JSR     BADDR
        BCC     REGERR
        STX     XHI
        LDX     SP
        LDD     XHI
        STD     6,X
ENDP    JMP     PROMPT
REGCCH  LDAA    #'=
        JSR     OUTCHAR
        LDX     SP
        JSR     INBYTE
        BCC     REGERR
        STAA    1,X
ENDC    JMP     PROMPT
REGLDS	LDAA    #'=
	JSR     OUTCHAR
	JSR	BADDR
	BCC	REGERR
	STX	SP
	TXS
	JMP	PROMPT
REGRS	LDS	#STACK
	STS	SP
	JSR	PROMPT
REGERR	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

***************
* Set and Clear Bits of Port B
SET_BIT_0
	LDAA	PIA1DAT
	ORAA	#$01
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_1
	LDAA	PIA1DAT
	ORAA	#$02
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_2
	LDAA	PIA1DAT
	ORAA	#$04
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_3
	LDAA	PIA1DAT
	ORAA	#$08
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_4
	LDAA	PIA1DAT
	ORAA	#$10
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_5
	LDAA	PIA1DAT
	ORAA	#$20
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_6
	LDAA	PIA1DAT
	ORAA	#$40
	STAA	PIA1DAT
	JMP	PROMPT
SET_BIT_7
	LDAA	PIA1DAT
	ORAA	#$80
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_0
	LDAA	PIA1DAT
	ANDA	#$FE
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_1
	LDAA	PIA1DAT
	ANDA	#$FD
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_2
	LDAA	PIA1DAT
	ANDA	#$FB
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_3
	LDAA	PIA1DAT
	ANDA	#$F7
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_4
	LDAA	PIA1DAT
	ANDA	#$EF
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_5
	LDAA	PIA1DAT
	ANDA	#$DF
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_6
	LDAA	PIA1DAT
	ANDA	#$BF
	STAA	PIA1DAT
	JMP	PROMPT
CLR_BIT_7
	LDAA	PIA1DAT
	ANDA	#$7F
	STAA	PIA1DAT
	JMP	PROMPT
***************
* MC6845 Data
MC6845_40Z FCB     $30
        FCB     $28   
        FCB     $2B   
        FCB     $43   
        FCB     $1D   
        FCB     $0C   
        FCB     $19   
        FCB     $1B   
        FCB     $50   
        FCB     $09   
        FCB     $00   
        FCB     $10   
        FCB     $00   
        FCB     $00   
        FCB     $00   
        FCB     $00   
MC6845_72Z FCB     $62
        FCB     $50   
        FCB     $53   
        FCB     $29   
        FCB     $26   
        FCB     $00   
        FCB     $19   
        FCB     $1F   
        FCB     $50   
        FCB     $07   
        FCB     $00   
        FCB     $07   
        FCB     $00   
        FCB     $00   
        FCB     $00   
        FCB     $00   
MC6845_80Z FCB     $62
        FCB     $50   
        FCB     $54   
        FCB     $29   
        FCB     $1E   
        FCB     $00   
        FCB     $19   
        FCB     $1B   
        FCB     $52   
        FCB     $09   
        FCB     $00   
        FCB     $10   
        FCB     $00   
        FCB     $00   
        FCB     $00   
        FCB     $00

W6845H	LDAA	#$80
W6845 	PSHA
	CLRB              
ZC4E4   STAB    M6845_0   
        LDAA    ,X        
        STAA    M6845_1   
        INX               
        INCB              
        CMPB    #$10      
        BNE     ZC4E4
        LDAA	PIA1DAT 
        anda 	#$7F
        PULB
        ABA
        STAA 	PIA1DAT 
        JMP	PROMPT
***************
SETUP3	LDX 	#DSP3TX
	JSR     PDATA
	LDX     #MC6845_40Z
	CLRA
	JMP 	W6845
***************
SETUP4	LDX 	#DSP4TX
	JSR     PDATA
	LDX     #MC6845_40Z
	CLRA
	JMP 	W6845
	
***************
SETUP7	LDX 	#DSP7TX
	JSR     PDATA
	LDX     #MC6845_72Z
	JMP 	W6845H
***************
SETUP8	LDX 	#DSP8TX
	JSR     PDATA
	LDX     #MC6845_80Z
	JMP 	W6845H
***************
* CHANGE MEMORY (MC AAAA DD NN)
CHANGE  JSR     OUTS
	JSR     BADDR   BUILD ADDRESS
	BCC     CHANGER
CHA51	JSR     PCRLF   C/R L/F
	LDX     #XHI
        JSR     OUT4HS  PRINT ADDRESS
	LDX     XHI
        JSR     OUT2HS  PRIND DATA (OLD)
	STX     XHI     SAVE DATA ADDRESS
	JSR     INBYTE	INPUT NEW DATA
	BCC     CHANG1
	DEX    
	STAA    ,X      CHANGE MEMEORY
	CMPA    ,X
	BEQ     CHA51   DID CHANGE
	LDAA    #'?
	JSR     OUTCHAR
	BRA     CHA51
CHANG1  CMPA    #$DD
	BEQ     CHA51
CHANGEE JMP     PROMPT
CHANGER	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

***************
* MEMORY WRITE (MW SADR)
MEMWRT	JSR	OUTS	PRINT SPACE
	JSR	BADDR	ENTER STARTING ADDRESS
	BCC	MEMWRTE	ADDRESS OK?
	STX	XHI
MEMWRTA	CLR	TEMP	CLEAR DATA ENTRY FLAG
	JSR	PCRLF
	LDX	#XHI
	JSR	OUT4HS	PRINT CURRENT ADDRESS
	LDX	XHI
MEMWRTD	JSR	INBYTE	GET BYTE FROM USER
	BCS	MEMWRTW	BYTE OK
	TST	TEMP	CHECK DATA ENTRY FLAG
	BNE	MEMWRTA	LOOP IF PREVIOUS DATA BYTES ENTERED
	JMP	PROMPT	IF NOT, EXIT ROUTINE
MEMWRTW	STAA	,X	STORE ENTERED DATA BYTE
	LDAA	#1
	STAA	TEMP	SET DATA ENTRY FLAG
	INX
	STX	XHI	STORE CURRENT ADDRESS
	JSR	OUTS	PRINT SPACE
	BRA	MEMWRTD	WAIT FOR ANOTHER BYTE
MEMWRTE	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT

***************
* FILL MEMORY (MF SADR-EADR DA)
MFILL	JSR	OUTS	PRINT SPACE
        JSR     BADDR   BUILD STARTING ADDRESS
        BCC     MFILLE	CHECK IF CORRECT
        STX     XTEMP   SAVE STARTING ADDRESS
        LDAA    #'-     PRINT SEPARATOR
        JSR     OUTCHAR
        JSR     BADDR   BUILD ENDING ADRESS
        BCC     MFILLE	CHECK IF CORRECT
        STX     XHI
        CPX     XTEMP   CHECK IF CORRECT ADDRESS RANGE
        BLS     MFILLE  IF NOT, EXIT ROUTINE
        INX
        STX     XHI
        JSR	OUTS    PRINT SPACE
        JSR     INBYTE	LOAD FILL DATA
        BCC     MFILLE CHECK IF CORRECT
        TAB
        LDX     XTEMP
MFILL2  STAB    ,X     STORE DATA
        INX
        CPX     XHI
        BNE     MFILL2
        JMP     PROMPT
MFILLE  LDAA	#'?
	JSR	OUTCHAR
        JMP     PROMPT

***************
* DUMP MEMORY (MD AAAA)
DUMPERR	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT
DUMP	JSR	OUTS
	JSR	BADDR
	BCC	DUMPERR	END IF ADDRESS NOT OK
	STX	XHI
	JSR	PCRLF
DUMP0	LDAA	XLOW
	ANDA	#$F0	BEGIN DUMP AT $xxx0
	STAA	XLOW
	LDX	#DUMPTX
	JSR	PDATA
	LDAA	#16
	STAA	TEMP	ROW COUNTER. 16 ROWS = 1 MEMORY PAGE
** PRINT ROW
DUMP1	LDX	#XHI
	JSR	OUT4HS	PRINT ADDRESS
	JSR	OUTS
	LDX	XHI
	STX	XTEMP	SAVE X FOR ASCII PRINT
** PRINT ROW OF BYTES
DUMP2	JSR	OUT2HS
	STX	XHI	SAVE NOW INCREMENTED X
	LDAA	XLOW
	ANDA	#$0F	FILTER OUT LAST NIB
	CMPA	#$08
	BNE	DUMP25
	JSR	OUTS
DUMP25	CMPA	#$00	LAST BYTE IN ROW?
	BNE	DUMP2
	LDX	XTEMP	RESTORE POINTER
	STX	XHI	  FOR ASCII DUMP
	JSR	OUTS
** PRINT ROW OF ASCII
DUMP3	LDAA	0,X
	CMPA	#$7E
	BHI	DUMP4	BYTE IS NOT PRINTABLE
	CMPA	#$20
	BGE	DUMP5	BYTE IS PRINTABLE
DUMP4	LDAA	#'.
DUMP5	JSR	OUTCHAR	PRINT ASCII CHAR
	INX
	STX	XHI	POINT TO NEXT CHARACTER
	LDAA	XLOW
	ANDA	#$0F
	BNE	DUMP3	LAST CHARACTER IN ROW?
	JSR	PCRLF
	DEC	TEMP
	BEQ	DUMPE	LAST ROW?
	BRA	DUMP1
DUMPE	JSR	INCHAR
	CMPA	#$0D
	BEQ	DUMP0	DUMP NEXT PAGE
	JMP	PROMPT

***************
* LOAD S19 RECORD
LOAD	LDX	#LDTX
	JSR	PDATA
LOAD1	LDAA	#$0D
	JSR	OUTCHAR
LOAD2	LDAA	#$39	(RTS)
	STAA	CONOVEC	DISABLE CONSOLE OUTPUT
	JSR	INCHAR
	CMPA	#'S
	BNE	LOAD2   1ST CHAR NOT (S)
	JSR	INCHAR  READ CHAR
	CMPA	#'9      
	BEQ	LOAD21  2ND CHAR (9)
	CMPA	#'1
	BNE	LOAD2   2ND CHAR NOT (1)
	CLR	CKSM    CLEAR CHECKSUM
	JSR	INBYTE	READ BYTE
	TAB
	ADDB	CKSM
	STAB	CKSM
	SUBA	#2
	STAA	TEMP	BYTE COUNT
	JSR	BADDR
	BCC	LOAD19	ADDRESS OK?
	LDAB	CKSM
	ADDB	XHI
	ADDB	XLOW
	STAB	CKSM
LOAD11	JSR	INBYTE
	TAB
	ADDB	CKSM
	STAB	CKSM
	DEC	TEMP
	BEQ	LOAD15	ZERO BYTE COUNT
	STAA	,X	STORE DATA
	INX
	BRA	LOAD11
LOAD15	INC	CKSM	INCREMENT CHECKSUM
	BEQ	LOAD1
LOAD19	LDAA	#$7E	(JMP EXTENDED)
	STAA	CONOVEC	ENABLE CONSOLE OUTPUT
	LDX	#LFAILTX	PRINT ERROR MESSAGE
        JSR	PDATA
LOAD20	JSR	CONIVEC
        ANDA	#$DF	TO UPPER CASE
        CMPA	#'Y
        BNE	LOAD20
	JMP	PROMPT
LOAD21	JSR	INCHAR
	CMPA	#$0D
	BNE	LOAD21
	LDAA	#$7E	(JMP EXTENDED)
	STAA	CONOVEC	ENABLE CONSOLE OUTPUT
	LDX	#LDOKTX
	JSR	PDATA
	JMP	PROMPT

***************
* PAGE SELECT ROUTINE
PAGE	LDAA	#'=
	JSR	OUTCHAR
	LDAA	#$FF
	STAA	PIA1DIR	SET ALL OUTPUT
	JSR	INHEX
	BCC	PAGERR
	CMPA	#$07
	BLS	PAGESET
PAGERR	LDAA	#'?
	JSR	OUTCHAR
	JMP	PROMPT
PAGESET	ORAA	#$80	MASK EXTERNAL INTERRUPTS
	STAA	PIA1DAT
	JMP	PROMPT

***************
* DO SINGLE STEP
SSTEP	JSR	PCRLF
STEP	LDS	SP		RESTORE PROGRAM STACK POINTER
	TSX
	LDAA	,X		LOAD CC
	ANDA	#$EF		CLEAR INTERUPT MASK BIT
	STAA	,X		SAVE CC
	LDAB	#$1F
STPWAI	DECB			WAIT FOR EVENTUAL SCI XFER
	CMPB	#$00		BEFORE TIMER INIT
	BNE	STPWAI
	LDX	#STOP		SET INTERRUPT VECTOR
	STX	TMOFVEC+1
	LDX	#$FFED		RESET COUNTER VALUE
	STX	COUNTHI
	LDX	TIMECON		CLEAR INTERRUPT BIT IN TIMER CTRL REG
	LDAA	#$04		ENABLE TIMER OVERFLOW INTERRUPT
	STAA	TIMECON
	RTI

***************
* SINGLE STEP INTERRUPT ENTRY
STOP	STS	SP		SAVE PROGRAM STACK POINTER
	LDX	#INTSEQ		RESTORE INTERRUPT VECTOR
	STX	TMOFVEC+1
	LDX	TIMECON		CLEARS INTERRUPT BIT IN TIMER CTRL REG
	LDAA	#$00		DISABLE TIMER INTERRUPT
	STAA	TIMECON
	LDX	SP		EXTRACT PROGRAM STOP ADDRESS
	LDAB	#6
	ABX
	LDX	,X
	CPX	#$C000
	BHI	STEP		NO STOP IN ROM
	STX	XTEMP
	LDAB	XTEMP
	CMPB	#$7F
	BEQ	STEP		NO STOP IN PAGE $7F
	LDX	#STOPTX
	JSR	PDATA
	JMP	PRTREG		PRINT REGS AND GO TO PROMPT

***************
* INIT BUILTIN ACIA
SCIINIT	
*	LDAA    #$05    ENABLE INTERNAL ACIA, INTERNAL CLOCK, 9600 BAUD
	LDAA    #$0C    ENABLE INTERNAL ACIA, EXTERNAL CLOCK, 38400 BAUD
	STAA    ACIAMOD
	LDAA    #$0A    ENABLE RECIEVE AND TRANSMITT DATA
	STAA    ACIASTA
	LDAA    ACIARXD	FLUSH BUFFER AND CLEAR ERROR FLAGS
	RTS
	
***************
* BUILTIN ACIA OUTPUT FROM A-ACC
SCIOUT	PSHB		SAVE B-REG
SCIOUT1	LDAB	ACIASTA
	ASLB
	ASLB
	ASLB
	BCC	SCIOUT1	READY FOR NEXT CHARACTER
	STAA	ACIATXD
	PULB	RESTORE	B-REG
	RTS

***************
* BUILTIN ACIA INPUT TO A-ACC
SCIINER	LDAA	ACIARXD	ON ERROR, FLUSH BUFFER AND CLEAR ERROR FLAG
SCIIN	LDAA	ACIASTA
	ANDA	#$C0	FILTER OUT RDRF AND ORFE
	CMPA	#$00
	BEQ	SCIIN	WAIT FOR CHARACTER
	CMPA	#$40
	BEQ	SCIINER	CHECK FOR FRAMING ERROR
	LDAA	ACIARXD	READ RECIEVED CHARACTER
        RTS

***************
* BUILTIN ACIA STATUS TO A-ACC
*  RETURNS 1 ON CHAR WAITING. 0 ON NO CHAR
SCISTAE	LDAA	ACIARXD	ON ERROR, FLUSH BUFFER AND CLEAR ERROR FLAG
SCISTAT	LDAA	ACIASTA
	ANDA	#$C0	FILTER OUT RDRF AND ORFE
	CMPA	#$00
	BEQ	SCISTA0	NO ERROR AND NO CHARACTER
	CMPA	#$40
	BEQ	SCISTAE	CHECK FOR ERROR
	LDAA	#$01	CHARACTER WAITING
        RTS
SCISTA0	LDAA	#$00
	RTS

***************
* OUTPUT/INPUT ONE CHAR TO/FROM A-REGISTER AND ECHO
INCHAR  JSR	CONIVEC
	JMP	CONOVEC
OUTCHAR EQU	CONOVEC

***************
* PRINT DATA POINTED AT BY X-REG
PDATA2  JSR     OUTCHAR
	INX
PDATA   LDAA    ,X
	CMPA    #4
	BNE     PDATA2  GO ON IF NOT EOT
	RTS

***************
* OUTPUT CRLF
PCRLF	PSHX
	LDX	#CRLFTX
	JSR	PDATA
	PULX
	RTS
	
***************
* OUTPUT HEX CHARS
OUTHL   LSRA            OUT HEX LEFT BCD DIGIT
	LSRA
	LSRA
	LSRA
OUTHR   ANDA    #$F     OUT HEX RIGHT BCD DIGIT
	ADDA    #$30
	CMPA    #$39
	BLS     OUTHE
	ADDA    #$7
OUTHE	JMP	OUTCHAR

OUT2H   LDAA    0,X
        JSR     OUTHL   OUTPUT LEFT HEX CHAR
	LDAA    0,X
	INX
        JMP     OUTHR   OUTPUT RIGHT HEX CHAR

OUT4HS  BSR     OUT2H   OUTPUT 4 HEX CHAR + SPACE
OUT2HS  BSR     OUT2H   OUTPUT 2 HEX CHAR + SPACE
OUTS    LDAA    #$20    SPACE
	JMP     OUTCHAR   (BSR & RTS)

***************
* INPUT HEX CHAR INTO A-ACC
INHEX   JSR     INCHAR
	SUBA    #$30
	BMI     NOTHEX
	CMPA    #$09
        BLE     IN1HG
        ANDA    #$DF    CONVERT TO UPPER CASE
	CMPA    #$11
	BMI     NOTHEX
	CMPA    #$16
	BGT     NOTHEX
	SUBA    #7
IN1HG	SEC	INPUT OK. SET CARRY
	RTS
NOTHEX  CLC	INPUT BAD. CLEAR CARRY
	RTS

***************
* INPUT BYTE (TWO FRAMES) INTO A-ACC
INBYTE	JSR	INHEX	GET HEX CHAR
	BCC	INBYTE1
	ASLA
	ASLA
	ASLA
	ASLA
	TAB
        JSR	INHEX
	BCC	INBYTE1
	ABA
	SEC	GOOD INPUT
	RTS
INBYTE1	CLC	BAD INPUT
	RTS

***************
* BUILD ADDRESS INTO X-REG
BADDR   BSR     INBYTE	READ FIRST FRAME
	BCC     BADDRE
	STAA    XHI
	BSR     INBYTE	READ SECOND FRAME
	BCC     BADDRE
	STAA    XLOW
	LDX     XHI     (X) ADDRESS WE BUILD
BADDRE  RTS

***************
* INTERRUPT SEQUENCE
INTSEQ	STS	SP
	JMP	PRTREG	PRINT REGS AND GO TO PROMPT

***************
* TRAP INTERRUPT SEQUENCE
TRAP    STS     SP      SAVE TARGET STACKPOINTER
        LDX     #TRAPTX
	JSR	PDATA
        JMP     PRTREG	PRINT REGS AND GO TO PROMPT

***************
* INITIATE VECTOR JUMPTABLE
INITVEC	LDAA	#$7E	JMP EXT OP CODE
        LDX     #INTSEQ
	STAA	NMIVEC
        STX     NMIVEC+1
	STAA	SWIVEC
        STX     SWIVEC+1
	STAA	IRQVEC
        STX     IRQVEC+1
	STAA	TMICVEC
        STX     TMICVEC+1
	STAA	TMOCVEC
        STX     TMOCVEC+1
	STAA	TMOFVEC
        STX     TMOFVEC+1
	LDX	#SCIOUT
	STAA	CONOVEC
	STX	CONOVEC+1
	LDX	#SCIIN
	STAA	CONIVEC
	STX	CONIVEC+1
	LDX	#SCISTAT
	STAA	CONSVEC
	STX	CONSVEC+1
        RTS

***************
* I/O READ FUNCTION
*  IN: X = ADDRESS
*      B = I/O PAGE
* OUT: A = DATA
IORD	LDAA	PIA1DAT
	PSHA			SAVE PAGE REG
	STAB	PIA1DAT		SET NEW I/O PAGE
	LDAA	,X		ACCESS I/O PAGE
	PULB
	STAB	PIA1DAT		RESTORE PAGE REG
	RTS

***************
* I/O WRITE FUNCTION
* IN: X = ADDRESS
*     B = I/O PAGE
*     A = DATA
IOWR	PSHA			SAVE DATA
	LDAA	PIA1DAT
	PSHA			SAVE PAGE REG
	STAB	PIA1DAT		SET NEW I/O PAGE
	PULB			RESTORE PAGE REG VALUE
	PULA			RESORE DATA VALUE
	STAA	,X		ACCESS I/O PAGE
	STAB	PIA1DAT		RESTORE PAGE REG
	RTS

***************
* VECTORS
        ORG     $FFEE
        FDB     TRAP	FFEE-EF	TRAP

	ORG     $FFF2
        FDB     TMOFVEC	FFF2-3  TIMER OVER FLOW
        FDB     TMOCVEC	FFF4-5  TIMER OUTPUT COMPARE
        FDB     TMICVEC	FFF6-7  TIMER INPUT CAPTURE
	FDB     IRQVEC	FFF8-9	IRQ
	FDB     SWIVEC	FFFA-B	SOFTWARE INTERRUPT
	FDB     NMIVEC	FFFC-D	NMI
	FDB     RESET	FFFE-F	RESET

***************
* EXTENDED ROM
EXROM	EQU	$f000
