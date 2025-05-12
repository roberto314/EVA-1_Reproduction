;****************************************************
;* KK Systems EVA-1 Firmware                        *
;* Disassembled and commented 2025 by R. Offner     *
;* v1.0                                             *
;* This is the original Version                     *
;* without any enhancements.                        *
;* While this Version works (more or less)          *
;* It has some problems in the initialisation of    *
;* the 6845 (Cursor is shifted)!                    *
;****************************************************
;
; Very quick remainder how the communication between a HX-20 and the EVA-1 works.
;
; 1. MASTER - SLAVE HANDSHAKE
;
; SOURCE         DESTINATION            COMMENT
;
;                                  ENQURE TO DESTINATION DEVICE
;(EOT)    --->                     $04
; P1      --->                     is usually $31
; DID     --->                     can be $30 or $20
; SID     --->                     can be $20 or $30
; ENQ     --->                     $05
;        <---    ACK               $06
;
; 2. HEADER                                                                            Memory Location
;                                                                                        RX    TX
; SOH     --->                     $01 - SEND HEADER (FUNCTION)                           -    $80
; FMT     --->                     usually $00 for HX-20 -> EVA-1 and $01 for other dir. $80   $81
; DID     --->                     can be $30 or $20                                     $81   $82
; SID     --->                     can be $20 or $30                                     $82   $83
; FNC     --->                     $xx                                                   $83   $84
; SIZ     --->                     $xx                                                   $84   $85
; HCS     --->                     Checksum                                              $85   
;        <---    ACK (NAK), (WAK)  $06
;
; 3. DATA
;
; STX     --->                     $02                                                  
; DB0     --->                     Data Bytes                                            $A0
; DB1     --->                     Data Bytes                                            $A1
;  .      --->                     Data Bytes                                            $A2
;  .      --->                     Data Bytes                                            $A3
; DBN     --->                     Data Bytes                                            $A4
; ETX     --->                     $03
; CKS     --->                     Checksum
;        <---    ACK, (NAK)        $06
;
; 4. END OF TRAMSMISSION
;
;(EOT)    --->                     $04
;
;-------------------------------------------------------------------
; CONTROL CODES
; SOH = 01
; STX = 02
; ETX = 03
; EOT = 04
; ENQ = 05
; ACK = 06
; DLE = 10
; NAK = 15
; WAK = DLE ;  (1F 3B)
;-------------------------------------------------------------------
; FMT CODE  FMT  DID  SID  FNC  SIZ  MSG       COMMENT
;   00       1    1    1    1    1   1-256   PREFERRED FORMAT
;   01       1    1    1    1    1   1-256   RETURNED RESULT
;   02       1    1    1    1    2   1-65536          (NOT SUPPORTED)
;   03       1    1    1    1    2   1-65536          (NOT SUPPORTED)
;   04       2    2    2    1    1   1-256            (NOT SUPPORTED)
;   05       2    2    2    1    1   1-256            (NOT SUPPORTED)
;   06       2    2    2    1    2   1-65536          (NOT SUPPORTED)
;   07       2    2    2    1    2   1-65536          (NOT SUPPORTED)
;-------------------------------------------------------------------
; Some useful IDs:
; HX-20: $20
; PX-8:  $22
; PX-4:  $23
; EVA-1 (and prob. other Screen Adpters) $30
; First TF-20 (first & second disk):  $31
; Second TF-20 (third & fourth disk): $32
;****************************************************
;* Used Labels                                      *
;****************************************************

P1DDR          EQU     $0000
P2DDR          EQU     $0001
P1DR           EQU     $0002
P2DR           EQU     $0003
CNTH           EQU     $0009
RMCR           EQU     $0010
TRCSR          EQU     $0011
RDR            EQU     $0012
TDR            EQU     $0013
RCR            EQU     $0014
HDRBUFSTRT     EQU     $0080 ; -|- Header Buffer
HDRSRC         EQU     $0082 ;  |
HDRFUNC        EQU     $0083 ;  |
HDRSZFN        EQU     $0084 ;  | - SIZ for RX and FNC for TX
HDRCSSZ        EQU     $0085 ; -| - HCS for RX and SIZ for TX
_6845STRTADRH  EQU     $0098 ; 
_6845STRTADRL  EQU     $0099 ; 6845 Start Address
CursorColumn   EQU     $009A ; Colunm of cursor (0..Char_per_Line-1)
CursorRow      EQU     $009B ; Row of Cursor (0..24)
_6845CRSRH     EQU     $009C ; 
_6845CRSRL     EQU     $009D ; _6845CRSR always 0x1000 less than RAMTXTSTART
TXTBUFA0A1     EQU     $00A0 ; -|- Text Buffer
TXTBUFA1       EQU     $00A1 ;  |
TXTBUFA2A3     EQU     $00A2 ;  |
TXTBUFA3       EQU     $00A3 ;  |
TXTBUFA4A5     EQU     $00A4 ;  |
TXTBUFA6A7     EQU     $00A6 ;  |
TXTBUFA8A9     EQU     $00A8 ; -|
TEMPACAD       EQU     $00AC ; Temp Var. only used in CONVXPXLCHAR
TEMPAEAF       EQU     $00AE ; Temp Var. only used in CONVXPXLCHAR
TEMPAF         EQU     $00AF ; Temp Var. only used in CONVXPXLCHAR
TEMPB0B1       EQU     $00B0 ; Temp Var. used in DRWLN and READCHAR
TEMPB2B3       EQU     $00B2 ; Temp Var. used in DRWLN
TEMPB4B5       EQU     $00B4 ; Temp Var. used in DRWLN
TEMPB6B7       EQU     $00B6 ; Temp Var. used in DRWLN
TEMPB8B9       EQU     $00B8 ; Temp Var. used in DRWLN (only stored, never read)
TEMPBABB       EQU     $00BA ; Temp Var. used in DRWLN (only stored, never read)
TEMPBCBD       EQU     $00BC ; Temp Var. used in DRWLN
TEMPBEBF       EQU     $00BE ; Temp Var. used in DRWLN
TEMPBF         EQU     $00BF ; Temp Var. used in DRWLN
RXTXCNT        EQU     $00C1 ; How many Char. to send
TEMPC2C3       EQU     $00C2 ; Temp Var. used in DRWLN
SETB7          EQU     $00C4 ; If $FF Bit7 of Next character is set
CHRTMP         EQU     $00C5 ; Temp Character Storage for Intensity and Inverse Bit
_6845CRSRREG   EQU     $00C6 ; Temp Var. for 6845 Cursor Register
CHKSUM         EQU     $00C7 ; Temp Var. for Checksum Calculation
TMPCRSRRW      EQU     $00C8 ; Temp Var. for Cursor Row used in READLINE
TEMP16_01      EQU     $00CA ; Temp Var. for 'X' Register
RAMTXTSTART    EQU     $00CC ; RAM Address where the Text starts (starts @ $1000)
M00CE          EQU     $00CE
M00D0          EQU     $00D0
M00D1          EQU     $00D1
Char_per_Line  EQU     $00D2 ; can be 32, 40, 72 or 80
GRAPHMOD       EQU     $00D3 ; if 0 only Textmode, else Graphic also
TMPRDCH        EQU     $00D4 ; Temp Var. for READCHAR
TEMPD8D9       EQU     $00D8
M00D9          EQU     $00D9
M00DA          EQU     $00DA
RAMEND         EQU     $00FF
TEXTRAM        EQU     $1000 ;
TRAMEND        EQU     $1800 ; End of Text Ram +1
M6845_0        EQU     $4000
M6845_1        EQU     $4001
GRAPHICRAM     EQU     $8000 ; Start of Graphic RAM
GRAMEND        EQU     $C000 ; End of Graphic RAM+1 (used for Clear)

;****************************************************
;* Program Code / Data Areas                        *
;****************************************************

        ORG     $c000

hdlr_RST       SEI                     ;
               LDS     #RAMEND         ;
               CLRB                    ;
               LDAA    #$FF            ;
               STAA    CHKSUM          ;
               STAA    P1DDR           ;
               STAB    P2DDR           ;
               LDAA    #$0F            ;
               STAA    P1DR            ; Clear INVERT, TXTON_OFF, GTEXT, TEXT (P1.0-P1.3), Set CHARSET, T/G, HIGHRES (P1.4-P1.7)
               STAB    GRAPHMOD        ;
               STAB    P2DR            ;
               STAB    _6845CRSRREG    ;
               LDAA    #$0C            ;
               STAA    RMCR            ; CC0 and CC1 set - external Serial clock (8xBaudrate)
               LDAB    #$0A            ;
               STAB    TRCSR           ; Receiver and Transmitter Enable
               LDAA    #$C0            ;
               STAA    RCR             ; enable internal RAM
               JSR     CONF40          ;
               JSR     ClearGRAM       ;
               LDS     #GREETING-1     ; Write Text to TextRAM
               LDX     #$139C          ; Start Position of Text
LOOP01         PULA                    ;
               STAA    ,X              ;
               INX                     ;
               CMPA    #$21            ;
               BNE     LOOP01          ;
               LDS     #RAMEND         ;
;*******************************************************************************
;*******************************************************************************
;**                      RX State Machine                                     **
;*******************************************************************************
;*******************************************************************************
LOOP02         LDX     TRCSR           ;
WAITHNDSHK     LDAA    TRCSR           ; State Machine for Handshake (1)
               BITA    #$40            ; check for Overrun Framing Error
               BNE     LOOP02          ;
               LDAB    RDR             ; Received Char is in 'B'
               CMPB    #$31            ;
               BNE     WAITHNDSHK      ; Check 'START? (31)' - everything before gets thrown away, START of STATE MACHINE!
               BSR     WAITRXC         ;
               CMPB    #$30            ;
               BNE     WAITHNDSHK      ; Check 'DID (30)'
               BSR     WAITRXC         ;
               CMPB    #$20            ;
               BNE     WAITHNDSHK      ; Check 'SID (20)'
               BSR     WAITRXC         ;
               CMPB    #$05            ;
               BNE     WAITHNDSHK      ; Check for 'ENQ (05)'
               BSR     Send_ACK        ;
               BSR     WAITRXC         ;
               CMPB    #$01            ;
               BEQ     GETHDR          ; Check 'SOH (01)'
               BRA     SNDNONACK       ;
;------------------------------------  ;
WAITRXC        TST     >TRCSR          ; Blocking Wait for RX Character
               BPL     WAITRXC         ;
               LDAB    RDR             ;
               RTS                     ;
;--------------------------------------;
GETTEXT        BSR     WAITRXC         ; State Machine for Text (4)
               CMPB    #$02            ; Check 'STX (02)'
               BNE     SNDNONACK       ;
               LDX     #TXTBUFA0A1     ; Buffer starts @ $A0
               TBA                     ; RXChar is now also in 'A' for Checksum
               LDAB    HDRSZFN         ;
               INCB                    ;
               STAB    RXTXCNT         ;
LOOP03         BSR     WAITRXC         ;
               STAB    ,X              ;
               ABA                     ;
               INX                     ;
               DEC     >RXTXCNT        ;
               BNE     LOOP03          ;
               BSR     WAITRXC         ;
               CMPB    #$03            ;  Check 'ETX (03)'
               BNE     Send_NACK       ;
               ABA                     ;
               BSR     WAITRXC         ;
               ABA                     ;
               BEQ     Send_ACK        ; Check for Checksum = zero
Send_NACK      LDAB    #$15            ;
               BRA     SendB           ; Send NACK
;--------------------------------------;
GETHDR         TBA                     ; State Machine for Header (2), RXChar is now also in 'A' for Checksum (01)
               LDX     #HDRBUFSTRT     ; Buffer starts at $80
LOOP04         BSR     WAITRXC         ;
               STAB    ,X              ; Store in Buffer: 01,00,30
               ABA                     ; add to Checksum (01+00+30)
               INX                     ;
               CPX     #HDRCSSZ        ; Check max Length of 6 Characters (HDRCSSZ is @ $85)
               BNE     LOOP04          ;
               BSR     WAITRXC         ; Get Checksum from HX-20
               ABA                     ;
               BEQ     HDRFINISHED     ; Check for Checksum = zero.
SNDNONACK      BSR     Send_NACK       ; There may be a small Bug! EVA-1 sends a NACK every Time after Header 01,00,30!
               JMP     WAITHNDSHK      ;
;--------------------------------------;
Send_ACK       LDAB    #$06            ;
SendB          LDAA    TRCSR           ;
               ANDA    #$20            ;  Check TX Reg. Empty
               BEQ     SendB           ;
               STAB    TDR             ;
               RTS                     ;
;--------------------------------------;
HDRFINISHED    BSR     Send_ACK        ; Header is done (3), Send ACK
               BSR     GETTEXT         ; Receive TEXT Packet
               CLR     >HDRCSSZ        ;
               BSR     WAITRXC         ;
               CMPB    #$04            ; Check 'EOT (04)'
               BNE     SNDNONACK       ;
               LDAA    HDRFUNC         ;
               LDX     #EPSEND         ; Load End of EPSFUNC Jumptable
               STX     TEMP16_01       ;
               LDX     #EPSPFUNC       ; Load Beginning of EPSFUNC Jumptable
               JSR     EXECSRCH        ; Search for correct Function, if found Jump to it
               BRA     SNDNONACK       ;
;*******************************************************************************
;*******************************************************************************
;**                      TX Functions                                         **
;*******************************************************************************
;*******************************************************************************
; SNDHDR:
; Builds 6 Byte Header and sends it [01 01 20 30 FUNC SIZ]
; needs HDRCSSZ @ $85 which becomes SIZ and HDRFUNC
; Terminates with RTS
;
SNDHDR         LDAA    #$06            ; 6 Bytes long
               STAA    RXTXCNT         ;
               LDAA    HDRFUNC         ; FUNC from $83
               STAA    HDRSZFN         ; to $84
               LDX     HDRDAT01        ; Reads $01,$01 from EPROM
               STX     HDRBUFSTRT      ; Buffer starts @ $80
               LDX     HDRDAT02        ; Reads $20,$30 from EPROM
               STX     HDRSRC          ; @ $82
               LDX     #HDRBUFSTRT     ;
;--------------------------------------;
; SNDARR1:
; sends Values for RXTXCNT from 'X' Pointer and builds Checksum
; blocking waits for one RX Character
;
SNDARR1        CLRA                    ;
SNDARR2        LDAB    ,X              ;
               ANDB    CHKSUM          ;
               ABA                     ; Add to Checksum
               PSHA                    ;
               BSR     SendB           ;
               PULA                    ;
               INX                     ;
               DEC     >RXTXCNT        ;
               BNE     SNDARR2         ; Loop until all char are sent
               NEGA                    ;
               TAB                     ;
               BSR     SendB           ; Send Checksum
               JSR     WAITRXC         ;
               LDAA    #$FF            ;
               STAA    CHKSUM          ;
               RTS                     ;
;--------------------------------------;
               LDX     #SendPKT_00     ; Never executed
               LDAA    #$01            ; Never executed
;--------------------------------------;
SENDPAK        ADDA    #$03            ; 'A' is the Charactercount without Checksum, Start and Stop, 'X' is the Pointer to Data
               STAA    RXTXCNT         ; (only used for one Packet - Send Screensize)
LOOP06         LDAB    ,X              ;
               JSR     SendB           ;
               INX                     ;
               DEC     >RXTXCNT        ;
               BNE     LOOP06          ;
               JSR     WAITRXC         ;
;--------------------------------------;
Send_EOT       LDAB    #$04            ;
               JSR     SendB           ;
               BRA     DISABLED        ; JMP to WAITHNDSHK
;--------------------------------------;
               CLR     >GRAPHMOD       ; Never executed
               JSR     CONF80          ; Never executed
DISABLED       JMP     WAITHNDSHK      ;
;*******************************************************************************
;*******************************************************************************
;**                      various Functions                                    **
;*******************************************************************************
;*******************************************************************************
; RSTTXTSCRN:
; Clears all the TextRAM ($1000-$1FFF) and all rel. variables
; Terminates with RTS
;
RSTTXTSCRN     LDX     #$0000          ; Clear everything
               STX     M00D0           ;
               STX     SETB7           ;
               STX     _6845STRTADRH   ;
               STX     CursorColumn    ;
               STX     _6845CRSRH      ;
               LDX     #TEXTRAM        ;
               STX     RAMTXTSTART     ;
               CLRA                    ;
ClearTRAM      STAA    ,X              ;
               INX                     ;
               CPX     #TRAMEND        ;  End of TRAM (Ch. from 0x1800 to 0x2000)
               BNE     ClearTRAM       ;
               RTS                     ;
;*******************************************************************************
;*******************************************************************************
;**                       EPSP Functions                                      **
;*******************************************************************************
;*******************************************************************************
READCHAR       LDAA    TXTBUFA2A3      ; EPSPFUNC: $97
               STAA    M00CE           ;
               STAA    HDRCSSZ         ;
               JSR     SNDHDR          ;
               LDX     TXTBUFA0A1      ;
               STX     CursorColumn    ;
               JSR     SET6845ADD      ;
               LDX     RAMTXTSTART     ;
               INX                     ;
               LDAB    M00CE           ;
               ABX                     ;
               LDAA    ,X              ;
               STAA    TMPRDCH         ;
               STX     TEMPB0B1        ;
               LDAA    #$03            ;
               STAA    ,X              ;
               ADDB    #$02            ;
               STAB    RXTXCNT         ;
               LDX     RAMTXTSTART     ;
               LDAB    #$02            ;
               JSR     SendB           ;
               TBA                     ; 'A' holds $02
               LDAB    #$7F            ;
               STAB    CHKSUM          ;
               JSR     SNDARR2         ; send it from 'X'
               LDX     TEMPB0B1        ;
               LDAA    TMPRDCH         ;
               STAA    ,X              ;
               BRA     Send_EOT        ;
;--------------------------------------;
SCRNSIZE       BSR     LDCHKSM         ; EPSPFUNC: $88 and $89
               LDX     #SCRNSZ8025     ; Reads $02,$50,$19,$03,$92 from EPROM
               LDAA    #$02            ; send two bytes excluding Start, Stop and checksum (=5)
               JMP     SENDPAK         ;
                                       ;
LDCHKSM        LDAA    #$01            ;
SHDRCSSZ       STAA    HDRCSSZ         ;
               JMP     SNDHDR          ;
;--------------------------------------;
GCRSRPOS       LDAA    M00D0           ; EPSPFUNC: $8c
               STAA    M00D1           ;
               LDAA    CursorColumn    ;
               INCA                    ;
               STAA    M00D0           ;
SENDTX2        BSR     LDCHKSM         ;
               LDX     CursorColumn    ;
               STX     TXTBUFA1        ; $A1,$A2 is 00,00 at first
               LDAA    #$03            ;
               STAA    TXTBUFA3        ; add ETX ($03)
               LDAA    #$02            ; send 4 char + checksum
SENDTXT        ADDA    #$02            ;
               STAA    RXTXCNT         ;
               LDAA    #$02            ;
               STAA    TXTBUFA0A1      ; add STX ($02) to $A0
               LDX     #TXTBUFA0A1     ;
               JSR     SNDARR1         ; send it!
               JMP     Send_EOT        ; send EOT ($04)
;--------------------------------------;
GETPOINT       CLRB                    ; EPSPFUNC: $8f
               STAB    HDRCSSZ         ;
               LDD     TXTBUFA0A1      ; in A0A1 is the X Pos.
               JSR     CONVXPXLCHAR    ;
               ANDB    M00CE           ;
               BEQ     ZC1CA           ;
               LDAB    #$01            ;
ZC1CA          STAB    TXTBUFA1        ;
               JSR     SNDHDR          ;
               LDAA    #$03            ;
               STAA    TXTBUFA2A3      ;
               LDAA    #$01            ;
               BRA     SENDTXT         ;
;--------------------------------------;
READLINE       LDAA    #$03            ; EPSPFUNC: $91
               BSR     SHDRCSSZ        ;
               LDAA    CursorRow       ;
               BEQ     ZC1EB           ;
ZC1DF          JSR     CRSRLFT         ;
               LDX     RAMTXTSTART     ;
               LDAA    ,X              ;
               BNE     ZC1DF           ;
               JSR     INCCRSR         ;
ZC1EB          LDAA    CursorRow       ;
               STAA    TMPCRSRRW       ;
LOCAL0a        LDX     RAMTXTSTART     ;
               LDAA    ,X              ;
               BEQ     LOCAL09         ;
               JSR     INCCRSR         ;
               JSR     SET6845ADD      ;
               BRA     LOCAL0a         ;
                                       ;
LOCAL09        LDX     #TXTBUFA0A1     ;
               LDAA    TMPCRSRRW       ;
               STAA    $02,X           ;
               LDAB    CursorRow       ;
               STAB    $04,X           ;
               LDAA    CursorColumn    ;
               STAA    $03,X           ;
               LDAA    M00D1           ;
               BEQ     ZC215           ;
               DECA                    ;
               INC     $04,X           ;
               CLR     $03,X           ;
ZC215          STAA    $01,X           ;
               LDAA    $04,X           ;
               STAA    TMPCRSRRW       ;
               CLRA                    ;
               STAA    M00D0           ;
               LDAA    #$03            ;
               STAA    $05,X           ;
               LDAA    #$04            ;
               BRA     SENDTXT         ;
;--------------------------------------;
TEXTON_OFF     LDAA    P1DR            ; Char: $F1
               EORA    #$02            ; Toggle TXTON_OFF (P1.1)
LOCAL01        STAA    P1DR            ;
               BRA     SENDRESP        ;
;--------------------------------------;
INVERTTEXT     LDAA    P1DR            ; Char: $F2
               EORA    #$01            ; Toggle INVERT (P1.0)
               BRA     LOCAL01         ;
;--------------------------------------;
CTRLCHAR       LDX     #SwitchCharset  ; Load End of Control Char Jumptable
               STX     TEMP16_01       ; holds End of Control Char Jumptable
               LDX     #CTRLTAB        ; Load Start of Control Char Jumptable
               BSR     EXECSRCH        ;
               BRA     REGULACHAR      ;
;--------------------------------------;
EXECSRCH       CMPA    ,X              ; Search for Function and Jump to it
               BEQ     EXECFUNC        ;
               INX                     ;
               INX                     ;
               INX                     ;
               CPX     TEMP16_01       ; holds End of EPSFUNC or Function Jumptable
               BNE     EXECSRCH        ;
               RTS                     ; nothing found, must be a regular character
;--------------------------------------;
EXECFUNC       INS                     ; Execute Function
               INS                     ;
               LDX     $01,X           ;
               JMP     ,X              ;
;--------------------------------------;
WRITECHAR      LDAA    TXTBUFA0A1      ; EPSPFUNC: $92 and $98, 'A' holds the Character
               BRA     CTRLCHAR        ; Check is it a Controlcharacter
;--------------------------------------;
REGULACHAR     CMPA    #$20            ; Write a regular character (above $1F)
               BMI     SENDRESP        ;
               LDX     RAMTXTSTART     ;
               ANDA    #$7F            ; limit to characters < $80
               ORAA    CHRTMP          ;
               STAA    ,X              ;
               LDAA    SETB7           ;
               STAA    CHRTMP          ;
               BSR     INCCRSR         ;
SENDRESP       BSR     SET6845ADD      ; Set 6845 Start- and Cursor Address
               JMP     SENDTX2         ; send packets (Header, Text and EOT)
;--------------------------------------;
; INCCRSR:
; Scrolls down when CursorColumn is one less than Char_per_Line
; Clears CursorColumn 
; 
INCCRSR        LDAA    CursorColumn    ;
               INCA                    ;
               STAA    CursorColumn    ;
               CMPA    Char_per_Line   ;
               BNE     ZC287           ;
               CLR     >CursorColumn   ;
               INC     >CursorRow      ;
               LDAB    #$18            ; no scroll below 24
               CMPB    CursorRow       ;
               BGE     ZC285           ;
               JSR     SCRLUP          ;
ZC285          BSR     SET6845ADD      ;
ZC287          RTS                     ;
;--------------------------------------;
SET6845ADD     BSR     CALCRAMPOS      ;
               STD     RAMTXTSTART     ;
               LDAB    #$0C            ; 
               STAB    M6845_0         ; Set Register c (Start Address H)
               LDAA    _6845STRTADRH   ;
               STAA    M6845_1         ;
               INCB                    ;
               STAB    M6845_0         ; Set Register d (Start Address L)
               LDAA    _6845STRTADRL   ;
               STAA    M6845_1         ;
               INCB                    ;
               STAB    M6845_0         ; Set Register e (Cursor H)
               LDAA    _6845CRSRH      ;
               STAA    M6845_1         ;
               INCB                    ;
               STAB    M6845_0         ; Set Register f (Cursor L)
               LDAA    _6845CRSRL      ;
               STAA    M6845_1         ;
               RTS                     ;
;--------------------------------------;
CALCRAMPOS     LDAB    #$18            ; $18 = 24 (Max. Rows-1)
               CMPB    CursorRow       ;
               BGE     LOCAL0d         ;
               STAB    CursorRow       ; limit CursorRow to $18 (=24)
LOCAL0d        LDAB    CursorRow       ;
               LDAA    Char_per_Line   ; can be 32, 40, 72 or 80
               MUL                     ; 24*80=0x780 (=1920)
               ADDB    CursorColumn    ;
               ADCA    #$00            ; must be < $7D0
               ADDD    _6845STRTADRH   ;
               STD     _6845CRSRH      ;
               ADDD    #TEXTRAM        ;
               RTS                     ;
;--------------------------------------;
SETDISPMOD     LDAB    TXTBUFA1        ; EPSPFUNC: $93
               STAB    GRAPHMOD        ; $01 if Graphics Mode
               LDAA    P1DR            ;
               CMPB    #$01            ; Check if Graphics Mode
               BNE     SETTXTMOD       ; Graphics Mode here
               ORAA    #$40            ; Set T/G (P1.6)
               STAA    P1DR            ;
               LDX     #MC6845_GRAPH   ; Load Settings for Graphics Mode
               LDAA    #$50            ;
               JSR     W6845REG2       ;
               LDX     #$0000          ;
               STX     _6845STRTADRH   ;
               JMP     SENDRESP        ;
;--------------------------------------;
SETTXTMOD      ORAA    #$02            ; Textmode
               ANDA    #$BF            ; Clear T/G (P1.6)
               STAA    P1DR            ;
               JSR     CONF80          ; Configure for 80 Char. Text
               JMP     Home            ;
;--------------------------------------;
SETCRSR        LDX     TXTBUFA0A1      ;
               STX     CursorColumn    ;
               JSR     SET6845ADD      ;
               BRA     SRLISTFLG       ; does nothing (jumps back to WAITHNDSHK)
;--------------------------------------;
SRLISTFLG      JMP     WAITHNDSHK      ;
;--------------------------------------;
CONVXPXLCHAR   STD     TEMPD8D9        ; Converts X-Pixel to Character Pos.
               LDAA    #$06            ;
               STAA    M00DA           ;
               BSR     CONV03          ;
               LDAB    M00D9           ;
               INCB                    ;
               STAB    TXTBUFA1        ;
               CLR     >TXTBUFA0A1     ;
               STAA    M00CE           ;
               CLRA                    ;
               STAA    TEMPD8D9        ;
               LDAA    #$08            ;
               STAA    M00DA           ;
               LDAB    TXTBUFA3        ;
               STAB    M00D9           ;
               BSR     CONV03          ;
               LDAB    M00D9           ;
               STAB    TEMPAF          ;
               LDAB    #$80            ;
               MUL                     ;
               STD     TEMPACAD        ;
               LDAA    #$50            ;
               LDAB    TEMPAF          ;
               MUL                     ;
               ADDD    TXTBUFA0A1      ;
               STAB    TEMPAEAF        ;
               ANDB    #$80            ;
               ASLD                    ;
               ASLD                    ;
               ASLD                    ;
               LDAB    TEMPAEAF        ;
               ANDB    #$7F            ;
               ADDD    TEMPACAD        ;
               ADDD    #GRAPHICRAM     ;
               PSHB                    ;
               PSHA                    ;
               PULX                    ;
               LDAA    #$80            ;
               LDAB    M00CE           ;
               BEQ     CONV02          ;
CONV01         LSRA                    ;
               DECB                    ;
               BNE     CONV01          ;
CONV02         LDAB    ,X              ;
               STAA    M00CE           ;
               RTS                     ;
;--------------------------------------;
SETPOINT       BSR     PUTPXL          ; EPSPFUNC: $C7
_SRLISTFLG1_   BRA     SRLISTFLG       ; does nothing (jumps back to WAITHNDSHK)
;--------------------------------------;
PUTPXL         LDD     TXTBUFA0A1      ; in A0A1 is the X Pos.
               BSR     CONVXPXLCHAR    ;
               TST     >TXTBUFA4A5     ;
               BEQ     PXL01           ;
               ORAB    M00CE           ;
PXL02          STAB    ,X              ;
               RTS                     ;
                                       ;
PXL01          COMA                    ;
               STAA    M00CE           ;
               ANDB    M00CE           ;
               BRA     PXL02           ;
;--------------------------------------;
CONV03         LDX     #$0009          ;
               LDD     TEMPD8D9        ;
CONV04         SUBA    M00DA           ;
               BCC     CONV05          ;
               ADDA    M00DA           ;
CONV05         ROLB                    ;
               ROLA                    ;
               DEX                     ;
               BNE     CONV04          ;
               STAB    M00D9           ;
               RORA                    ;
               ROLB                    ;
               COMB                    ;
               ANDB    #$01            ;
               STAB    TEMPD8D9        ;
               COM     >M00D9          ;
               RTS                     ;
;--------------------------------------;
DRAWLINE       BSR     DRWLN           ; EPSPFUNC: $C8
               BRA     _SRLISTFLG1_    ; does nothing (jumps back to WAITHNDSHK)
;--------------------------------------;
ZC38C          COMA                    ;
               COMB                    ;
               ADDB    #$01            ;
               ADCA    #$00            ;
               RTS                     ;
;--------------------------------------;
; This funtion draws a line from       ;
;  X1 , Y1 to  X2 , Y2                 ;
; A0A1 A2A3   A4A5 A6A7                ;
;--------------------------------------;
DRWLN          LDX     #$01DF          ; Limit to 479 (prob. x direction)
               CPX     TXTBUFA0A1      ; check Limit
               BPL     LIN01           ;
               STX     TXTBUFA0A1      ;
LIN01          CPX     TXTBUFA4A5      ; check Limit
               BPL     LIN02           ;
               STX     TXTBUFA4A5      ;
LIN02          LDX     #$C7            ; Limit to 199
               CPX     TXTBUFA2A3      ; check Limit
               BPL     LIN03           ;
               STX     TXTBUFA2A3      ;
LIN03          CPX     TXTBUFA6A7      ; check Limit
               BPL     LIN04           ;
               STX     TXTBUFA6A7      ;
LIN04          LDX     #$0000          ;
               STX     TEMPBEBF        ;
               STX     TEMPB8B9        ;
               STX     TEMPBCBD        ;
               STX     TEMPBABB        ;
               LDX     TXTBUFA0A1      ;
               STX     TEMPB0B1        ;
               LDX     TXTBUFA2A3      ;
               STX     TEMPB2B3        ;
               LDD     TXTBUFA4A5      ;
               SUBD    TXTBUFA0A1      ;
               BPL     LIN05           ;
               BSR     ZC38C           ;
               INC     >TEMPBEBF       ;
LIN05          STD     TEMPB4B5        ;
               LDD     TXTBUFA6A7      ;
               SUBD    TXTBUFA2A3      ;
               BPL     LIN06           ;
               BSR     ZC38C           ;
               INC     >TEMPBF         ;
LIN06          STD     TEMPB6B7        ;
               LDAA    TXTBUFA8A9      ; $01, used only in TestPicture
               STAA    TXTBUFA4A5      ;
               LDX     TEMPB4B5        ;
               STX     TEMPD8D9        ;
               CPX     TEMPB6B7        ;
               BMI     LIN07           ;
               INC     >TEMPBCBD       ;
               LDX     TEMPB6B7        ;
               BRA     LIN08           ;
                                       ;
LIN07          LDX     TEMPB6B7        ;
               STX     TEMPD8D9        ;
               LDX     TEMPB4B5        ;
LIN08          STX     M00DA           ;
               JSR     LIN17           ;
               LDX     TEMPD8D9        ;
               STX     TEMPC2C3        ;
LIN09          JSR     ZC490           ;
               BSR     LIN0a           ;
               LDAA    TEMPBCBD        ;
               BEQ     LIN0c           ;
               BSR     LIN10           ;
               LDX     TEMPB4B5        ;
               STX     TEMPD8D9        ;
               LDX     TEMPB6B7        ;
               BRA     LIN0d           ;
                                       ;
LIN0a          LDX     TEMPB4B5        ;
               BNE     LIN0b           ;
               LDX     TEMPB6B7        ;
               BNE     LIN0b           ;
               INS                     ;
               INS                     ;
LIN0b          RTS                     ;
                                       ;
LIN0c          BSR     LIN14           ;
               LDX     TEMPB6B7        ;
               STX     TEMPD8D9        ;
               LDX     TEMPB4B5        ;
LIN0d          STX     M00DA           ;
               BSR     LIN17           ;
               LDX     TEMPD8D9        ;
               CPX     TEMPC2C3        ;
               BMI     LIN0f           ;
               BRA     LIN09           ;
                                       ;
LIN0e          BSR     LIN10           ;
               BRA     LIN09           ;
                                       ;
LIN0f          LDAA    TEMPBCBD        ;
               BEQ     LIN0e           ;
               BSR     LIN14           ;
               BRA     LIN09           ;
                                       ;
LIN10          LDX     TEMPB4B5        ;
               BEQ     LIN13           ;
               DEX                     ;
               STX     TEMPB4B5        ;
               LDX     TEMPB0B1        ;
               LDAA    TEMPBEBF        ;
               BNE     LIN11           ;
               INX                     ;
               BRA     LIN12           ;
                                       ;
LIN11          DEX                     ;
LIN12          STX     TEMPB0B1        ;
LIN13          RTS                     ;
                                       ;
LIN14          LDX     TEMPB6B7        ;
               BEQ     LIN13           ;
               DEX                     ;
               STX     TEMPB6B7        ;
               LDX     TEMPB2B3        ;
               LDAA    TEMPBF          ;
               BNE     LIN15           ;
               INX                     ;
               BRA     LIN16           ;
                                       ;
LIN15          DEX                     ;
LIN16          STX     TEMPB2B3        ;
               RTS                     ;
                                       ;
LIN17          LDD     TEMPD8D9        ;
               ASLD                    ;
               ASLD                    ;
               ASLD                    ;
               ASLD                    ;
               ASLD                    ;
               ASLD                    ;
               STD     TEMPD8D9        ;
               LDAA    #$11            ;
               STAA    RXTXCNT         ;
               CLRA                    ;
               CLRB                    ;
LIN18          SUBD    M00DA           ;
               BCC     LIN19           ;
               ADDD    M00DA           ;
LIN19          ROL     >M00D9          ;
               ROL     >TEMPD8D9       ;
               ROLB                    ;
               ROLA                    ;
               DEC     >RXTXCNT        ;
               BNE     LIN18           ;
               COM     >M00D9          ;
               COM     >TEMPD8D9       ;
               RTS                     ;
                                       ;
ZC490          LDX     TEMPB0B1        ;
               STX     TXTBUFA0A1      ;
               LDX     TEMPB2B3        ;
               STX     TXTBUFA2A3      ;
               JMP     PUTPXL          ; actually put the pixel on screen
;--------------------------------------;
SETLINETERM    BRA     _WAITHNDSHK1_   ;
;--------------------------------------;
GRAMSETUP      TST     >GRAPHMOD       ; Check if Graphics Mode
               BEQ     LOCAL06         ; if not Return
ClearGRAM      LDX     #GRAPHICRAM     ;
               CLRA                    ;
LOOP07         STAA    ,X              ;
               INX                     ;
               CPX     #GRAMEND        ;
               BNE     LOOP07          ;
LOCAL06        RTS                     ;
;--------------------------------------;
CLRGRAPH       BSR     GRAMSETUP       ; EPSPFUNC: $ca
_WAITHNDSHK1_  JMP     WAITHNDSHK      ;
;--------------------------------------;
Graphic_CLR    BSR     GRAMSETUP       ; Char: $F6
_SENDRESP1_    JMP     SENDRESP        ;
;--------------------------------------;
Set_80Char     BSR     CONF80          ; Char: $F8
               BRA     _SENDRESP1_     ;
;--------------------------------------;
Set_72Char     LDAA    #$48            ; Char: $F7
               BSR     CONF72          ;
               CMPB    #$01            ;
               BEQ     _SENDRESP1_     ;
               LDX     #$0148          ;
               STX     M6845_0         ; Set Register 1 (Hor. Displayed) to 0x48 - 72
               LDX     #$0250          ;
               STX     M6845_0         ; Set Register 2 (Hor. Sync Pos.) to 0x50 - 80
               BRA     _SENDRESP1_     ;
;--------------------------------------;
CONF80         LDAA    #$50            ;
CONF72         LDAB    #$01            ;
               CMPB    GRAPHMOD        ; Check if Graphics Mode
               BEQ     _RTS2_          ; if yes Return
               LDX     #MC6845_7280Z   ;
W6845REG2      STAA    Char_per_Line   ; Akku can be 72 or 80
               LDAA    #$80            ;
W6845REG1      PSHA                    ;
               CLRB                    ;
LOCAL0c        STAB    M6845_0         ; Write all 16 Registers of 6845
               LDAA    ,X              ;
               STAA    M6845_1         ;
               INX                     ;
               INCB                    ;
               CMPB    #$10            ; Check if finished
               BNE     LOCAL0c         ;
               LDAA    P1DR            ;
               ANDA    #$7F            ; Mask P1.7 (HIGHRES) off
               PULB                    ; Holds $80
               ABA                     ;
               STAA    P1DR            ; Set HIGHRES
               JMP     RSTTXTSCRN      ;
;--------------------------------------;
DelLine        LDAB    Char_per_Line   ; Char: $04 and $05
               SUBB    CursorColumn    ;
               BSR     ZC50B           ;
               BRA     _SENDRESP2_     ;
;--------------------------------------;
CLRLINE        LDAB    Char_per_Line   ; belongs to SCRLUP
               ADDB    Char_per_Line   ; two lines
               SUBB    #$20            ; minus 32 characters
ZC50B          CLRA                    ;
               LDX     RAMTXTSTART     ;
ZC50E          STAA    ,X              ; Clear the next ~1.5 Lines
               INX                     ;
               DECB                    ;
               BNE     ZC50E           ;
_RTS2_         RTS                     ;
;--------------------------------------;
CRSRLFT        DEC     >CursorColumn   ;
               BPL     ZC524           ;
               LDAA    Char_per_Line   ;
               DECA                    ;
               STAA    CursorColumn    ;
               DEC     >CursorRow      ;
               BMI     ZC556           ;
ZC524          JMP     SET6845ADD      ;
;--------------------------------------;
DeleteChar     LDX     RAMTXTSTART     ; Char: $08
               DEX                     ;
               LDAB    ,X              ;
               BEQ     _SENDRESP2_     ;
               BSR     CRSRLFT         ;
               LDX     RAMTXTSTART     ;
ZC532          LDAB    $01,X           ;
               STAB    ,X              ;
               INX                     ;
               CMPB    #$00            ;
               BNE     ZC532           ;
               BRA     _SENDRESP2_     ;
;--------------------------------------;
TABHOR         LDAB    CursorColumn    ; Char: $09
ZC53F          LDAA    #$08            ;
               SUBB    #$08            ;
               BPL     ZC53F           ;
               ADDB    #$08            ;
               SBA                     ;
               ADDA    CursorColumn    ;
               CMPA    Char_per_Line   ;
               BNE     ZC552           ;
               CLRA                    ;
               INC     >CursorRow      ;
ZC552          STAA    CursorColumn    ;
               BRA     _SENDRESP2_     ;
;--------------------------------------;
ZC556          INS                     ;
               INS                     ;
;--------------------------------------;
Home           LDX     #$0000          ; Char: $0B
               STX     TMPCRSRRW       ;
               STX     CursorColumn    ;
               BRA     _SENDRESP2_     ;
;--------------------------------------;
ClearScreen    JSR     RSTTXTSCRN      ; Char: $0C
               BRA     _SENDRESP2_     ;
;--------------------------------------;
Enter          CLR     >CursorColumn   ; Char: $01 and $0D
_SENDRESP2_    JMP     SENDRESP        ;
;--------------------------------------;
ZC56C          BSR     SCRLUP          ;
_SENDRESP6_    BRA     _SENDRESP2_     ;
;--------------------------------------;
; SCRLUP: 
; Moves Window up one Line, deletes the next ~1.5 Lines in Memory, adjusts 6845 Start Address and RAM Start Addr.
; CursorRow must be $18 to Scroll, Clears CursorColumn
; Terminates with RTS
;
SCRLUP         LDX     _6845STRTADRH   ;
               LDAB    Char_per_Line   ;
               ABX                     ;
               STX     _6845STRTADRH   ; add Char_per_Line to _6845STRTADRH
               LDX     RAMTXTSTART     ;
               ABX                     ;
               INC     >CursorRow      ;
               LDAA    #$18            ; load last row (=24)
               CMPA    CursorRow       ;
               BGE     _RTS2_          ; no scroll below 24
               STAA    CursorRow       ; limit CursorRow to $18
               CLR     >CursorColumn   ;
               STX     RAMTXTSTART     ; add Char_per_Line to RAMTXTSTART
               LDAA    P1DR            ;
               ANDA    #$BF            ; Clear T/G (P1.6)
               STAA    P1DR            ;
               LDAA    _6845STRTADRH   ;
               ANDA    #$07            ; Limit to 7FF
               STAA    _6845STRTADRH   ;
               LDAA    RAMTXTSTART     ;
               ANDA    #$17            ; limit to $17FF
               STAA    RAMTXTSTART     ;
               JSR     SET6845ADD      ;
               JMP     CLRLINE         ; clear the next ~1.5 Lines
;--------------------------------------; belongs to MovCRSR_Up
; SCRLDWN:
; doesn't actually scroll anywhere, it just moves the cursor up until first line
;
SCRLDWN        LDD     _6845STRTADRH   ;
               BEQ     _SENDRESP6_     ;
               LDAA    CursorRow       ;
               DECA                    ;
               STAA    CursorRow       ;
               SUBB    Char_per_Line   ;
               SBCA    #$00            ;
               STD     _6845STRTADRH   ;
               BRA     _SENDRESP6_     ;
;--------------------------------------;
ToggleInsertMd LDX     RAMTXTSTART     ; Char: $12
ZC5B5          INX                     ;
               LDAA    ,X              ;
               BNE     ZC5B5           ;
               STAA    $01,X           ;
ZC5BC          DEX                     ;
               LDAA    ,X              ;
               STAA    $01,X           ;
               CPX     RAMTXTSTART     ;
               BGE     ZC5BC           ;
               LDAA    #$20            ;
               STAA    $01,X           ;
               JMP     SENDTX2         ;
;--------------------------------------;
Set_CRSR_Blink LDAA    _6845CRSRREG    ; Char: $F5
               EORA    #$60            ;
               STAA    _6845CRSRREG    ;
;--------------------------------------;
Set_CRSR_On    LDAA    _6845CRSRREG    ; Char: $16
               BRA     LOCAL05         ;
;--------------------------------------;
Set_CRSR_Off   LDAA    #$20            ; Char: $17
LOCAL05        BSR     Write_CRSR_Reg  ;
_SENDRESP3_    JMP     SENDRESP        ;
;--------------------------------------;
DelScreen      BSR     CLRSCRN         ; Char: $1A
_SENDRESP7_    BRA     _SENDRESP3_     ;
;--------------------------------------;
CLRSCRN        LDAA    RAMTXTSTART     ;
               ANDA    #$17            ; limit to 17FF (now $1FFF)
               STAA    RAMTXTSTART     ;
               LDX     RAMTXTSTART     ;
               CLRA                    ;
LOCAL02        STAA    ,X              ;
               INX                     ;
               CPX     #TRAMEND        ; End of TRAM (Ch. from 0x1800 to 0x2000)
               BNE     LOCAL02         ;
               RTS                     ;
;--------------------------------------;
MovCRSR_Rgt    INC     >CursorColumn   ; Char: $1C
               LDAB    Char_per_Line   ;
               CMPB    CursorColumn    ;
               BNE     _SENDRESP7_     ;
               CLR     >CursorColumn   ;
               INC     >CursorRow      ;
               BRA     _SENDRESP7_     ;
;--------------------------------------;
MovCRSR_Lft    JSR     CRSRLFT         ; Char: $1D
               BRA     _SENDRESP7_     ;
;--------------------------------------;
MovCRSR_Up     LDAA    CursorRow       ; Char: $10 and $1E
               BEQ     _SENDRESP7_     ;
               CMPA    #$19            ;
               BMI     ZC614           ;
               JMP     SCRLDWN         ;
                                       ;
ZC614          DECA                    ;
ZC615          STAA    CursorRow       ;
               BRA     _SENDRESP7_     ;
;--------------------------------------;
Linefeed       LDAA    M00D1           ; Char: $0A
               BEQ     MovCRSR_Dwn     ;
               CLR     >M00D1          ;
               LDAA    CursorRow       ;
               CMPA    #$18            ;
               BMI     _SENDRESP7_     ;
;--------------------------------------;
MovCRSR_Dwn    LDAA    CursorRow       ; Char: $11 and $1F
               CMPA    #$18            ; compare with last line (=24)
               BMI     ZC62F           ;
               JMP     ZC56C           ; if in last line: jump to SCRLUP + SENDRESP
                                       ;
ZC62F          INCA                    ; not in last line
               BRA     ZC615           ; STA CursorRow + SENDRESP
;--------------------------------------; belongs to Set_CRSR_Off
Write_CRSR_Reg LDAB    #$0A            ;
               STAB    M6845_0         ;
               STAA    M6845_1         ;
_RTS1_         RTS                     ;
;--------------------------------------;
Set_40Char     BSR     CONF40          ; Char: $F4
_SENDRESP8_    BRA     _SENDRESP7_     ;
;--------------------------------------;
Set_32Char     LDAA    #$20            ; Char: $F3
               BSR     CONF32          ;
               CMPB    #$01            ;
               BEQ     _SENDRESP7_     ;
               LDX     #$0120          ;  Set Register 1 (Hor. Displayed) to 0x20 - 32
               STX     M6845_0         ;
               LDX     #$0228          ;  Set Register 2 (Hor. Sync Pos.) to 0x28 - 40
               STX     M6845_0         ;
_SENDRESP9_    BRA     _SENDRESP8_     ;
;--------------------------------------;
SendPKT_00     FCB     $02,$00,$03,$FB ;
SCRNSZ8025     FCB     $02,$50,$19,$03,$92 ; Message Packet for 80x25 Screen Size
MC6845_7280Z   FCB     $62             ; Set Register 0 (H Total) - 98
               FCB     $50             ; Set Register 1 (H Displayed) - 80
               FCB     $54             ; Set Register 2 (H Sync. Pos.) - 84
               FCB     $29             ; Set Register 3 (Sync. Width) - 41
               FCB     $1E             ; Set Register 4 (V Total) - 30
               FCB     $00             ; Set Register 5 (V Total Adj.)
               FCB     $19             ; Set Register 6 (V Displayed) - 25
               FCB     $1B             ; Set Register 7 (V Sync. Pos.) - 27
               FCB     $A2             ; Set Register 8 (Interlace Md. and Skew) - 162
               FCB     $09             ; Set Register 9 (Max. Scanline Address)
               FCB     $00             ; Set Register 10 (Crsr Start)
               FCB     $10             ; Set Register 11 (Crsr End) - 16
               FCB     $00             ; Set Register 12 (Start Address H)
               FCB     $00             ; Set Register 13 (Start Address L)
               FCB     $00             ; Set Register 14 (Crsr H)
               FCB     $00             ; Set Register 15 (Crsr L)
;--------------------------------------;
CONF40         LDAA    #$28            ;
CONF32         LDAB    GRAPHMOD        ;
               CMPB    #$01            ; Check if Graphics Mode
               BEQ     _RTS1_          ; if yes Return
               LDX     #MC6845_3240Z   ;
               STAA    Char_per_Line   ; Akku can be 32, 40
               CLRA                    ;
               JMP     W6845REG1       ;
;--------------------------------------;
MC6845_3240Z   FCB     $30             ; Set Register 0 (H Total) - 48
               FCB     $28             ; Set Register 1 (H Displayed) - 40
               FCB     $2B             ; Set Register 2 (H Sync. Pos.) - 43
               FCB     $43             ; Set Register 3 (Sync. Width) - 67
               FCB     $1D             ; Set Register 4 (V Total) - 29
               FCB     $0C             ; Set Register 5 (V Total Adj.) - 12
               FCB     $19             ; Set Register 6 (V Displayed) - 25
               FCB     $1B             ; Set Register 7 (V Sync. Pos.) - 27
               FCB     $A0             ; Set Register 8 (Interlace Md. and Skew) - 160
               FCB     $09             ; Set Register 9 (Max. Scanline Address)
               FCB     $00             ; Set Register 10 (Crsr Start)
               FCB     $10             ; Set Register 11 (Crsr End) - 16
               FCB     $00             ; Set Register 12 (Start Address H)
               FCB     $00             ; Set Register 13 (Start Address L)
               FCB     $00             ; Set Register 14 (Crsr H)
               FCB     $00             ; Set Register 15 (Crsr L)
MC6845_GRAPH   FCB     $62             ; Set Register 0 (H Total) - 98
               FCB     $50             ; Set Register 1 (H Displayed) - 80
               FCB     $53             ; Set Register 2 (H Sync. Pos.) - 83
               FCB     $29             ; Set Register 3 (Sync. Width) - 41
               FCB     $26             ; Set Register 4 (V Total) - 38
               FCB     $00             ; Set Register 5 (V Total Adj.)
               FCB     $19             ; Set Register 6 (V Displayed) - 25
               FCB     $1F             ; Set Register 7 (V Sync. Pos.) - 31
               FCB     $A0             ; Set Register 8 (Interlace Md. and Skew) - 160
               FCB     $07             ; Set Register 9 (Max. Scanline Address)
               FCB     $00             ; Set Register 10 (Crsr Start)
               FCB     $07             ; Set Register 11 (Crsr End)
               FCB     $00             ; Set Register 12 (Start Address H)
               FCB     $00             ; Set Register 13 (Start Address L)
               FCB     $00             ; Set Register 14 (Crsr H)
               FCB     $00             ; Set Register 15 (Crsr L)
HDRDAT01       FCB     $01,$01         ;
HDRDAT02       FCB     $20,$30         ;
;--------------------------------------;
CTRLTAB        FCB     $01             ;
               FDB     Enter           ;
               FCB     $0E             ; CTRL + N - ?
               FDB     SPCLCHAR        ;
               FCB     $04             ; CTRL + D or -> - Move Cursor
               FDB     DelLine         ;
               FCB     $05             ; CTRL + E - Delete to EOL
               FDB     DelLine         ;
               FCB     $06             ; CTRL + F - Cursor to right edge
               FDB     TGLINVERS       ;
               FCB     $07             ; CTRL + G - ? Make Sound (in MH20 Emulator)
               FDB     TGLINTENS       ;
               FCB     $08             ; CTRL + H - Delete one char.
               FDB     DeleteChar      ;
               FCB     $09             ; CTRL + I - Hor. TAB
               FDB     TABHOR          ;
               FCB     $0A             ; CTRL + J - Linefeed
               FDB     Linefeed        ;
               FCB     $0B             ; CTRL + K - Home
               FDB     Home            ;
               FCB     $0C             ; CTRL + L - Clear Screen
               FDB     ClearScreen     ;
               FCB     $0D             ; CTRL + M - Enter
               FDB     Enter           ;
               FCB     $10             ; CTRL + P - Move Window up - new functions
               FDB     MovCRSR_Up      ;
               FCB     $11             ; CTRL + Q - Move Window down - new functions
               FDB     MovCRSR_Dwn     ;
               FCB     $12             ; CTRL + R - Toggle Insert Mode
               FDB     ToggleInsertMd  ;
               FCB     $16             ; CTRL + V - Make Cursor visible
               FDB     Set_CRSR_On     ;
               FCB     $17             ; CTRL + W - Make Cursor invisible
               FDB     Set_CRSR_Off    ;
               FCB     $1A             ; CTRL + Z - Delete Cursor to end of screen
               FDB     DelScreen       ;
               FCB     $1C             ; CTRL + \ - Move Cursor right
               FDB     MovCRSR_Rgt     ;
               FCB     $1D             ; CTRL + ] - Move Cursor left
               FDB     MovCRSR_Lft     ;
               FCB     $1E             ; CTRL + ^ - Move Cursor up
               FDB     MovCRSR_Up      ;
               FCB     $1F             ; CTRL + _ - Move Cursor down
               FDB     MovCRSR_Dwn     ;
               FCB     $F0             ; CTRL + 0 - TestPicture (Graphic Mode) (Enter Screen1,1, then CTRL+0)
               FDB     TestPicture     ;
               FCB     $F1             ; CTRL + 1 - Text On/Off
               FDB     TEXTON_OFF      ;
               FCB     $F2             ; CTRL + 2 - Inverted Text
               FDB     INVERTTEXT      ;
               FCB     $F3             ; CTRL + 3 - 32 Characters
               FDB     Set_32Char      ;
               FCB     $F4             ; CTRL + 4 - 40 Characters
               FDB     Set_40Char      ;
               FCB     $F5             ; CTRL + 5 - Cursor Blink
               FDB     Set_CRSR_Blink  ;
               FCB     $F6             ; CTRL + 6 - Graphics Clear Screen
               FDB     Graphic_CLR     ;
               FCB     $F7             ; CTRL + 7 - 72 Characters
               FDB     Set_72Char      ;
               FCB     $F8             ; CTRL + 8 - 80 Characters
               FDB     Set_80Char      ;
               FCB     $F9             ; CTRL + 9 - second Charset
               FDB     SwitchCharset   ;
SwitchCharset  LDAA    P1DR            ;
               EORA    #$20            ; Toggle Charset (P1.5)
;--------------------------------------;
ZC707          STAA    P1DR            ;
_SENDRESP4_    JMP     SENDRESP        ;
;--------------------------------------;
TGLINTENS      LDAA    P1DR            ; Char: $07, Toggle INTENSITY (P1.2 - Pin15), Set INVERSE (P1.3 - Pin16)
               ORAA    #$08            ; 
               EORA    #$04            ;
               BRA     ZC707           ;
;--------------------------------------;
TGLINVERS      LDAA    P1DR            ; Char: $06, Toggle INVERSE (P1.3 - Pin16), Set INTENSITY (P1.2 - Pin15)
               ORAA    #$04            ;
               EORA    #$08            ;
               BRA     ZC707           ;
;--------------------------------------;
SPCLCHAR       LDAA    #$80            ; Char: $0E, Sets Bit 7 of the next character
               EORA    SETB7           ;
               STAA    SETB7           ;
               BRA     _SENDRESP4_     ;
;-----------------------0123456789012345678901234567890123456789
GREETING       FCC     'Bitte, "SCREEN 1,0" eingeben!'
               FCB     $04

;--------------------------------------;
EPSPFUNC       FCB     $88             ; READ SCREEN SIZE (L.C)
               FDB     SCRNSIZE        ;
               FCB     $89             ; GET PHYSICAL SCREEN SIZE (L.C)
               FDB     SCRNSIZE        ;
               FCB     $8C             ; GET CURSOR POSITION
               FDB     GCRSRPOS        ;
               FCB     $8F             ; GET POINT ON THE DISPLAY (L.C)
               FDB     GETPOINT        ;
               FCB     $91             ; READ THE EXTEND OF CURRENT LINE (L.C)
               FDB     READLINE        ;
               FCB     $92             ; WRITE ONE CHARACTER TO VIRTUAL SCREEN (L.C)
               FDB     WRITECHAR       ;
               FCB     $93             ; SET DISPLAY MODE (C)
               FDB     SETDISPMOD      ;
               FCB     $97             ; READ CHARACTERS FROM VS. (L,C)
               FDB     READCHAR        ;
               FCB     $98             ; WRITE ONE CHARACTER TO VS AND GET EXTEND OF NEW CURSOR POSITION.(L,C)
               FDB     WRITECHAR       ;
               FCB     $C2             ; SET CURSOR POSITION ON THE VIRTUAL SCREEN (L.C)
               FDB     SETCRSR         ;
               FCB     $C5             ; SET LIST FLAG (L.C)
               FDB     SRLISTFLG       ;
               FCB     $C6             ; RESET LIST FLAG (L.C)
               FDB     SRLISTFLG       ;
               FCB     $C7             ; SET POINT TO DISPLAY (L.C)
               FDB     SETPOINT        ;
               FCB     $C8             ; DRAW LINE TO DISPLAY (L.C)
               FDB     DRAWLINE        ;
               FCB     $C9             ; SET LINE TERMINATE POSITION (L.C)
               FDB     SETLINETERM     ;
               FCB     $CA             ; CLEAR THE GRAPHICS SCREEN. (L.C)
               FDB     CLRGRAPH        ;
               FCB     $CB             ; SET SCRLUPSPEED (C)
               FDB     DISABLED        ;
               FCB     $CF             ; COLOR SET SELECT (C) 
               FDB     DISABLED        ;
               FCB     $D4             ; ?
               FDB     DISABLED        ;
EPSEND         FCB     $01             ;
;--------------------------------------;
TestPicture    TST     >GRAPHMOD       ; Char: $F0, Check if Graphics Mode
               BEQ     _SENDRESP5_     ; if no Return
               JSR     GRAMSETUP       ;
               LDAA    #$0D            ;
               STAA    GRAPHMOD        ;
               LDX     #DATATAB        ;
               STX     TEMP16_01       ;
TP01           LDX     TEMP16_01       ;
               LDD     ,X              ;
               STD     TXTBUFA0A1      ;
               LDD     $02,X           ;
               STD     TXTBUFA2A3      ;
               LDD     $04,X           ;
               STD     TXTBUFA4A5      ;
               LDD     $06,X           ;
               STD     TXTBUFA6A7      ;
               LDAA    #$01            ;
               STAA    TXTBUFA8A9      ;
               LDAB    #$04            ;
               ABX                     ; increase 4 words
               STX     TEMP16_01       ;
               JSR     DRWLN           ;
               DEC     >GRAPHMOD       ;
               BNE     TP01            ;
               INC     >GRAPHMOD       ;
_SENDRESP5_    JMP     SENDRESP        ;
;--------------------------------------;
;                         X0      Y0  ->  X1      Y1   ; Line from X1Y1 to X2Y2
;                       A0  A1  A2  A3  A4  A5  A6  A7
DATATAB        FCB     $00,$B4,$00,$00 ; 0 (180,0)
               FCB     $00,$B4,$00,$BF ; 1 (180,191)
               FCB     $01,$2C,$00,$BF ; 2 (300,191)
               FCB     $01,$2C,$00,$00 ; 3 (300,0)
               FCB     $00,$73,$00,$00 ; 4 (115,0)
               FCB     $00,$73,$00,$32 ; 5 (115,50)
               FCB     $01,$6D,$00,$32 ; 6 (365,50)
               FCB     $01,$6D,$00,$8D ; 7 (365,141)
               FCB     $00,$73,$00,$8D ; 8 (115,141)
               FCB     $00,$73,$00,$BF ; 9 (115,191)
               FCB     $01,$6D,$00,$BF ; 10 (365,191)
               FCB     $01,$6D,$00,$00 ; 11 (365,0)
               FCB     $00,$73,$00,$00 ; 12 (115,0)
               FCB     $00,$73,$00,$BF ; 13 (115,191)
               FCB     $77

hdlr_NMI       RTI                              ;C7EF: 3B
unused         FCB     $C7,$EF,$C7,$EF,$C7,$EF,$C7,$EF
               FCB     $C7,$EF,$C7,$EF,$C7,$EF,$C0,$00
        ORG     $ffF0

svec_IRQ_SCI FDB     hdlr_NMI                 ;FFF0: C7 EF
svec_IRQ_T0F FDB     hdlr_NMI                 ;FFF2: C7 EF
svec_IRQ_OCF FDB     hdlr_NMI                 ;FFF4: C7 EF
svec_IRQ_ICF FDB     hdlr_NMI                 ;FFF6: C7 EF
svec_IRQ_EXT FDB     hdlr_NMI                 ;FFF8: C7 EF
svec_SWI FDB     hdlr_NMI                 ;FFFA: C7 EF
svec_NMI FDB     hdlr_NMI                 ;FFFC: C7 EF
svec_RST FDB     hdlr_RST                 ;FFFE: C0 00

        END
