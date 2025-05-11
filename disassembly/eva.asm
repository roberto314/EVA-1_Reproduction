f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V1.80
Loaded binary file eva_34swap.bin

;****************************************************
;* Used Labels                                      *
;****************************************************

P1DDR   EQU     $0000
P2DDR   EQU     $0001
P1DR    EQU     $0002
P2DR    EQU     $0003
CNTH    EQU     $0009
RMCR    EQU     $0010
TRCSR   EQU     $0011
RDR     EQU     $0012
TDR     EQU     $0013
RCR     EQU     $0014
M0080   EQU     $0080
M0082   EQU     $0082
M0083   EQU     $0083
M0084   EQU     $0084
M0085   EQU     $0085
StartAddressH EQU     $0098
StartAddressL EQU     $0099
CursorColumn EQU     $009A
CursorRow EQU     $009B
CursorH EQU     $009C
CursorL EQU     $009D
FunctionCode EQU     $00A0
M00A1   EQU     $00A1
M00A2   EQU     $00A2
M00A3   EQU     $00A3
M00A4   EQU     $00A4
M00A6   EQU     $00A6
M00A8   EQU     $00A8
M00AC   EQU     $00AC
M00AE   EQU     $00AE
M00AF   EQU     $00AF
M00B0   EQU     $00B0
M00B2   EQU     $00B2
M00B4   EQU     $00B4
M00B6   EQU     $00B6
M00B8   EQU     $00B8
M00BA   EQU     $00BA
M00BC   EQU     $00BC
M00BE   EQU     $00BE
M00BF   EQU     $00BF
M00C1   EQU     $00C1
M00C2   EQU     $00C2
M00C4   EQU     $00C4
M00C5   EQU     $00C5
CursorStart EQU     $00C6
M00C7   EQU     $00C7
M00C8   EQU     $00C8
M00CA   EQU     $00CA
TextPosition EQU     $00CC
M00CE   EQU     $00CE
M00D0   EQU     $00D0
M00D1   EQU     $00D1
Char_per_Line EQU     $00D2
M00D3   EQU     $00D3
M00D4   EQU     $00D4
M00D8   EQU     $00D8
M00D9   EQU     $00D9
M00DA   EQU     $00DA
RAMEND  EQU     $00FF
M01DF   EQU     $01DF
TEXTRAM EQU     $1000
Gr_TXT_Pos EQU     $139C
M1800   EQU     $1800
M6845_0 EQU     $4000
M6845_1 EQU     $4001
GRAPHICRAM EQU     $8000

;****************************************************
;* Program Code / Data Areas                        *
;****************************************************

        ORG     $C000

hdlr_RST SEI                              ;C000: 0F 
        LDS     #RAMEND                  ;C001: 8E 00 FF 
        CLRB                             ;C004: 5F 
        LDAA    #$FF                     ;C005: 86 FF 
        STAA    M00C7                    ;C007: 97 C7 
        STAA    P1DDR                    ;C009: 97 00 
        STAB    P2DDR                    ;C00B: D7 01 
        LDAA    #$0F                     ;C00D: 86 0F 
        STAA    P1DR                     ;C00F: 97 02          Clear INVERT, TXTON_OFF, GTEXT, TEXT (P1.0-P1.3), Set CHARSET, T/G, HIGHRES (P1.4-P1.7)
        STAB    M00D3                    ;C011: D7 D3 
        STAB    P2DR                     ;C013: D7 03 
        STAB    CursorStart              ;C015: D7 C6 
        LDAA    #$0C                     ;C017: 86 0C 
        STAA    RMCR                     ;C019: 97 10          CC0 and CC1 set - external Serial clock (8xBaudrate)
        LDAB    #$0A                     ;C01B: C6 0A 
        STAB    TRCSR                    ;C01D: D7 11          Receiver and Transmitter Enable
        LDAA    #$C0                     ;C01F: 86 C0 
        STAA    RCR                      ;C021: 97 14          enable internal RAM 
        JSR     PreloadA_40              ;C023: BD C6 6E 
        JSR     ClearGRAM                ;C026: BD C4 A2 
        LDS     #GrTXT                   ;C029: 8E C7 23       Write Text to TextRAM
        LDX     #Gr_TXT_Pos              ;C02C: CE 13 9C 
ZC02F   PULA                             ;C02F: 32 
        STAA    ,X                       ;C030: A7 00 
        INX                              ;C032: 08 
        CMPA    #$21                     ;C033: 81 21 
        BNE     ZC02F                    ;C035: 26 F8 
        LDS     #RAMEND                  ;C037: 8E 00 FF 
ZC03A   LDX     TRCSR                    ;C03A: DE 11 
CheckRS232Data LDAA    TRCSR                    ;C03C: 96 11 
        BITA    #$40                     ;C03E: 85 40 
        BNE     ZC03A                    ;C040: 26 F8 
        LDAB    RDR                      ;C042: D6 12 
        CMPB    #$31                     ;C044: C1 31 
        BNE     CheckRS232Data           ;C046: 26 F4          Check 'START? (31)'
        BSR     CkRXReg                  ;C048: 8D 1A 
        CMPB    #$30                     ;C04A: C1 30 
        BNE     CheckRS232Data           ;C04C: 26 EE          Check 'DID (30)'
        BSR     CkRXReg                  ;C04E: 8D 14 
        CMPB    #$20                     ;C050: C1 20 
        BNE     CheckRS232Data           ;C052: 26 E8          Check 'SID (20)'
        BSR     CkRXReg                  ;C054: 8D 0E 
        CMPB    #$05                     ;C056: C1 05 
        BNE     CheckRS232Data           ;C058: 26 E2          Check for 'ENQ (05)'
        BSR     Send_ACK                 ;C05A: 8D 53 
        BSR     CkRXReg                  ;C05C: 8D 06 
        CMPB    #$01                     ;C05E: C1 01 
        BEQ     GotSOH                   ;C060: 27 34          Check 'SOH (01)'
        BRA     ZC0AA                    ;C062: 20 46 
CkRXReg TST     >TRCSR                   ;C064: 7D 00 11 
        BPL     CkRXReg                  ;C067: 2A FB 
        LDAB    RDR                      ;C069: D6 12 
        RTS                              ;C06B: 39 
ZC06C   BSR     CkRXReg                  ;C06C: 8D F6 
        CMPB    #$02                     ;C06E: C1 02          Check 'STX (02)'
        BNE     ZC0AA                    ;C070: 26 38 
        LDX     #FunctionCode            ;C072: CE 00 A0 
        TBA                              ;C075: 17 
        LDAB    M0084                    ;C076: D6 84 
        INCB                             ;C078: 5C 
        STAB    M00C1                    ;C079: D7 C1 
ZC07B   BSR     CkRXReg                  ;C07B: 8D E7 
        STAB    ,X                       ;C07D: E7 00 
        ABA                              ;C07F: 1B 
        INX                              ;C080: 08 
        DEC     >M00C1                   ;C081: 7A 00 C1 
        BNE     ZC07B                    ;C084: 26 F5 
        BSR     CkRXReg                  ;C086: 8D DC 
        CMPB    #$03                     ;C088: C1 03          Check 'ETX (03)'
        BNE     Send_NACK                ;C08A: 26 06 
        ABA                              ;C08C: 1B 
        BSR     CkRXReg                  ;C08D: 8D D5 
        ABA                              ;C08F: 1B 
        BEQ     Send_ACK                 ;C090: 27 1D 
Send_NACK LDAB    #$15                     ;C092: C6 15 
        BRA     SendB                    ;C094: 20 1B 
GotSOH  TBA                              ;C096: 17 
        LDX     #M0080                   ;C097: CE 00 80 
ZC09A   BSR     CkRXReg                  ;C09A: 8D C8 
        STAB    ,X                       ;C09C: E7 00 
        ABA                              ;C09E: 1B 
        INX                              ;C09F: 08 
        CPX     #M0085                   ;C0A0: 8C 00 85 
        BNE     ZC09A                    ;C0A3: 26 F5 
        BSR     CkRXReg                  ;C0A5: 8D BD 
        ABA                              ;C0A7: 1B 
        BEQ     ZC0BA                    ;C0A8: 27 10 
ZC0AA   BSR     Send_NACK                ;C0AA: 8D E6 
        JMP     CheckRS232Data           ;C0AC: 7E C0 3C 
Send_ACK LDAB    #$06                     ;C0AF: C6 06 
SendB   LDAA    TRCSR                    ;C0B1: 96 11 
        ANDA    #$20                     ;C0B3: 84 20          Check TX Reg. Empty
        BEQ     SendB                    ;C0B5: 27 FA 
        STAB    TDR                      ;C0B7: D7 13 
        RTS                              ;C0B9: 39 
ZC0BA   BSR     Send_ACK                 ;C0BA: 8D F3 
        BSR     ZC06C                    ;C0BC: 8D AE 
        CLR     >M0085                   ;C0BE: 7F 00 85 
        BSR     CkRXReg                  ;C0C1: 8D A1 
        CMPB    #$04                     ;C0C3: C1 04 
        BNE     ZC0AA                    ;C0C5: 26 E3 
        LDAA    M0083                    ;C0C7: 96 83 
        LDX     #MC77B                   ;C0C9: CE C7 7B 
        STX     M00CA                    ;C0CC: DF CA 
        LDX     #MC742                   ;C0CE: CE C7 42 
        JSR     ZC240                    ;C0D1: BD C2 40 
        BRA     ZC0AA                    ;C0D4: 20 D4 
ZC0D6   LDAA    #$06                     ;C0D6: 86 06 
        STAA    M00C1                    ;C0D8: 97 C1 
        LDAA    M0083                    ;C0DA: 96 83 
        STAA    M0084                    ;C0DC: 97 84 
        LDX     MC69F                    ;C0DE: FE C6 9F 
        STX     M0080                    ;C0E1: DF 80 
        LDX     MC6A1                    ;C0E3: FE C6 A1 
        STX     M0082                    ;C0E6: DF 82 
        LDX     #M0080                   ;C0E8: CE 00 80 
ZC0EB   CLRA                             ;C0EB: 4F 
ZC0EC   LDAB    ,X                       ;C0EC: E6 00 
        ANDB    M00C7                    ;C0EE: D4 C7 
        ABA                              ;C0F0: 1B 
        PSHA                             ;C0F1: 36 
        BSR     SendB                    ;C0F2: 8D BD 
        PULA                             ;C0F4: 32 
        INX                              ;C0F5: 08 
        DEC     >M00C1                   ;C0F6: 7A 00 C1 
        BNE     ZC0EC                    ;C0F9: 26 F1 
        NEGA                             ;C0FB: 40 
        TAB                              ;C0FC: 16 
        BSR     SendB                    ;C0FD: 8D B2 
        JSR     CkRXReg                  ;C0FF: BD C0 64 
        LDAA    #$FF                     ;C102: 86 FF 
        STAA    M00C7                    ;C104: 97 C7 
        RTS                              ;C106: 39 
        LDX     #SendPKT_00              ;C107: CE C6 55 
        LDAA    #$01                     ;C10A: 86 01 
Send_Packet ADDA    #$03                     ;C10C: 8B 03 
        STAA    M00C1                    ;C10E: 97 C1 
ZC110   LDAB    ,X                       ;C110: E6 00 
        JSR     SendB                    ;C112: BD C0 B1 
        INX                              ;C115: 08 
        DEC     >M00C1                   ;C116: 7A 00 C1 
        BNE     ZC110                    ;C119: 26 F5 
        JSR     CkRXReg                  ;C11B: BD C0 64 
Send_EOT LDAB    #$04                     ;C11E: C6 04 
        JSR     SendB                    ;C120: BD C0 B1 
        BRA     GRPH_cbcfd4              ;C123: 20 06 
        CLR     >M00D3                   ;C125: 7F 00 D3 
        JSR     ZC4D3                    ;C128: BD C4 D3 
GRPH_cbcfd4 JMP     CheckRS232Data           ;C12B: 7E C0 3C 
ResetTextScreen LDX     #$0000                   ;C12E: CE 00 00 
        STX     M00D0                    ;C131: DF D0 
        STX     M00C4                    ;C133: DF C4 
        STX     StartAddressH            ;C135: DF 98 
        STX     CursorColumn             ;C137: DF 9A 
        STX     CursorH                  ;C139: DF 9C 
        LDX     #TEXTRAM                 ;C13B: CE 10 00 
        STX     TextPosition             ;C13E: DF CC 
        CLRA                             ;C140: 4F 
ClearTRAM STAA    ,X                       ;C141: A7 00 
        INX                              ;C143: 08 
        CPX     #M1800                   ;C144: 8C 18 00 Changed to 2000
        BNE     ClearTRAM                ;C147: 26 F8 
        RTS                              ;C149: 39 
GRPH_Comma LDAA    M00A2                    ;C14A: 96 A2 
        STAA    M00CE                    ;C14C: 97 CE 
        STAA    M0085                    ;C14E: 97 85 
        JSR     ZC0D6                    ;C150: BD C0 D6 
        LDX     FunctionCode             ;C153: DE A0 
        STX     CursorColumn             ;C155: DF 9A 
        JSR     ZC288                    ;C157: BD C2 88 
        LDX     TextPosition             ;C15A: DE CC 
        INX                              ;C15C: 08 
        LDAB    M00CE                    ;C15D: D6 CE 
        ABX                              ;C15F: 3A 
        LDAA    ,X                       ;C160: A6 00 
        STAA    M00D4                    ;C162: 97 D4 
        STX     M00B0                    ;C164: DF B0 
        LDAA    #$03                     ;C166: 86 03 
        STAA    ,X                       ;C168: A7 00 
        ADDB    #$02                     ;C16A: CB 02 
        STAB    M00C1                    ;C16C: D7 C1 
        LDX     TextPosition             ;C16E: DE CC 
        LDAB    #$02                     ;C170: C6 02 
        JSR     SendB                    ;C172: BD C0 B1 
        TBA                              ;C175: 17 
        LDAB    #$7F                     ;C176: C6 7F 
        STAB    M00C7                    ;C178: D7 C7 
        JSR     ZC0EC                    ;C17A: BD C0 EC 
        LDX     M00B0                    ;C17D: DE B0 
        LDAA    M00D4                    ;C17F: 96 D4 
        STAA    ,X                       ;C181: A7 00 
        BRA     Send_EOT                 ;C183: 20 99 
GRPH_EZ BSR     ZC18F                    ;C185: 8D 08 
        LDX     #SendPKT_50_19           ;C187: CE C6 59 
        LDAA    #$02                     ;C18A: 86 02 
        JMP     Send_Packet              ;C18C: 7E C1 0C 
ZC18F   LDAA    #$01                     ;C18F: 86 01 
ZC191   STAA    M0085                    ;C191: 97 85 
        JMP     ZC0D6                    ;C193: 7E C0 D6 
GRPH_F  LDAA    M00D0                    ;C196: 96 D0 
        STAA    M00D1                    ;C198: 97 D1 
        LDAA    CursorColumn             ;C19A: 96 9A 
        INCA                             ;C19C: 4C 
        STAA    M00D0                    ;C19D: 97 D0 
ZC19F   BSR     ZC18F                    ;C19F: 8D EE 
        LDX     CursorColumn             ;C1A1: DE 9A 
        STX     M00A1                    ;C1A3: DF A1 
        LDAA    #$03                     ;C1A5: 86 03 
        STAA    M00A3                    ;C1A7: 97 A3 
        LDAA    #$02                     ;C1A9: 86 02 
ZC1AB   ADDA    #$02                     ;C1AB: 8B 02 
        STAA    M00C1                    ;C1AD: 97 C1 
        LDAA    #$02                     ;C1AF: 86 02 
        STAA    FunctionCode             ;C1B1: 97 A0 
        LDX     #FunctionCode            ;C1B3: CE 00 A0 
        JSR     ZC0EB                    ;C1B6: BD C0 EB 
        JMP     Send_EOT                 ;C1B9: 7E C1 1E 
GRPH_Y  CLRB                             ;C1BC: 5F 
        STAB    M0085                    ;C1BD: D7 85 
        LDD     FunctionCode             ;C1BF: DC A0 
        JSR     ZC301                    ;C1C1: BD C3 01 
        ANDB    M00CE                    ;C1C4: D4 CE 
        BEQ     ZC1CA                    ;C1C6: 27 02 
        LDAB    #$01                     ;C1C8: C6 01 
ZC1CA   STAB    M00A1                    ;C1CA: D7 A1 
        JSR     ZC0D6                    ;C1CC: BD C0 D6 
        LDAA    #$03                     ;C1CF: 86 03 
        STAA    M00A2                    ;C1D1: 97 A2 
        LDAA    #$01                     ;C1D3: 86 01 
        BRA     ZC1AB                    ;C1D5: 20 D4 
GRPH_I  LDAA    #$03                     ;C1D7: 86 03 
        BSR     ZC191                    ;C1D9: 8D B6 
        LDAA    CursorRow                ;C1DB: 96 9B 
        BEQ     ZC1EB                    ;C1DD: 27 0C 
ZC1DF   JSR     ZC515                    ;C1DF: BD C5 15 
        LDX     TextPosition             ;C1E2: DE CC 
        LDAA    ,X                       ;C1E4: A6 00 
        BNE     ZC1DF                    ;C1E6: 26 F7 
        JSR     ZC26D                    ;C1E8: BD C2 6D 
ZC1EB   LDAA    CursorRow                ;C1EB: 96 9B 
        STAA    M00C8                    ;C1ED: 97 C8 
ZC1EF   LDX     TextPosition             ;C1EF: DE CC 
        LDAA    ,X                       ;C1F1: A6 00 
        BEQ     ZC1FD                    ;C1F3: 27 08 
        JSR     ZC26D                    ;C1F5: BD C2 6D 
        JSR     ZC288                    ;C1F8: BD C2 88 
        BRA     ZC1EF                    ;C1FB: 20 F2 
ZC1FD   LDX     #FunctionCode            ;C1FD: CE 00 A0 
        LDAA    M00C8                    ;C200: 96 C8 
        STAA    $02,X                    ;C202: A7 02 
        LDAB    CursorRow                ;C204: D6 9B 
        STAB    $04,X                    ;C206: E7 04 
        LDAA    CursorColumn             ;C208: 96 9A 
        STAA    $03,X                    ;C20A: A7 03 
        LDAA    M00D1                    ;C20C: 96 D1 
        BEQ     ZC215                    ;C20E: 27 05 
        DECA                             ;C210: 4A 
        INC     $04,X                    ;C211: 6C 04 
        CLR     $03,X                    ;C213: 6F 03 
ZC215   STAA    $01,X                    ;C215: A7 01 
        LDAA    $04,X                    ;C217: A6 04 
        STAA    M00C8                    ;C219: 97 C8 
        CLRA                             ;C21B: 4F 
        STAA    M00D0                    ;C21C: 97 D0 
        LDAA    #$03                     ;C21E: 86 03 
        STAA    $05,X                    ;C220: A7 05 
        LDAA    #$04                     ;C222: 86 04 
        BRA     ZC1AB                    ;C224: 20 85 
TextON_OFF LDAA    P1DR                     ;C226: 96 02 
        EORA    #$02                     ;C228: 88 02          Toggle TXTON_OFF (P1.1)
ZC22A   STAA    P1DR                     ;C22A: 97 02 
        BRA     ZC268                    ;C22C: 20 3A 
InvertText LDAA    P1DR                     ;C22E: 96 02 
        EORA    #$01                     ;C230: 88 01          Toggle INVERT (P1.0)
        BRA     ZC22A                    ;C232: 20 F6 
ZC234   LDX     #SwitchCharset           ;C234: CE C7 03 
        STX     M00CA                    ;C237: DF CA 
        LDX     #FuncJumpTab             ;C239: CE C6 A3 
        BSR     ZC240                    ;C23C: 8D 02 
        BRA     ZC256                    ;C23E: 20 16 
ZC240   CMPA    ,X                       ;C240: A1 00 
        BEQ     ZC24C                    ;C242: 27 08 
        INX                              ;C244: 08 
        INX                              ;C245: 08 
        INX                              ;C246: 08 
        CPX     M00CA                    ;C247: 9C CA 
        BNE     ZC240                    ;C249: 26 F5 
        RTS                              ;C24B: 39 
ZC24C   INS                              ;C24C: 31 
        INS                              ;C24D: 31 
        LDX     $01,X                    ;C24E: EE 01 
        JMP     ,X                       ;C250: 6E 00 
GRPH_OM LDAA    FunctionCode             ;C252: 96 A0 
        BRA     ZC234                    ;C254: 20 DE 
ZC256   CMPA    #$20                     ;C256: 81 20 
        BMI     ZC268                    ;C258: 2B 0E 
        LDX     TextPosition             ;C25A: DE CC 
        ANDA    #$7F                     ;C25C: 84 7F 
        ORAA    M00C5                    ;C25E: 9A C5 
        STAA    ,X                       ;C260: A7 00 
        LDAA    M00C4                    ;C262: 96 C4 
        STAA    M00C5                    ;C264: 97 C5 
        BSR     ZC26D                    ;C266: 8D 05 
ZC268   BSR     ZC288                    ;C268: 8D 1E 
        JMP     ZC19F                    ;C26A: 7E C1 9F 
ZC26D   LDAA    CursorColumn             ;C26D: 96 9A 
        INCA                             ;C26F: 4C 
        STAA    CursorColumn             ;C270: 97 9A 
        CMPA    Char_per_Line            ;C272: 91 D2 
        BNE     ZC287                    ;C274: 26 11 
        CLR     >CursorColumn            ;C276: 7F 00 9A 
        INC     >CursorRow               ;C279: 7C 00 9B 
        LDAB    #$18                     ;C27C: C6 18 
        CMPB    CursorRow                ;C27E: D1 9B 
        BGE     ZC285                    ;C280: 2C 03 
        JSR     ZC570                    ;C282: BD C5 70 
ZC285   BSR     ZC288                    ;C285: 8D 01 
ZC287   RTS                              ;C287: 39 
ZC288   BSR     ZC2B2                    ;C288: 8D 28 
        STD     TextPosition             ;C28A: DD CC 
        LDAB    #$0C                     ;C28C: C6 0C 
        STAB    M6845_0                  ;C28E: F7 40 00       Set Register c (Start Address H)
        LDAA    StartAddressH            ;C291: 96 98 
        STAA    M6845_1                  ;C293: B7 40 01 
        INCB                             ;C296: 5C 
        STAB    M6845_0                  ;C297: F7 40 00       Set Register d (Start Address L)
        LDAA    StartAddressL            ;C29A: 96 99 
        STAA    M6845_1                  ;C29C: B7 40 01 
        INCB                             ;C29F: 5C 
        STAB    M6845_0                  ;C2A0: F7 40 00       Set Register e (Cursor H)
        LDAA    CursorH                  ;C2A3: 96 9C 
        STAA    M6845_1                  ;C2A5: B7 40 01 
        INCB                             ;C2A8: 5C 
        STAB    M6845_0                  ;C2A9: F7 40 00       Set Register f (Cursor L)
        LDAA    CursorL                  ;C2AC: 96 9D 
        STAA    M6845_1                  ;C2AE: B7 40 01 
        RTS                              ;C2B1: 39 
ZC2B2   LDAB    #$18                     ;C2B2: C6 18 
        CMPB    CursorRow                ;C2B4: D1 9B 
        BGE     ZC2BA                    ;C2B6: 2C 02 
        STAB    CursorRow                ;C2B8: D7 9B 
ZC2BA   LDAB    CursorRow                ;C2BA: D6 9B 
        LDAA    Char_per_Line            ;C2BC: 96 D2 
        MUL                              ;C2BE: 3D 
        ADDB    CursorColumn             ;C2BF: DB 9A 
        ADCA    #$00                     ;C2C1: 89 00 
        ADDD    StartAddressH            ;C2C3: D3 98 
        STD     CursorH                  ;C2C5: DD 9C 
        ADDD    #TEXTRAM                 ;C2C7: C3 10 00 
        RTS                              ;C2CA: 39 
GRPH_P  LDAB    M00A1                    ;C2CB: D6 A1 
        STAB    M00D3                    ;C2CD: D7 D3 
        LDAA    P1DR                     ;C2CF: 96 02 
        CMPB    #$01                     ;C2D1: C1 01          Check INVERT (P1.0)
        BNE     ZC2E9                    ;C2D3: 26 14 
        ORAA    #$40                     ;C2D5: 8A 40          Set T/G (P1.6)
        STAA    P1DR                     ;C2D7: 97 02 
        LDX     #MC6845_GRAPH            ;C2D9: CE C6 8F 
        LDAA    #$50                     ;C2DC: 86 50 
        JSR     ZC4DE                    ;C2DE: BD C4 DE 
        LDX     #$0000                   ;C2E1: CE 00 00 
        STX     StartAddressH            ;C2E4: DF 98 
        JMP     ZC268                    ;C2E6: 7E C2 68 
ZC2E9   ORAA    #$02                     ;C2E9: 8A 02 
        ANDA    #$BF                     ;C2EB: 84 BF          Clear T/G (P1.6)
        STAA    P1DR                     ;C2ED: 97 02 
        JSR     ZC4D3                    ;C2EF: BD C4 D3 
        JMP     Home                     ;C2F2: 7E C5 58 
GRPH_c2 LDX     FunctionCode             ;C2F5: DE A0 
        STX     CursorColumn             ;C2F7: DF 9A 
        JSR     ZC288                    ;C2F9: BD C2 88 
        BRA     GRPH_c5c6                ;C2FC: 20 00 
GRPH_c5c6 JMP     CheckRS232Data           ;C2FE: 7E C0 3C 
ZC301   STD     M00D8                    ;C301: DD D8 
        LDAA    #$06                     ;C303: 86 06 
        STAA    M00DA                    ;C305: 97 DA 
        BSR     ZC36B                    ;C307: 8D 62 
        LDAB    M00D9                    ;C309: D6 D9 
        INCB                             ;C30B: 5C 
        STAB    M00A1                    ;C30C: D7 A1 
        CLR     >FunctionCode            ;C30E: 7F 00 A0 
        STAA    M00CE                    ;C311: 97 CE 
        CLRA                             ;C313: 4F 
        STAA    M00D8                    ;C314: 97 D8 
        LDAA    #$08                     ;C316: 86 08 
        STAA    M00DA                    ;C318: 97 DA 
        LDAB    M00A3                    ;C31A: D6 A3 
        STAB    M00D9                    ;C31C: D7 D9 
        BSR     ZC36B                    ;C31E: 8D 4B 
        LDAB    M00D9                    ;C320: D6 D9 
        STAB    M00AF                    ;C322: D7 AF 
        LDAB    #$80                     ;C324: C6 80 
        MUL                              ;C326: 3D 
        STD     M00AC                    ;C327: DD AC 
        LDAA    #$50                     ;C329: 86 50 
        LDAB    M00AF                    ;C32B: D6 AF 
        MUL                              ;C32D: 3D 
        ADDD    FunctionCode             ;C32E: D3 A0 
        STAB    M00AE                    ;C330: D7 AE 
        ANDB    #$80                     ;C332: C4 80 
        ASLD                             ;C334: 05 
        ASLD                             ;C335: 05 
        ASLD                             ;C336: 05 
        LDAB    M00AE                    ;C337: D6 AE 
        ANDB    #$7F                     ;C339: C4 7F 
        ADDD    M00AC                    ;C33B: D3 AC 
        ADDD    #GRAPHICRAM              ;C33D: C3 80 00 
        PSHB                             ;C340: 37 
        PSHA                             ;C341: 36 
        PULX                             ;C342: 38 
        LDAA    #$80                     ;C343: 86 80 
        LDAB    M00CE                    ;C345: D6 CE 
        BEQ     ZC34D                    ;C347: 27 04 
ZC349   LSRA                             ;C349: 44 
        DECB                             ;C34A: 5A 
        BNE     ZC349                    ;C34B: 26 FC 
ZC34D   LDAB    ,X                       ;C34D: E6 00 
        STAA    M00CE                    ;C34F: 97 CE 
        RTS                              ;C351: 39 
GRPH_c7 BSR     ZC356                    ;C352: 8D 02 
ZC354   BRA     GRPH_c5c6                ;C354: 20 A8 
ZC356   LDD     FunctionCode             ;C356: DC A0 
        BSR     ZC301                    ;C358: 8D A7 
        TST     >M00A4                   ;C35A: 7D 00 A4 
        BEQ     ZC364                    ;C35D: 27 05 
        ORAB    M00CE                    ;C35F: DA CE 
ZC361   STAB    ,X                       ;C361: E7 00 
        RTS                              ;C363: 39 
ZC364   COMA                             ;C364: 43 
        STAA    M00CE                    ;C365: 97 CE 
        ANDB    M00CE                    ;C367: D4 CE 
        BRA     ZC361                    ;C369: 20 F6 
ZC36B   LDX     #CNTH                    ;C36B: CE 00 09 
        LDD     M00D8                    ;C36E: DC D8 
ZC370   SUBA    M00DA                    ;C370: 90 DA 
        BCC     ZC376                    ;C372: 24 02 
        ADDA    M00DA                    ;C374: 9B DA 
ZC376   ROLB                             ;C376: 59 
        ROLA                             ;C377: 49 
        DEX                              ;C378: 09 
        BNE     ZC370                    ;C379: 26 F5 
        STAB    M00D9                    ;C37B: D7 D9 
        RORA                             ;C37D: 46 
        ROLB                             ;C37E: 59 
        COMB                             ;C37F: 53 
        ANDB    #$01                     ;C380: C4 01 
        STAB    M00D8                    ;C382: D7 D8 
        COM     >M00D9                   ;C384: 73 00 D9 
        RTS                              ;C387: 39 
GRPH_c8 BSR     ZC393                    ;C388: 8D 09 
        BRA     ZC354                    ;C38A: 20 C8 
ZC38C   COMA                             ;C38C: 43 
        COMB                             ;C38D: 53 
        ADDB    #$01                     ;C38E: CB 01 
        ADCA    #$00                     ;C390: 89 00 
        RTS                              ;C392: 39 
ZC393   LDX     #M01DF                   ;C393: CE 01 DF 
        CPX     FunctionCode             ;C396: 9C A0 
        BPL     ZC39C                    ;C398: 2A 02 
        STX     FunctionCode             ;C39A: DF A0 
ZC39C   CPX     M00A4                    ;C39C: 9C A4 
        BPL     ZC3A2                    ;C39E: 2A 02 
        STX     M00A4                    ;C3A0: DF A4 
ZC3A2   LDX     #M00C7                   ;C3A2: CE 00 C7 
        CPX     M00A2                    ;C3A5: 9C A2 
        BPL     ZC3AB                    ;C3A7: 2A 02 
        STX     M00A2                    ;C3A9: DF A2 
ZC3AB   CPX     M00A6                    ;C3AB: 9C A6 
        BPL     ZC3B1                    ;C3AD: 2A 02 
        STX     M00A6                    ;C3AF: DF A6 
ZC3B1   LDX     #$0000                   ;C3B1: CE 00 00 
        STX     M00BE                    ;C3B4: DF BE 
        STX     M00B8                    ;C3B6: DF B8 
        STX     M00BC                    ;C3B8: DF BC 
        STX     M00BA                    ;C3BA: DF BA 
        LDX     FunctionCode             ;C3BC: DE A0 
        STX     M00B0                    ;C3BE: DF B0 
        LDX     M00A2                    ;C3C0: DE A2 
        STX     M00B2                    ;C3C2: DF B2 
        LDD     M00A4                    ;C3C4: DC A4 
        SUBD    FunctionCode             ;C3C6: 93 A0 
        BPL     ZC3CF                    ;C3C8: 2A 05 
        BSR     ZC38C                    ;C3CA: 8D C0 
        INC     >M00BE                   ;C3CC: 7C 00 BE 
ZC3CF   STD     M00B4                    ;C3CF: DD B4 
        LDD     M00A6                    ;C3D1: DC A6 
        SUBD    M00A2                    ;C3D3: 93 A2 
        BPL     ZC3DC                    ;C3D5: 2A 05 
        BSR     ZC38C                    ;C3D7: 8D B3 
        INC     >M00BF                   ;C3D9: 7C 00 BF 
ZC3DC   STD     M00B6                    ;C3DC: DD B6 
        LDAA    M00A8                    ;C3DE: 96 A8 
        STAA    M00A4                    ;C3E0: 97 A4 
        LDX     M00B4                    ;C3E2: DE B4 
        STX     M00D8                    ;C3E4: DF D8 
        CPX     M00B6                    ;C3E6: 9C B6 
        BMI     ZC3F1                    ;C3E8: 2B 07 
        INC     >M00BC                   ;C3EA: 7C 00 BC 
        LDX     M00B6                    ;C3ED: DE B6 
        BRA     ZC3F7                    ;C3EF: 20 06 
ZC3F1   LDX     M00B6                    ;C3F1: DE B6 
        STX     M00D8                    ;C3F3: DF D8 
        LDX     M00B4                    ;C3F5: DE B4 
ZC3F7   STX     M00DA                    ;C3F7: DF DA 
        JSR     ZC466                    ;C3F9: BD C4 66 
        LDX     M00D8                    ;C3FC: DE D8 
        STX     M00C2                    ;C3FE: DF C2 
ZC400   JSR     ZC490                    ;C400: BD C4 90 
        BSR     ZC413                    ;C403: 8D 0E 
        LDAA    M00BC                    ;C405: 96 BC 
        BEQ     ZC41E                    ;C407: 27 15 
        BSR     ZC43E                    ;C409: 8D 33 
        LDX     M00B4                    ;C40B: DE B4 
        STX     M00D8                    ;C40D: DF D8 
        LDX     M00B6                    ;C40F: DE B6 
        BRA     ZC426                    ;C411: 20 13 
ZC413   LDX     M00B4                    ;C413: DE B4 
        BNE     ZC41D                    ;C415: 26 06 
        LDX     M00B6                    ;C417: DE B6 
        BNE     ZC41D                    ;C419: 26 02 
        INS                              ;C41B: 31 
        INS                              ;C41C: 31 
ZC41D   RTS                              ;C41D: 39 
ZC41E   BSR     ZC452                    ;C41E: 8D 32 
        LDX     M00B6                    ;C420: DE B6 
        STX     M00D8                    ;C422: DF D8 
        LDX     M00B4                    ;C424: DE B4 
ZC426   STX     M00DA                    ;C426: DF DA 
        BSR     ZC466                    ;C428: 8D 3C 
        LDX     M00D8                    ;C42A: DE D8 
        CPX     M00C2                    ;C42C: 9C C2 
        BMI     ZC436                    ;C42E: 2B 06 
        BRA     ZC400                    ;C430: 20 CE 
ZC432   BSR     ZC43E                    ;C432: 8D 0A 
        BRA     ZC400                    ;C434: 20 CA 
ZC436   LDAA    M00BC                    ;C436: 96 BC 
        BEQ     ZC432                    ;C438: 27 F8 
        BSR     ZC452                    ;C43A: 8D 16 
        BRA     ZC400                    ;C43C: 20 C2 
ZC43E   LDX     M00B4                    ;C43E: DE B4 
        BEQ     ZC451                    ;C440: 27 0F 
        DEX                              ;C442: 09 
        STX     M00B4                    ;C443: DF B4 
        LDX     M00B0                    ;C445: DE B0 
        LDAA    M00BE                    ;C447: 96 BE 
        BNE     ZC44E                    ;C449: 26 03 
        INX                              ;C44B: 08 
        BRA     ZC44F                    ;C44C: 20 01 
ZC44E   DEX                              ;C44E: 09 
ZC44F   STX     M00B0                    ;C44F: DF B0 
ZC451   RTS                              ;C451: 39 
ZC452   LDX     M00B6                    ;C452: DE B6 
        BEQ     ZC451                    ;C454: 27 FB 
        DEX                              ;C456: 09 
        STX     M00B6                    ;C457: DF B6 
        LDX     M00B2                    ;C459: DE B2 
        LDAA    M00BF                    ;C45B: 96 BF 
        BNE     ZC462                    ;C45D: 26 03 
        INX                              ;C45F: 08 
        BRA     ZC463                    ;C460: 20 01 
ZC462   DEX                              ;C462: 09 
ZC463   STX     M00B2                    ;C463: DF B2 
        RTS                              ;C465: 39 
ZC466   LDD     M00D8                    ;C466: DC D8 
        ASLD                             ;C468: 05 
        ASLD                             ;C469: 05 
        ASLD                             ;C46A: 05 
        ASLD                             ;C46B: 05 
        ASLD                             ;C46C: 05 
        ASLD                             ;C46D: 05 
        STD     M00D8                    ;C46E: DD D8 
        LDAA    #$11                     ;C470: 86 11 
        STAA    M00C1                    ;C472: 97 C1 
        CLRA                             ;C474: 4F 
        CLRB                             ;C475: 5F 
ZC476   SUBD    M00DA                    ;C476: 93 DA 
        BCC     ZC47C                    ;C478: 24 02 
        ADDD    M00DA                    ;C47A: D3 DA 
ZC47C   ROL     >M00D9                   ;C47C: 79 00 D9 
        ROL     >M00D8                   ;C47F: 79 00 D8 
        ROLB                             ;C482: 59 
        ROLA                             ;C483: 49 
        DEC     >M00C1                   ;C484: 7A 00 C1 
        BNE     ZC476                    ;C487: 26 ED 
        COM     >M00D9                   ;C489: 73 00 D9 
        COM     >M00D8                   ;C48C: 73 00 D8 
        RTS                              ;C48F: 39 
ZC490   LDX     M00B0                    ;C490: DE B0 
        STX     FunctionCode             ;C492: DF A0 
        LDX     M00B2                    ;C494: DE B2 
        STX     M00A2                    ;C496: DF A2 
        JMP     ZC356                    ;C498: 7E C3 56 
GRPH_c9 BRA     ZC4B1                    ;C49B: 20 14 
ZC49D   TST     >M00D3                   ;C49D: 7D 00 D3 
        BEQ     ZC4AE                    ;C4A0: 27 0C 
ClearGRAM LDX     #GRAPHICRAM              ;C4A2: CE 80 00 
        CLRA                             ;C4A5: 4F 
ZC4A6   STAA    ,X                       ;C4A6: A7 00 
        INX                              ;C4A8: 08 
        CPX     #hdlr_RST                ;C4A9: 8C C0 00 
        BNE     ZC4A6                    ;C4AC: 26 F8 
ZC4AE   RTS                              ;C4AE: 39 
GRPH_ca BSR     ZC49D                    ;C4AF: 8D EC 
ZC4B1   JMP     CheckRS232Data           ;C4B1: 7E C0 3C 
Graphic_CLR BSR     ZC49D                    ;C4B4: 8D E7 
Jump_C268_1 JMP     ZC268                    ;C4B6: 7E C2 68 
Set_80Char BSR     ZC4D3                    ;C4B9: 8D 18 
        BRA     Jump_C268_1              ;C4BB: 20 F9 
Set_72Char LDAA    #$48                     ;C4BD: 86 48 
        BSR     ZC4D5                    ;C4BF: 8D 14 
        CMPB    #$01                     ;C4C1: C1 01 
        BEQ     Jump_C268_1              ;C4C3: 27 F1 
        LDX     #$0148                   ;C4C5: CE 01 48 
        STX     M6845_0                  ;C4C8: FF 40 00       Set Register 1 (Hor. Displayed) to 0x48 - 72
        LDX     #$0250                   ;C4CB: CE 02 50 
        STX     M6845_0                  ;C4CE: FF 40 00       Set Register 2 (Hor. Sync Pos.) to 0x50 - 80
        BRA     Jump_C268_1              ;C4D1: 20 E3 
ZC4D3   LDAA    #$50                     ;C4D3: 86 50 
ZC4D5   LDAB    #$01                     ;C4D5: C6 01 
        CMPB    M00D3                    ;C4D7: D1 D3 
        BEQ     ZC514                    ;C4D9: 27 39 
        LDX     #MC6845_7280Z            ;C4DB: CE C6 5E 
ZC4DE   STAA    Char_per_Line            ;C4DE: 97 D2 
        LDAA    #$80                     ;C4E0: 86 80 
Write_6845_Registers PSHA                             ;C4E2: 36 
        CLRB                             ;C4E3: 5F 
ZC4E4   STAB    M6845_0                  ;C4E4: F7 40 00 
        LDAA    ,X                       ;C4E7: A6 00 
        STAA    M6845_1                  ;C4E9: B7 40 01 
        INX                              ;C4EC: 08 
        INCB                             ;C4ED: 5C 
        CMPB    #$10                     ;C4EE: C1 10 
        BNE     ZC4E4                    ;C4F0: 26 F2 
        LDAA    P1DR                     ;C4F2: 96 02 
        ANDA    #$7F                     ;C4F4: 84 7F          Clear HIGHRES (P1.7) ?
        PULB                             ;C4F6: 33 
        ABA                              ;C4F7: 1B 
        STAA    P1DR                     ;C4F8: 97 02 
        JMP     ResetTextScreen          ;C4FA: 7E C1 2E 
DelLine LDAB    Char_per_Line            ;C4FD: D6 D2 
        SUBB    CursorColumn             ;C4FF: D0 9A 
        BSR     ZC50B                    ;C501: 8D 08 
        BRA     Jump_C268_2              ;C503: 20 64 
ZC505   LDAB    Char_per_Line            ;C505: D6 D2 
        ADDB    Char_per_Line            ;C507: DB D2 
        SUBB    #$20                     ;C509: C0 20 
ZC50B   CLRA                             ;C50B: 4F 
        LDX     TextPosition             ;C50C: DE CC 
ZC50E   STAA    ,X                       ;C50E: A7 00 
        INX                              ;C510: 08 
        DECB                             ;C511: 5A 
        BNE     ZC50E                    ;C512: 26 FA 
ZC514   RTS                              ;C514: 39 
ZC515   DEC     >CursorColumn            ;C515: 7A 00 9A 
        BPL     ZC524                    ;C518: 2A 0A 
        LDAA    Char_per_Line            ;C51A: 96 D2 
        DECA                             ;C51C: 4A 
        STAA    CursorColumn             ;C51D: 97 9A 
        DEC     >CursorRow               ;C51F: 7A 00 9B 
        BMI     ZC556                    ;C522: 2B 32 
ZC524   JMP     ZC288                    ;C524: 7E C2 88 
DeleteChar LDX     TextPosition             ;C527: DE CC 
        DEX                              ;C529: 09 
        LDAB    ,X                       ;C52A: E6 00 
        BEQ     Jump_C268_2              ;C52C: 27 3B 
        BSR     ZC515                    ;C52E: 8D E5 
        LDX     TextPosition             ;C530: DE CC 
ZC532   LDAB    $01,X                    ;C532: E6 01 
        STAB    ,X                       ;C534: E7 00 
        INX                              ;C536: 08 
        CMPB    #$00                     ;C537: C1 00 
        BNE     ZC532                    ;C539: 26 F7 
        BRA     Jump_C268_2              ;C53B: 20 2C 
Hor.Tab LDAB    CursorColumn             ;C53D: D6 9A 
ZC53F   LDAA    #$08                     ;C53F: 86 08 
        SUBB    #$08                     ;C541: C0 08 
        BPL     ZC53F                    ;C543: 2A FA 
        ADDB    #$08                     ;C545: CB 08 
        SBA                              ;C547: 10 
        ADDA    CursorColumn             ;C548: 9B 9A 
        CMPA    Char_per_Line            ;C54A: 91 D2 
        BNE     ZC552                    ;C54C: 26 04 
        CLRA                             ;C54E: 4F 
        INC     >CursorRow               ;C54F: 7C 00 9B 
ZC552   STAA    CursorColumn             ;C552: 97 9A 
        BRA     Jump_C268_2              ;C554: 20 13 
ZC556   INS                              ;C556: 31 
        INS                              ;C557: 31 
Home    LDX     #$0000                   ;C558: CE 00 00 
        STX     M00C8                    ;C55B: DF C8 
        STX     CursorColumn             ;C55D: DF 9A 
        BRA     Jump_C268_2              ;C55F: 20 08 
ClearScreen JSR     ResetTextScreen          ;C561: BD C1 2E 
        BRA     Jump_C268_2              ;C564: 20 03 
Enter   CLR     >CursorColumn            ;C566: 7F 00 9A 
Jump_C268_2 JMP     ZC268                    ;C569: 7E C2 68 
ZC56C   BSR     ZC570                    ;C56C: 8D 02 
Jump_C268_6 BRA     Jump_C268_2              ;C56E: 20 F9 
ZC570   LDX     StartAddressH            ;C570: DE 98 
        LDAB    Char_per_Line            ;C572: D6 D2 
        ABX                              ;C574: 3A 
        STX     StartAddressH            ;C575: DF 98 
        LDX     TextPosition             ;C577: DE CC 
        ABX                              ;C579: 3A 
        INC     >CursorRow               ;C57A: 7C 00 9B 
        LDAA    #$18                     ;C57D: 86 18 
        CMPA    CursorRow                ;C57F: 91 9B 
        BGE     ZC514                    ;C581: 2C 91 
        STAA    CursorRow                ;C583: 97 9B 
        CLR     >CursorColumn            ;C585: 7F 00 9A 
        STX     TextPosition             ;C588: DF CC 
        LDAA    P1DR                     ;C58A: 96 02 
        ANDA    #$BF                     ;C58C: 84 BF          Clear T/G (P1.6)
        STAA    P1DR                     ;C58E: 97 02 
        LDAA    StartAddressH            ;C590: 96 98 
        ANDA    #$07                     ;C592: 84 07  Changed to 0F
        STAA    StartAddressH            ;C594: 97 98 
        LDAA    TextPosition             ;C596: 96 CC 
        ANDA    #$17                     ;C598: 84 17 
        STAA    TextPosition             ;C59A: 97 CC 
        JSR     ZC288                    ;C59C: BD C2 88 
        JMP     ZC505                    ;C59F: 7E C5 05 
ZC5A2   LDD     StartAddressH            ;C5A2: DC 98 
        BEQ     Jump_C268_6              ;C5A4: 27 C8 
        LDAA    CursorRow                ;C5A6: 96 9B 
        DECA                             ;C5A8: 4A 
        STAA    CursorRow                ;C5A9: 97 9B 
        SUBB    Char_per_Line            ;C5AB: D0 D2 
        SBCA    #$00                     ;C5AD: 82 00 
        STD     StartAddressH            ;C5AF: DD 98 
        BRA     Jump_C268_6              ;C5B1: 20 BB 
ToggleInsertMd LDX     TextPosition             ;C5B3: DE CC 
ZC5B5   INX                              ;C5B5: 08 
        LDAA    ,X                       ;C5B6: A6 00 
        BNE     ZC5B5                    ;C5B8: 26 FB 
        STAA    $01,X                    ;C5BA: A7 01 
ZC5BC   DEX                              ;C5BC: 09 
        LDAA    ,X                       ;C5BD: A6 00 
        STAA    $01,X                    ;C5BF: A7 01 
        CPX     TextPosition             ;C5C1: 9C CC 
        BGE     ZC5BC                    ;C5C3: 2C F7 
        LDAA    #$20                     ;C5C5: 86 20 
        STAA    $01,X                    ;C5C7: A7 01 
        JMP     ZC19F                    ;C5C9: 7E C1 9F 
Set_CRSR_Blink LDAA    CursorStart              ;C5CC: 96 C6 
        EORA    #$60                     ;C5CE: 88 60 
        STAA    CursorStart              ;C5D0: 97 C6 
Set_CRSR_On LDAA    CursorStart              ;C5D2: 96 C6 
        BRA     ZC5D8                    ;C5D4: 20 02 
Set_CRSR_Off LDAA    #$20                     ;C5D6: 86 20 
ZC5D8   BSR     Write_CRSR_Reg           ;C5D8: 8D 58 
Jump_C268_3 JMP     ZC268                    ;C5DA: 7E C2 68 
DelScreen BSR     ZC5E1                    ;C5DD: 8D 02 
Jump_C268_7 BRA     Jump_C268_3              ;C5DF: 20 F9 
ZC5E1   LDAA    TextPosition             ;C5E1: 96 CC 
        ANDA    #$17                     ;C5E3: 84 17 
        STAA    TextPosition             ;C5E5: 97 CC 
        LDX     TextPosition             ;C5E7: DE CC 
        CLRA                             ;C5E9: 4F 
ZC5EA   STAA    ,X                       ;C5EA: A7 00 
        INX                              ;C5EC: 08 
        CPX     #M1800                   ;C5ED: 8C 18 00  Changed to 2000
        BNE     ZC5EA                    ;C5F0: 26 F8 
        RTS                              ;C5F2: 39 
MovCRSR_Rgt INC     >CursorColumn            ;C5F3: 7C 00 9A 
        LDAB    Char_per_Line            ;C5F6: D6 D2 
        CMPB    CursorColumn             ;C5F8: D1 9A 
        BNE     Jump_C268_7              ;C5FA: 26 E3 
        CLR     >CursorColumn            ;C5FC: 7F 00 9A 
        INC     >CursorRow               ;C5FF: 7C 00 9B 
        BRA     Jump_C268_7              ;C602: 20 DB 
MovCRSR_Lft JSR     ZC515                    ;C604: BD C5 15 
        BRA     Jump_C268_7              ;C607: 20 D6 
MovCRSR_Up LDAA    CursorRow                ;C609: 96 9B 
        BEQ     Jump_C268_7              ;C60B: 27 D2 
        CMPA    #$19                     ;C60D: 81 19 
        BMI     ZC614                    ;C60F: 2B 03 
        JMP     ZC5A2                    ;C611: 7E C5 A2 
ZC614   DECA                             ;C614: 4A 
ZC615   STAA    CursorRow                ;C615: 97 9B 
        BRA     Jump_C268_7              ;C617: 20 C6 
Linefeed LDAA    M00D1                    ;C619: 96 D1 
        BEQ     MovCRSR_Dwn              ;C61B: 27 09 
        CLR     >M00D1                   ;C61D: 7F 00 D1 
        LDAA    CursorRow                ;C620: 96 9B 
        CMPA    #$18                     ;C622: 81 18 
        BMI     Jump_C268_7              ;C624: 2B B9 
MovCRSR_Dwn LDAA    CursorRow                ;C626: 96 9B 
        CMPA    #$18                     ;C628: 81 18 
        BMI     ZC62F                    ;C62A: 2B 03 
        JMP     ZC56C                    ;C62C: 7E C5 6C 
ZC62F   INCA                             ;C62F: 4C 
        BRA     ZC615                    ;C630: 20 E3 
Write_CRSR_Reg LDAB    #$0A                     ;C632: C6 0A 
        STAB    M6845_0                  ;C634: F7 40 00 
        STAA    M6845_1                  ;C637: B7 40 01 
_RTS_   RTS                              ;C63A: 39 
Set_40Char BSR     PreloadA_40              ;C63B: 8D 31 
Jump_C268_8 BRA     Jump_C268_7              ;C63D: 20 A0 
Set_32Char LDAA    #$20                     ;C63F: 86 20 
        BSR     ZC670                    ;C641: 8D 2D 
        CMPB    #$01                     ;C643: C1 01 
        BEQ     Jump_C268_7              ;C645: 27 98 
        LDX     #$0120                   ;C647: CE 01 20       Set Register 1 (Hor. Displayed) to 0x20 - 32
        STX     M6845_0                  ;C64A: FF 40 00 
        LDX     #$0228                   ;C64D: CE 02 28       Set Register 2 (Hor. Sync Pos.) to 0x28 - 40
        STX     M6845_0                  ;C650: FF 40 00 
        BRA     Jump_C268_8              ;C653: 20 E8 
SendPKT_00 FCB     $02,$00,$03,$FB          ;C655: 02 00 03 FB 
SendPKT_50_19 FCB     $02                      ;C659: 02 
        FCB     $50                      ;C65A: 50 
        FCB     $19,$03,$92              ;C65B: 19 03 92 
MC6845_7280Z FCB     $62                      ;C65E: 62             Set Register 0 (H Total) - 98
        FCB     $50                      ;C65F: 50             Set Register 1 (H Displayed) - 80
        FCB     $54                      ;C660: 54             Set Register 2 (H Sync. Pos.) - 84
        FCB     $29                      ;C661: 29             Set Register 3 (Sync. Width) - 41
        FCB     $1E                      ;C662: 1E             Set Register 4 (V Total) - 30
        FCB     $00                      ;C663: 00             Set Register 5 (V Total Adj.) 
        FCB     $19                      ;C664: 19             Set Register 6 (V Displayed) - 25
        FCB     $1B                      ;C665: 1B             Set Register 7 (V Sync. Pos.) - 27 
        FCB     $A2                      ;C666: A2             Set Register 8 (Interlace Md. and Skew) - 162
        FCB     $09                      ;C667: 09             Set Register 9 (Max. Scanline Address) 
        FCB     $00                      ;C668: 00             Set Register 10 (Crsr Start) 
        FCB     $10                      ;C669: 10             Set Register 11 (Crsr End) - 16
        FCB     $00                      ;C66A: 00             Set Register 12 (Start Address H) 
        FCB     $00                      ;C66B: 00             Set Register 13 (Start Address L) 
        FCB     $00                      ;C66C: 00             Set Register 14 (Crsr H) 
        FCB     $00                      ;C66D: 00             Set Register 15 (Crsr L) 
PreloadA_40 LDAA    #$28                     ;C66E: 86 28 
ZC670   LDAB    M00D3                    ;C670: D6 D3 
        CMPB    #$01                     ;C672: C1 01 
        BEQ     _RTS_                    ;C674: 27 C4 
        LDX     #MC6845_3240Z            ;C676: CE C6 7F 
        STAA    Char_per_Line            ;C679: 97 D2 
        CLRA                             ;C67B: 4F 
        JMP     Write_6845_Registers     ;C67C: 7E C4 E2 
MC6845_3240Z FCB     $30                      ;C67F: 30             Set Register 0 (H Total) - 48
        FCB     $28                      ;C680: 28             Set Register 1 (H Displayed) - 40
        FCB     $2B                      ;C681: 2B             Set Register 2 (H Sync. Pos.) - 43
        FCB     $43                      ;C682: 43             Set Register 3 (Sync. Width) - 67
        FCB     $1D                      ;C683: 1D             Set Register 4 (V Total) - 29
        FCB     $0C                      ;C684: 0C             Set Register 5 (V Total Adj.) - 12
        FCB     $19                      ;C685: 19             Set Register 6 (V Displayed) - 25
        FCB     $1B                      ;C686: 1B             Set Register 7 (V Sync. Pos.) - 27
        FCB     $A0                      ;C687: A0             Set Register 8 (Interlace Md. and Skew) - 160
        FCB     $09                      ;C688: 09             Set Register 9 (Max. Scanline Address)
        FCB     $00                      ;C689: 00             Set Register 10 (Crsr Start) 
        FCB     $10                      ;C68A: 10             Set Register 11 (Crsr End) - 16
        FCB     $00                      ;C68B: 00             Set Register 12 (Start Address H) 
        FCB     $00                      ;C68C: 00             Set Register 13 (Start Address L) 
        FCB     $00                      ;C68D: 00             Set Register 14 (Crsr H) 
        FCB     $00                      ;C68E: 00             Set Register 15 (Crsr L) 
MC6845_GRAPH FCB     $62                      ;C68F: 62             Set Register 0 (H Total) - 98
        FCB     $50                      ;C690: 50             Set Register 1 (H Displayed) - 80
        FCB     $53                      ;C691: 53             Set Register 2 (H Sync. Pos.) - 83
        FCB     $29                      ;C692: 29             Set Register 3 (Sync. Width) - 41
        FCB     $26                      ;C693: 26             Set Register 4 (V Total) - 38
        FCB     $00                      ;C694: 00             Set Register 5 (V Total Adj.) 
        FCB     $19                      ;C695: 19             Set Register 6 (V Displayed) - 25
        FCB     $1F                      ;C696: 1F             Set Register 7 (V Sync. Pos.) - 31
        FCB     $A0                      ;C697: A0             Set Register 8 (Interlace Md. and Skew) - 160
        FCB     $07                      ;C698: 07             Set Register 9 (Max. Scanline Address) 
        FCB     $00                      ;C699: 00             Set Register 10 (Crsr Start) 
        FCB     $07                      ;C69A: 07             Set Register 11 (Crsr End) 
        FCB     $00                      ;C69B: 00             Set Register 12 (Start Address H) 
        FCB     $00                      ;C69C: 00             Set Register 13 (Start Address L) 
        FCB     $00                      ;C69D: 00             Set Register 14 (Crsr H) 
        FCB     $00                      ;C69E: 00             Set Register 15 (Crsr L) 
MC69F   FCB     $01,$01                  ;C69F: 01 01 
MC6A1   FCB     $20,$30                  ;C6A1: 20 30 
FuncJumpTab FCB     $01                      ;C6A3: 01 
        FDB     Enter                    ;C6A4: C5 66 
        FCB     $0E                      ;C6A6: 0E             CTRL + N - ?
        FDB     CTRL_N                   ;C6A7: C7 1C 
        FCB     $04                      ;C6A9: 04             CTRL + D or -> - Move Cursor
        FDB     DelLine                  ;C6AA: C4 FD 
        FCB     $05                      ;C6AC: 05             CTRL + E - Delete to EOL
        FDB     DelLine                  ;C6AD: C4 FD 
        FCB     $06                      ;C6AF: 06             CTRL + F - Cursor to right edge
        FDB     CRSR_RgtEdge             ;C6B0: C7 14 
        FCB     $07                      ;C6B2: 07             CTRL + G - ? Make Sound (in MH20 Emulator)
        FDB     CTRL_G                   ;C6B3: C7 0C 
        FCB     $08                      ;C6B5: 08             CTRL + H - Delete one char.
        FDB     DeleteChar               ;C6B6: C5 27 
        FCB     $09                      ;C6B8: 09             CTRL + I - Hor. TAB
        FDB     Hor.Tab                  ;C6B9: C5 3D 
        FCB     $0A                      ;C6BB: 0A             CTRL + J - Linefeed
        FDB     Linefeed                 ;C6BC: C6 19 
        FCB     $0B                      ;C6BE: 0B             CTRL + K - Home
        FDB     Home                     ;C6BF: C5 58 
        FCB     $0C                      ;C6C1: 0C             CTRL + L - Clear Screen
        FDB     ClearScreen              ;C6C2: C5 61 
        FCB     $0D                      ;C6C4: 0D             CTRL + M - Enter
        FDB     Enter                    ;C6C5: C5 66 
        FCB     $10                      ;C6C7: 10             CTRL + P - Move Window up
        FDB     MovCRSR_Up               ;C6C8: C6 09 
        FCB     $11                      ;C6CA: 11             CTRL + Q - Move Window down
        FDB     MovCRSR_Dwn              ;C6CB: C6 26 
        FCB     $12                      ;C6CD: 12             CTRL + R - Toggle Insert Mode
        FDB     ToggleInsertMd           ;C6CE: C5 B3 
        FCB     $16                      ;C6D0: 16             CTRL + V - Make Cursor visible
        FDB     Set_CRSR_On              ;C6D1: C5 D2 
        FCB     $17                      ;C6D3: 17             CTRL + W - Make Cursor invisible
        FDB     Set_CRSR_Off             ;C6D4: C5 D6 
        FCB     $1A                      ;C6D6: 1A             CTRL + Z - Delete Cursor to end of screen
        FDB     DelScreen                ;C6D7: C5 DD 
        FCB     $1C                      ;C6D9: 1C             CTRL + \ - Move Cursor right
        FDB     MovCRSR_Rgt              ;C6DA: C5 F3 
        FCB     $1D                      ;C6DC: 1D             CTRL + ] - Move Cursor left
        FDB     MovCRSR_Lft              ;C6DD: C6 04 
        FCB     $1E                      ;C6DF: 1E             CTRL + ^ - Move Cursor up
        FDB     MovCRSR_Up               ;C6E0: C6 09 
        FCB     $1F                      ;C6E2: 1F             CTRL + _ - Move Cursor down
        FDB     MovCRSR_Dwn              ;C6E3: C6 26 
        FCB     $F0                      ;C6E5: F0             CTRL + 0 - TestPicture (Graphic Mode)
        FDB     TestPicture              ;C6E6: C7 7C 
        FCB     $F1                      ;C6E8: F1             CTRL + 1 - Text On/Off
        FDB     TextON_OFF               ;C6E9: C2 26 
        FCB     $F2                      ;C6EB: F2             CTRL + 2 - Inverted Text
        FDB     InvertText               ;C6EC: C2 2E 
        FCB     $F3                      ;C6EE: F3             CTRL + 3 - 32 Characters
        FDB     Set_32Char               ;C6EF: C6 3F 
        FCB     $F4                      ;C6F1: F4             CTRL + 4 - 40 Characters
        FDB     Set_40Char               ;C6F2: C6 3B 
        FCB     $F5                      ;C6F4: F5             CTRL + 5 - Cursor Blink
        FDB     Set_CRSR_Blink           ;C6F5: C5 CC 
        FCB     $F6                      ;C6F7: F6             CTRL + 6 - Graphics Clear Screen
        FDB     Graphic_CLR              ;C6F8: C4 B4 
        FCB     $F7                      ;C6FA: F7             CTRL + 7 - 72 Characters
        FDB     Set_72Char               ;C6FB: C4 BD 
        FCB     $F8                      ;C6FD: F8             CTRL + 8 - 80 Characters
        FDB     Set_80Char               ;C6FE: C4 B9 
        FCB     $F9                      ;C700: F9             CTRL + 9 - second Charset
        FDB     SwitchCharset            ;C701: C7 03 
SwitchCharset LDAA    P1DR                     ;C703: 96 02 
        EORA    #$20                     ;C705: 88 20          Toggle Charset (P1.5)
ZC707   STAA    P1DR                     ;C707: 97 02 
Jump_C268_4 JMP     ZC268                    ;C709: 7E C2 68 
CTRL_G  LDAA    P1DR                     ;C70C: 96 02 
        ORAA    #$08                     ;C70E: 8A 08          Set GTEXT (P1.4), Toggle TEXT (P1.3)
        EORA    #$04                     ;C710: 88 04 
        BRA     ZC707                    ;C712: 20 F3 
CRSR_RgtEdge LDAA    P1DR                     ;C714: 96 02 
        ORAA    #$04                     ;C716: 8A 04          Toggle GTEXT (P1.4), Set TEXT (P1.3)
        EORA    #$08                     ;C718: 88 08 
        BRA     ZC707                    ;C71A: 20 EB 
CTRL_N  LDAA    #$80                     ;C71C: 86 80 
        EORA    M00C4                    ;C71E: 98 C4 
        STAA    M00C4                    ;C720: 97 C4 
        BRA     Jump_C268_4              ;C722: 20 E5 
GrTXT   FCB     'B,'i,'t,'t,'e,',,' ,'"  ;C724: 42 69 74 74 65 2C 20 22 
        FCB     'S,'C,'R,'E,'E,'N,' ,'1  ;C72C: 53 43 52 45 45 4E 20 31 
        FCB     ',,'0,'",' ,'e,'i,'n,'g  ;C734: 2C 30 22 20 65 69 6E 67 
        FCB     'e,'b,'e,'n,'!,$04       ;C73C: 65 62 65 6E 21 04 
MC742   FCB     $88                      ;C742: 88             GRPH + E - 
        FDB     GRPH_EZ                  ;C743: C1 85 
        FCB     $89                      ;C745: 89             GRPH + Z - 
        FDB     GRPH_EZ                  ;C746: C1 85 
        FCB     $8C                      ;C748: 8C             GRPH + F - 
        FDB     GRPH_F                   ;C749: C1 96 
        FCB     $8F                      ;C74B: 8F             GRPH + Y - 
        FDB     GRPH_Y                   ;C74C: C1 BC 
        FCB     $91                      ;C74E: 91             GRPH + I - 
        FDB     GRPH_I                   ;C74F: C1 D7 
        FCB     $92                      ;C751: 92             GRPH + O - 
        FDB     GRPH_OM                  ;C752: C2 52 
        FCB     $93                      ;C754: 93             GRPH + P - 
        FDB     GRPH_P                   ;C755: C2 CB 
        FCB     $97                      ;C757: 97             GRPH + , - 
        FDB     GRPH_Comma               ;C758: C1 4A 
        FCB     $98                      ;C75A: 98             GRPH + M - 
        FDB     GRPH_OM                  ;C75B: C2 52 
        FCB     $C2                      ;C75D: C2             ? - 
        FDB     GRPH_c2                  ;C75E: C2 F5 
        FCB     $C5                      ;C760: C5 
        FDB     GRPH_c5c6                ;C761: C2 FE 
        FCB     $C6                      ;C763: C6 
        FDB     GRPH_c5c6                ;C764: C2 FE 
        FCB     $C7                      ;C766: C7 
        FDB     GRPH_c7                  ;C767: C3 52 
        FCB     $C8                      ;C769: C8 
        FDB     GRPH_c8                  ;C76A: C3 88 
        FCB     $C9                      ;C76C: C9 
        FDB     GRPH_c9                  ;C76D: C4 9B 
        FCB     $CA                      ;C76F: CA 
        FDB     GRPH_ca                  ;C770: C4 AF 
        FCB     $CB                      ;C772: CB 
        FDB     GRPH_cbcfd4              ;C773: C1 2B 
        FCB     $CF                      ;C775: CF 
        FDB     GRPH_cbcfd4              ;C776: C1 2B 
        FCB     $D4                      ;C778: D4 
        FDB     GRPH_cbcfd4              ;C779: C1 2B 
MC77B   FCB     $01                      ;C77B: 01 
TestPicture TST     >M00D3                   ;C77C: 7D 00 D3 
        BEQ     Jump_C268_5              ;C77F: 27 32 
        JSR     ZC49D                    ;C781: BD C4 9D 
        LDAA    #$0D                     ;C784: 86 0D 
        STAA    M00D3                    ;C786: 97 D3 
        LDX     #TestPicData             ;C788: CE C7 B6 
        STX     M00CA                    ;C78B: DF CA 
ZC78D   LDX     M00CA                    ;C78D: DE CA 
        LDD     ,X                       ;C78F: EC 00 
        STD     FunctionCode             ;C791: DD A0 
        LDD     $02,X                    ;C793: EC 02 
        STD     M00A2                    ;C795: DD A2 
        LDD     $04,X                    ;C797: EC 04 
        STD     M00A4                    ;C799: DD A4 
        LDD     $06,X                    ;C79B: EC 06 
        STD     M00A6                    ;C79D: DD A6 
        LDAA    #$01                     ;C79F: 86 01 
        STAA    M00A8                    ;C7A1: 97 A8 
        LDAB    #$04                     ;C7A3: C6 04 
        ABX                              ;C7A5: 3A 
        STX     M00CA                    ;C7A6: DF CA 
        JSR     ZC393                    ;C7A8: BD C3 93 
        DEC     >M00D3                   ;C7AB: 7A 00 D3 
        BNE     ZC78D                    ;C7AE: 26 DD 
        INC     >M00D3                   ;C7B0: 7C 00 D3 
Jump_C268_5 JMP     ZC268                    ;C7B3: 7E C2 68 
TestPicData FCB     $00,$B4,$00,$00,$00,$B4  ;C7B6: 00 B4 00 00 00 B4 
        FCB     $00,$BF,$01              ;C7BC: 00 BF 01 
        FCB     $2C                      ;C7BF: 2C 
        FCB     $00,$BF,$01              ;C7C0: 00 BF 01 
        FCB     $2C                      ;C7C3: 2C 
        FCB     $00,$00,$00              ;C7C4: 00 00 00 
        FCB     $73                      ;C7C7: 73 
        FCB     $00,$00,$00              ;C7C8: 00 00 00 
        FCB     $73                      ;C7CB: 73 
        FCB     $00                      ;C7CC: 00 
        FCB     $32                      ;C7CD: 32 
        FCB     $01                      ;C7CE: 01 
        FCB     $6D                      ;C7CF: 6D 
        FCB     $00                      ;C7D0: 00 
        FCB     $32                      ;C7D1: 32 
        FCB     $01                      ;C7D2: 01 
        FCB     $6D                      ;C7D3: 6D 
        FCB     $00,$8D,$00              ;C7D4: 00 8D 00 
        FCB     $73                      ;C7D7: 73 
        FCB     $00,$8D,$00              ;C7D8: 00 8D 00 
        FCB     $73                      ;C7DB: 73 
        FCB     $00,$BF,$01              ;C7DC: 00 BF 01 
        FCB     $6D                      ;C7DF: 6D 
        FCB     $00,$BF,$01              ;C7E0: 00 BF 01 
        FCB     $6D                      ;C7E3: 6D 
        FCB     $00,$00,$00              ;C7E4: 00 00 00 
        FCB     $73                      ;C7E7: 73 
        FCB     $00,$00,$00              ;C7E8: 00 00 00 
        FCB     $73                      ;C7EB: 73 
        FCB     $00,$BF                  ;C7EC: 00 BF 
        FCB     $77                      ;C7EE: 77 
hdlr_NMI RTI                              ;C7EF: 3B 

        ORG     $FFF0 

svec_IRQ_SCI FDB     hdlr_NMI                 ;FFF0: C7 EF 
svec_IRQ_T0F FDB     hdlr_NMI                 ;FFF2: C7 EF 
svec_IRQ_OCF FDB     hdlr_NMI                 ;FFF4: C7 EF 
svec_IRQ_ICF FDB     hdlr_NMI                 ;FFF6: C7 EF 
svec_IRQ_EXT FDB     hdlr_NMI                 ;FFF8: C7 EF 
svec_SWI FDB     hdlr_NMI                 ;FFFA: C7 EF 
svec_NMI FDB     hdlr_NMI                 ;FFFC: C7 EF 
svec_RST FDB     hdlr_RST                 ;FFFE: C0 00 

        END
