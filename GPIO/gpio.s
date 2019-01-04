;
; C=GPIO BASIC WEDGE  V1.0
; ========================
;
; THIS BASIC WEDGE ADDS THE EXCLAMATION POINT (!) AS A NEW COMMAND TO BASIC.
; HERE'S HOW IT WORKS:
; 
; USE:  !{PORT(0-31)},{COMMAND(0-5)} TO CONTROL A GPIO PORT. 
; 
;   !0,0 : REM  SET PIN 0 TO LOW (0V)
;   !7,1 : REM  SET PIN 7 TO HIGH (5V)
;   !5,3 : REM  SET PIN 5 TO INPUT MODE
; 
; 
; USE:  !{PORT(0-31)} TO READ A GPIO INPUT PORT.
;
;   PRINT !5 : REM  PRINTS 0 IF PIN 5 IS LOW, OR 1 IF IT IS HIGH
;   P = !5   : REM  SET VAR 'P' TO THE VALUE OF PIN 5 (0=LOW, 1=HIGH)
;
; 
;PORT COMMANDS
;  CMD DESCRIPTION                 
; +---+----------------------------+
; | 0 |SET OUTPUT PIN TO LOW (0V)  |
; | 1 |SET OUTPUT PIN TO HIGH (5V) |
; | 2 |SET PIN MODE TO OUTPUT      |
; | 3 |SET PIN MODE TO INPUT       |
; | 4 |TURN OFF PIN PULLUP RESISTOR|
; | 5 |TURN ON PIN PULLUP RESISTOR | 
; +---+----------------------------|
;
; ERROR MESSAGES
; *  ?I2C BUS  ERROR  -- CANNOT FIND THE C=GPIO, MAKE SURE IT IS PLUGGED IN.
; *  ?BAD PORT ERROR  -- AN INCORRECT PORT NUMBER WAS USED, USE 0 TO 31
; *  ?BAD CMD  ERROR  -- AN INCORRECT COMMAND NUMBER WAS USED, USE 0 TO 5

;   COMMODORE 64
;   UNCOMMENT TO BUILD FOR FOR C64
COMP    = "C64"
;   THEN:
;     LOAD "GPIO.PRG",8,1
;     SYS 49152

;   COMMODORE VIC-20
;   UNCOMMENT TO BUILD FOR VIC-20
;COMP    = "VIC20"
;   THE VIC-20 BUILD IS SET UP FOR CARTRIDGE, BUT IF YOU WANT TO LOAD IT IN FROM
;   DISK OR TAPE, USE  SYS 40969  TO START.

        PROCESSOR 6502

; SDA BIT7 $80 ON IODDIR
; SCL BIT0 $01 ON IODDIR

SHDREG  = $F9
BITSEL  = $FA
DEVICE  = $FB
ZP      = $FC
ADDR    = $FD
DATA    = $FE
SHADOW  = $0293

        IF COMP == "C64"
        ECHO "C64 MODE"
IOPORT  = $DD01
IODDIR  = $DD03
BASIC   = $A000
        ORG $C000
        ENDIF

        IF COMP == "VIC20"
        ECHO "VIC20 MODE"
IOPORT  = $9110
IODDIR  = $9112
BASIC   = $C000
        ORG $A000 ; RAM EXPANSION AT BLOCK 5
        ENDIF

CHRGET  = $0073
CHRGOT  = $0079
SYNERR  = BASIC + $0F08
NEWSTT  = BASIC + $07AE
GONE    = BASIC + $07E4
STROUT  = BASIC + $0B1E
BASWARM = BASIC + $0474
BASBYTE = BASIC + $179B
BASCOMA = BASIC + $0EFD
BASFUN  = BASIC + $0E86
BASERR  = BASIC + $0469
BASRETV = BASIC + $1391

CODESTART
        IF COMP == "VIC20"
        DC.W RESET
        DC.W NMI
        DC.B $41,$30,$C3,$C2,$CD    ;ROM PRESENT SIGNATURE
        ENDIF        

RESET
        IF COMP == "VIC20"
        ; VIC-20 RESET
        JSR $FD8D             ; INITIALISE AND TEST RAM
        JSR $FD52             ; RESTORE DEFAULT I/O VECTORS
        JSR $FDF9             ; INITIALIZE I/O REGISTERS
        JSR $E518             ; INITIALISE HARDWARE
        CLI
        JSR $E45B             ; INITIALISE BASIC VECTOR TABLE
        JSR $E3A4             ; INITIALISE BASIC RAM LOCATIONS
        JSR $E404             ; PRINT START UP MSG AND INITIALISE MEMORY PTRS
        LDX #$FB              ; VALUE FOR START STACK
        TXS                   ; SET STACK POINTER
        ENDIF        

        LDY #>BOOTMSG
        LDA #<BOOTMSG
        JSR STROUT            ; DISPLAY THE C=GPIO BOOT MESSAGE

        LDA #<WEDGE_O         ; HOOK INTO BASIC VECTORS FOR COMMANDS
        STA $0308             ; THIS IS USED TO OUTPUT TO THE DEVICE
        LDA #>WEDGE_O
        STA $0309

        LDA #<WEDGE_I         ; HOOK INTO BASIC VECTORS FOR EXPRESSIONS
        STA $030A             ; THIS IS USED TO READ FROM THE DEVICE
        LDA #>WEDGE_I
        STA $030B

        LDX #11               ; CLEAR SHADOW REGISTERS WHICH OCCUPY
        LDA #0                ; THE RS-232 CONTROL REGISTER SPACE
.1      STA SHADOW,X          ; (12 BYTES) FROM $0293 TO $029E
        DEX
        BNE .1
        LDA #$FF
        STA SHADOW+$1
        STA SHADOW+$4
        STA SHADOW+$7
        STA SHADOW+$A

        IF COMP == "VIC20"
        JMP BASWARM           ; READY
        ENDIF        

NMI
        RTS                   ; NMI JUST RETURNS

BIT_TBL DC.B $01,$02,$04,$08,$10,$20,$40,$80
REG_TBL DC.B $12,$12,$00,$00,$0C,$0C
SHD_LUT DC.B $00,$00,$01,$01,$02,$02,$FF,$FF
        DC.B $03,$03,$04,$04,$05,$05,$FF,$FF
        DC.B $06,$06,$07,$07,$08,$08,$FF,$FF
        DC.B $09,$09,$0A,$0A,$0B,$0B,$FF,$FF

        ; ASSIST BY CONVERTING PORT NUM TO DEVICE AND GETTING BIT SELECTION
        ; ON RETURN EITHER S IS HIGH INDICATING RAW I2C ACCESS
        ; OR A = ADDR OFFSET
GETPRMS SUBROUTINE
        JSR BASBYTE           ; FETCH A BYTE FROM BASIC TO USE AS THE PORT
        TXA
        BPL .WEDGE2           ; IF PORT > 127
        RTS                   ; RETURN WITH S HIGH
.WEDGE2 CMP #$20              ; ELSE IF PORT <= 127 THEN ENSURE PORT < 32
        BCC .WEDGE3
        LDA #<BADPORT         ; PORTS 32 TO 127 ARE INVALID
        LDY #>BADPORT
        JMP BASERR            ; ..SHOW "BAD PORT" BASIC ERROR
.WEDGE3 AND #7                ; TAKE BOTTOM 3 BITS OF PORT
        TAY
        LDA BIT_TBL,Y         ; ... AS A BIT POSITION
        STA BITSEL            ; STORE THAT IN BITSEL

        CPX #$10              ; IF PORT >= 16 THEN USE DEVICE 2
        BCC .DEV1
        LDA #$42              ; USE DEVICE 2
        DC.B $2C ; BIT SKIP
.DEV1   LDA #$40              ; ELSE USE DEVICE 1
        STA DEVICE            ; SAVE DEVICE NUM FOR I2C ROUTINES

        TXA                   ; PUT PORT BITS3:4 INTO SHDREG
        AND #$18                                    
        STA SHDREG

        TXA
        LSR
        LSR
        LSR
        AND #1                ; GET PORT BIT3 AS ADDR OFFSET
        RTS

        ; HANDLE WEDGE USED AS BASIC COMMAND
WEDGE_O SUBROUTINE
        JSR CHRGET            ; GET THE NEXT BASIC CHAR
        PHP                   ; SAVE CARRY IN CASE WE NEED TO PASS TO BASIC
        CMP #"!"              ; ! IS OUR WEDGE COMMAND CHAR
        BEQ .WEDGE
        PLP                   ; RECOVER THE CARRY...
        JMP GONE+3            ; NOT THE WEDGE, CONT TO BASIC

.WEDGE  PLP                   ; IT'S THE WEDGE CHAR, THROW AWAY SAVED CARRY
        JSR GETPRMS           ; CONVERT PORT INTO DEVICE AND ADDR
        BPL .NOTRAW           ; RETURNS S=HIGH IF RAW
        JMP I2C_O             ; USE IT AS A RAW I2C DEVICE NUMBER
.NOTRAW STA ADDR              ; STORE PORT BIT3 INTO BIT0 OF ADDR
        JSR BASCOMA           ; CHECK FOR A COMMA
        JSR BASBYTE+3         ; FETCH A VALUE FROM BASIC
        CPX #6                ; VALUES >= 6 ARE INVALID
        BCC .VAL_OK
        LDA #<BADVAL
        LDY #>BADVAL
        JMP BASERR            ; ..SHOW "BAD VALUE" BASIC ERROR
.VAL_OK
        LDA ADDR              ; PUT APPROPRIATE REGISTER INTO ADDR
        ORA REG_TBL,X
        STA ADDR

        TXA                   ; A = VALUE
        AND #1                ; IF VALUE BIT0==0 THEN WE ARE CLEARING A BIT
        BNE .SETBIT

        TXA
        ORA SHDREG
        TAY
        LDX SHD_LUT,Y 

        LDA BITSEL            ; CLEAR THE BIT...
        EOR #$FF              ; FLIP THE BITS SO WE CAN...
        AND SHADOW,X          ; LOGICAL AND TO CLEAR THE BIT
        JMP .WRITE            ; JMP TO .WRITE

.SETBIT
        TXA
        ORA SHDREG
        TAY
        LDX SHD_LUT,Y 

        LDA BITSEL            ; LOAD THE BIT TO SET
        ORA SHADOW,X          ; OR IT WITH THE SHADOW REGISTER

.WRITE
        STA SHADOW,X          ; SAVE THE CHANGE TO SHADOWS
        STA DATA              ; AND TO DATA
        SEI                   ; TURN OFF INTERRUPTS
        JSR INIT              ; INITIALIZE THE BUS PROTOCOL
        JSR WRITE             ; WRITE THE BYTE(DATA) TO I2C
        CLI                   ; TURN ON INTERRUPTS
        JMP NEWSTT            ; HANDLE NEXT BASIC STATEMENT

        ; HANDLE WEDGE USED AS BASIC VALUE
WEDGE_I SUBROUTINE
        LDA $7A               ; PUSH CURRENT BASIC BYTE ONTO STACK
        PHA
        LDA $7B
        PHA
        JSR CHRGET            ; GET THE NEXT BASIC CHAR
        CMP #"!"              ; ! IS OUR WEDGE COMMAND CHAR
        BEQ .1

        PLA                   ; IT'S NOT OUR STUFF
        STA $7B               ; ROLL BACK THE CHRGET AND...
        PLA
        STA $7A
        JMP BASIC+$0E86       ; ...SEND TO BASIC
.1
        PLA                   ; IT IS OUR STUFF
        PLA
        JSR GETPRMS           ; CONVERT PORT INTO DEVICE AND ADDR
        BPL .NOTRAW           ; RETURNS S=HIGH IF RAW
        JMP I2C_I             ; USE IT AS A RAW I2C DEVICE NUMBER
.NOTRAW ORA REG_TBL+0         ; ADD THE GPIO REGISTER TO ADDR OFFSET
        STA ADDR              ; STORE IN ADDR
        SEI                   ; TURN OFF INTERRUPTS
        JSR INIT              ; INITIALIZE THE BUS PROTOCOL
        JSR READ              ; READ THE BYTE
        CLI                   ; TURN ON INTERRUPTS
        LDA DATA              ; GET THE READ DATA
        AND BITSEL            ; SELECT THE BIT
        BEQ .ZERO             ; IF NOT ZERO SET TO 1 AND STORE IN Y
        LDY #1
        DC.B $24 ; BIT SKIP
.ZERO   TAY
        LDA #$00              ; SEND THE BIT BACK TO BASIC
        JMP BASRETV

        ; HANDLE WEDGE USED AS RAW I2C WRITE
I2C_O
        TXA                   ; FROM GETPRMS, X HAS DEVICE NUMBER
        AND #$7F              ; CLEAR TOP BIT
        STA DEVICE            ; STORE IN DEVICE
        JSR BASCOMA           ; CHECK FOR A COMMA
        JSR BASBYTE+3         ; FETCH A BYTE FROM BASIC
        STX ADDR              ; STORE AS ADDR
        JSR BASCOMA           ; CHECK FOR A COMMA
        JSR BASBYTE+3         ; FETCH A BYTE FROM BASIC
        STX DATA              ; STORE AS DATA
        SEI                   ; TURN OFF INTERRUPTS
        JSR INIT              ; INITIALIZE THE BUS PROTOCOL
        JSR WRITE             ; WRITE THE BYTE(DATA) TO I2C
        CLI                   ; TURN ON INTERRUPTS
        JMP NEWSTT            ; HANDLE NEXT BASIC STATEMENT

        ; HANDLE WEDGE USED AS RAW I2C READ
I2C_I
        TXA                   ; FROM GETPRMS, X HAS DEVICE NUMBER
        AND #$7F              ; CLEAR TOP BIT
        STA DEVICE            ; STORE IN DEVICE
        JSR BASCOMA           ; CHECK FOR A COMMA
        JSR BASBYTE+3         ; FETCH A BYTE FROM BASIC
        STX ADDR
        SEI                   ; TURN OFF INTERRUPTS
        JSR INIT              ; INITIALIZE THE BUS PROTOCOL
        JSR READ              ; READ THE BYTE
        CLI                   ; TURN ON INTERRUPTS
        LDY DATA              ; GET THE READ DATA
        LDA #$00              ; SEND THE BYTE BACK TO BASIC
        JMP BASRETV

        ; I2C SUBROUTINES
        SUBROUTINE
SDA1    LDA #$3F              ; JSR HERE TO SET SDA
        DC.B $2C ; BIT SKIP
SCK1    LDA #$FE              ; JSR HERE TO SET SCK
        AND IODDIR
        STA IODDIR
        RTS
SDA0    LDA #$80              ; JSR HERE TO CLEAR SDA
        DC.B $2C ; BIT SKIP
SCK0    LDA #$01              ; JSR HERE TO CLEAR SCK
        ORA IODDIR
        STA IODDIR
        RTS

INIT    SUBROUTINE            ; INITIALIZES THE I2C BUS
        LDA #0
        STA IOPORT            ; CLEAR USER PORT BITS
        STA IODDIR            ; MAKE USER PORT INPUTS
        JSR SDA1              ; I2C PROTOCOL FOR INIT
        JSR SCK0              ; SDA HIGH, SCK LOW...
        LDX #4
.1      JSR STOP              ; ...THEN SEND STOP FOUR TIMES
        DEX
        BNE .1
        RTS

START   SUBROUTINE            ; I2C PROTOCOL FOR STARTING A TRANSFER
        JSR SCK1
        JSR SDA1
        JSR SDA0
        JSR SCK0
        JSR SDA1
        RTS

STOP    SUBROUTINE            ; I2C PROTOCOL FOR STOPPING A TRANSFER
        JSR SDA0
        JSR SCK1
        JSR SDA1
        RTS

PUTBYTE SUBROUTINE            ; SERIALLY WRITE BYTE TO I2C BUS
        STA ZP
        LDX #8                ; 8 TIMES..
.3      BIT ZP                ; ..SEND THE HIGH BIT
        BMI .1
        JSR SDA0
        JMP .2
.1      JSR SDA1
.2      JSR SCK1
        JSR SCK0
        ASL ZP                ; SHIFT TO THE NEXT BIT
        DEX                   ; REPEAT
        BNE .3
        JSR SDA1
        RTS

GETBYTE SUBROUTINE            ; SERIALLY READ A BYTE FROM THE I2C BUS
        LDX #8
.1      JSR SCK1
        LDA IOPORT            ; READ FROM THE DEVICE INTO BIT 7
        ROL                   ; SHIFT THAT INTO ZP..
        ROL ZP
        JSR SCK0
        DEX                   ; ..8 TIMES..
        BNE .1
        JSR SDA1
        LDA ZP                ; ..TO GET OUR BYTE.
        RTS

GIVEACK SUBROUTINE            ; SEND AN ACKNOWLEDGEMENT SEQUENCE
        JSR SDA0
        JSR SCK1
        JSR SCK0
        JSR SDA1
        RTS

GETACK  SUBROUTINE            ; WAIT FOR AN ACKNOWLEDGEMENT SEQUENCE
        LDX #0
        LDY #0
        JSR SDA1
        JSR SCK1
.1      DEX                   ; X/Y IS 16 BIT WATCHDOG HERE
        BEQ .2                ; IF WE DEX ALL THE WAY TO ZERO JUMP DOWN AND DEY
        BIT IOPORT            ; READ FROM THE DEVICE INTO BIT 7
        BMI .1                ; DEVICE HAS ACKNOWLEDGED WHEN BIT GOES LOW
        JSR SCK0
        RTS
.2      DEY
        BNE .1                ; IF WE DEY ALL THE WAY TO ZERO..
        JSR STOP              ; ..TIMEOUT AND..
        PLA
        PLA
        LDA #<BUSERROR
        LDY #>BUSERROR
        JMP BASERR            ; ..SHOW A BASIC ERROR


SENDADR SUBROUTINE
        JSR START             ; SENDS I2C START CODE
        LDA DEVICE
        AND #$FE              ; LOW BIT 0 MEANS WRITE
        JSR PUTBYTE           ; WRITE OUT THE DEVICE ID BYTE AND..
        JSR GETACK            ; ..WAIT FOR AN OK
        LDA ADDR
        JSR PUTBYTE           ; SEND THE DEVICE REGISTER ADDRESS AND..
        JSR GETACK            ; ..WAIT FOR AN OK
        RTS

WRITE   SUBROUTINE
        JSR SENDADR           ; SELECT OUR DEVICE AND REGISTER
        LDA DATA
        JSR PUTBYTE           ; WRITE OUT THE DATA BYTE AND..
        JSR GETACK            ; ..WAIT FOR AN OK
        JSR STOP              ; THEN STOP
        RTS

READ    SUBROUTINE
        JSR SENDADR           ; SELECT OUR DEVICE AND REGISTER
        JSR START             ; I2C RESTART REQUIRED BECAUSE OF SWITCH TO READ
        LDA DEVICE
        ORA #$01              ; HIGH BIT 0 MEANS READ
        JSR PUTBYTE           ; WRITE OUT THE DEVICE ID BYTE AND..
        JSR GETACK            ; ..WAIT FOR AN OK
        JSR GETBYTE           ; READ THE RESULTING BYTE AND..
        STA DATA              ; ..SAVE IT
        JSR STOP              ; THEN STOP
        RTS

BOOTMSG = *
        DC.B $0D, "C=GPIO ENABLED", $0D, 0
BUSERROR = *
        DC.B "?I2C BUS  ERROR", 0
BADPORT = *
        DC.B "?BAD PORT  ERROR", 0
BADVAL  = *
        DC.B "?BAD CMD  ERROR", 0

CODEEND
        ECHO (CODEEND-CODESTART), "BYTES USED."
        ECHO "SYS", RESET, "TO START."
