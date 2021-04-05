                    MEMBER()
!-------------------------------------------------------------------------
! CbVlbPreviewClass
! Improve Clarion Window Designer Previews with VLB to provide sample data
!-------------------------------------------------------------------------
! 04-Apr-2021   First Release, based in part on List Format Parser Preview
! 05-Apr-2021   Cache Number 123 in Queue, Color Equates at Top
! 05-Apr-2021   ENTRY(@s) with UPR or CAP get sample 'BROWN FOX'/'Brown Fox'  in .EntryInit()
!-------------------------------------------------------------------------

    INCLUDE('CbVlbPreview.INC'),ONCE
    MAP
ChrCount PROCEDURE(STRING Text2Scan, STRING ChrList),LONG
    END
COLOR_C_FG  EQUATE(COLOR:Maroon) ! * Text Color like a Red Shirt washed in HOT Water
COLOR_C_BG  EQUATE(0FAFAFFH)     ! * Back Pinkish White
COLOR_C_SFG EQUATE(COLOR:None)
COLOR_C_SBG EQUATE(COLOR:None)
COLOR_Z_FG  EQUATE(Color:Green)  !Z Col  Style Text 
COLOR_Z_BG  EQUATE(0E0FFFFH)     !Z Col  Style Back  Lt Yellow
COLOR_Y_FG  EQUATE(Color:Teal)   !Y Cell Style
COLOR_Y_BG  EQUATE(0E0FFFFH)     !Y Cell Style Back  Lt Yellow
n1234567890 EQUATE('1234567890123456789012345678901234567890')
sAtoZ       EQUATE('abcdefghijklmnopqrstuvwxyz') !Must be 26 bytes
NumQ QUEUE,PRE(NumQ)  !Cache sample numbers for Pictures
Picture CSTRING(24)
Sample  DECIMAL(27,5)
Sign    STRING(1)
     END 
!====================================================
ChrCount PROCEDURE(STRING Text2Scan, STRING ChrList)!LONG
X LONG,AUTO
CntChr LONG
    CODE
    LOOP X=1 TO SIZE(Text2Scan)
        IF INSTRING(Text2Scan[X],ChrList) THEN CntChr += 1.
    END
    RETURN CntChr
!--------------------------------------
CbVlbPreviewClass.Construct PROCEDURE()
    CODE
    SELF.ColumnQ &= NEW(CbVlbColumnQueueType)
    SELF.DataQ &= NEW(CbVlbDataQueueType)
    RETURN

!-------------------------------------
CbVlbPreviewClass.Destruct PROCEDURE()
    CODE
    DISPOSE(SELF.ColumnQ)
    DISPOSE(SELF.DataQ)
    RETURN

!---------------------------------------------
CbVlbPreviewClass.Init PROCEDURE(LONG ListFEQ, LONG RowCount=0)
K BYTE,AUTO
    CODE
    IF ~ListFEQ{PROP:Format} THEN RETURN.
    SELF.FEQ=ListFEQ
    SELF.Items=ListFEQ{PROP:Items} ; IF SELF.Items<1 THEN SELF.Items=1.
    IF RowCount < 1 THEN RowCount=SELF.Items * 2.
    SELF.RowCnt = RowCount
    SELF.ClmCnt = SELF.LoadColumnQ() ; IF SELF.ClmCnt=0 THEN RETURN.
    SELF.LoadDataQ()
    ListFEQ{PROP:VLBval} =ADDRESS(SELF)
    ListFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc)
    LOOP K=255 TO 1 BY -1 ; ListFEQ{PROP:Alrt,K}=0 ; END  !Mouseleft can screw things uo
    RETURN
!TODO Right-Click POPUP on LIST to Offer Tricks. register AlertKey  !!!

!----------------------------------------------------
CbVlbPreviewClass.LoadColumnQ PROCEDURE()
ListFEQ LONG,AUTO
VlbCnt  USHORT(1)
ColX    USHORT,AUTO
FieldX  USHORT,AUTO
InX     LONG,AUTO
Fmt     STRING(1024),AUTO
AZ      STRING(sAtoZ)
StyleZ  LONG
ColQ    &CbVlbColumnQueueType
DateNow LONG
TimeNow LONG
  CODE
  ListFEQ=SELF.FEQ
  DateNow=TODAY() ; TimeNow=CLOCK()
  FREE(SELF.ColumnQ) ; ColQ &= SELF.ColumnQ
  LOOP ColX=1 TO 1024  !ListFEQ{PROPLIST:Exists,0}
     IF ~ListFEQ{PROPList:Exists, ColX} THEN BREAK.
     CLEAR(ColQ)
     ColQ:ColNo   = ColX
     FieldX       = ListFEQ{PROPLIST:FieldNo, ColX}
     ColQ:Picture = ListFEQ{PROPLIST:Picture, ColX}
     ColQ:PicType = ColQ:Picture[2]
     ColQ:Format  = ListFEQ{PROPLIST:Format, ColX}  !for Debug in Q view
     IF ColQ:PicType<>'P' THEN ColQ:PicType=lower(ColQ:PicType).
     CASE ColQ:PicType
     OF 'n' ; ColQ:DataText = SELF.Sample_AtN(ColQ:Picture, ColQ:PicSign)
     OF 'e' ; ColQ:IsLong=1 ; ColQ:DataLong=1234500
     OF 'd' ; ColQ:IsLong=1 ; ColQ:DataLong=DateNow
     OF 't' ; ColQ:IsLong=1 ; ColQ:DataLong=TimeNow
     OF   'P'   !<-- Upper P
     OROF 'p' ; ColQ:DataText=SELF.Sample_AtP(ColQ:Picture)
     OF   'K'   !<-- Upper K
     OROF 'k' ; ColQ:DataText=SELF.Sample_AtK(ColQ:Picture)
     ELSE !'s'
            ColQ:DataText=UPPER(AZ[1]) & AZ[2:26] & AZ ; AZ=AZ[2:26] & AZ[1]
            IF ColQ:PicType='s' AND DEFORMAT(ColQ:Picture) <=3 THEN
               ColQ:DataText=UPPER(ColQ:DataText)
            END
     END
     DO AddColumnQ
    !    ---Sequence in Queue-------------------------------
    !    1. LIST Column data to          Various
    !    2. * Color Foreground           LONG  PROPLIST:Color
    !    3. * Color Background           LONG  PROPLIST:Color
    !    4. * Color Selected Foreground  LONG  PROPLIST:Color
    !    5. * Color Selected Background  LONG  PROPLIST:Color
    !  = 6. I Icon in {Prop:IconList,#}  LONG  PROPLIST:Icon
    !  = 6. J Icon in {Prop:IconList,#}  LONG  PROPLIST:IconTrn
    !    7. T Tree Level                 LONG  PROPLIST:Tree
    !    8. Y Style Number for Cell      LONG  PROPLIST:CellStyle
    !    9. P Tool Tip for Cell        STRING  PROPLIST:Tip

     IF ListFEQ{PROPLIST:Color, ColX} THEN
        CLEAR(ColQ)
        ColQ:Mod='*'
        ColQ:IsLong=1
        ColQ:DataLong=COLOR_C_FG  ; DO AddColumnQ  !Text
        ColQ:DataLong=COLOR_C_BG  ; DO AddColumnQ  !Back
        ColQ:DataLong=COLOR_C_SFG ; DO AddColumnQ  !Selected FG
        ColQ:DataLong=COLOR_C_SBG ; DO AddColumnQ  !SBG
     END
     IF ListFEQ{PROPLIST:Icon, ColX} THEN        !Icon
        CLEAR(ColQ)
        ColQ:Mod='I'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     ELSIF ListFEQ{PROPLIST:IconTrn, ColX} THEN  !Tran Icon
        CLEAR(ColQ)
        ColQ:Mod='J'
        ColQ:IsLong=1
        ColQ:DataLong=2    ; DO AddColumnQ
     END
     IF ListFEQ{PROPLIST:Tree, ColX} THEN        !Tree
        CLEAR(ColQ)
        ColQ:Mod='T'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     END

     StyleZ=ListFEQ{PROPLIST:ColStyle,ColX}             !Z(#) Col Style
     IF StyleZ THEN
        ListFEQ{PROPSTYLE:TextColor,StyleZ}=COLOR_Z_FG
        ListFEQ{PROPSTYLE:BackColor,StyleZ}=COLOR_Z_BG
     END
     IF ListFEQ{PROPLIST:CellStyle, ColX} THEN          !Y Cell Style
        CLEAR(ColQ)
        ColQ:Mod='Y'
        ColQ:IsLong=1
        ColQ:DataLong=255    ; DO AddColumnQ
        ListFEQ{PROPSTYLE:TextColor,255}=COLOR_Y_FG
        ListFEQ{PROPSTYLE:BackColor,255}=COLOR_Y_BG
     END
     IF ListFEQ{PROPLIST:Tip, ColX} THEN                !P Tip
        CLEAR(ColQ)
        ColQ:Mod='P'
        ColQ:DataText='Tip for Column ' & ColX ; DO AddColumnQ
     END
  END
  LOOP InX=1 TO 5
        !v1 ListFEQ{PROP:IconList,InX}=CHOOSE(InX,ICON:Print,ICON:Copy,ICON:Cut,ICON:Paste,ICON:Save)
        ListFEQ{PROP:IconList,InX}=CHOOSE(InX,'~CheckOn.ico','~CheckOff.ico',ICON:Print,ICON:Copy,ICON:Paste,ICON:Save)
  END  !TODO Add CheckOn.ico Off.ico from Images, and POPUP() offer option to make Icon Check
  PRAGMA('link(CheckOn.ico)') ; PRAGMA('link(CheckOff.ico)')

  RETURN RECORDS(ColQ)
AddColumnQ ROUTINE
    ColQ:FldNo=FieldX ; FieldX += 1
    Add(ColQ)
    EXIT
!-------------------------------------------------
CbVlbPreviewClass.LoadDataQ PROCEDURE()
ListFEQ LONG,AUTO
Items   USHORT,AUTO
Rows    USHORT,AUTO
RowX    USHORT,AUTO
ColX    USHORT,AUTO
RowMod5 BYTE,AUTO
TweakSample BYTE(0)
Yr      USHORT,AUTO
Date5   LONG,DIM(5)  !Date Data for VLB
Time5   LONG,DIM(5)  !Time Date
DataQ   &CbVlbDataQueueType
ColQ    &CbVlbColumnQueueType
    CODE
    ListFEQ=SELF.FEQ
    Date5[1]=TODAY()  ; Yr=YEAR(Date5[1])
    Date5[2]=Date( 2, 3,Yr)            ! variety with 1 and 2 digits
    Date5[3]=Date( 6,21,Yr)
    Date5[4]=Date(10, 4,Yr)
    Date5[5]=Date(12,25,Yr)
    Time5[1]=DEFORMAT('07:02:11',@t4)
    Time5[2]=DEFORMAT('09:23:22',@t4)
    Time5[3]=DEFORMAT('10:04:13',@t4)
    Time5[4]=DEFORMAT('11:45:24',@t4)
    Time5[5]=DEFORMAT('22:56:09',@t4)

    DataQ &= SELF.DataQ ; FREE(DataQ) ; CLEAR(DataQ)
    ColQ  &= SELF.ColumnQ
    Items=SELF.Items
    Rows =SELF.RowCnt   ! Items * 2
    LOOP RowX=1 TO Rows
       RowMod5 = (RowX-1) % 5 + 1
       IF RowX=Rows OR RowX>Items THEN TweakSample=True.
       LOOP ColX=1 TO RECORDS(ColQ)
          GET(ColQ,ColX)
          DO CellRtn
       END
       IF RowMod5=1 THEN Date5[1]+=34 ; Time5[1] += 60*100.
    END
    SORT(SELF.DataQ,SELF.DataQ.RowNo,SELF.DataQ.ColNo)
    RETURN
CellRtn ROUTINE
    DataQ.RowNo    = RowX
    DataQ.ColNo    = ColQ:FldNo   !not ColQ:ColNo
    DataQ.PicMod   = ColQ:PicType & ColQ:Mod
    DataQ.IsLong   = ColQ:IsLong
    DataQ.DataLong = ColQ:DataLong
    DataQ.DataText = ColQ:DataText
    CASE ColQ:Mod
    OF 'T' OROF 'I' OROF 'J'
        DataQ.DataLong = RowMod5  !Tree Level, Icons, there are 5
    END
    CASE ColQ:PicType
    OF 'd' ; DataQ.DataLong=Date5[RowMod5]
    OF 't' ; DataQ.DataLong=Time5[RowMod5]
    OF 's' ; IF TweakSample THEN DataQ.DataText=UPPER(DataQ.DataText).
    OF 'n' ; IF TweakSample AND ColQ:PicSign THEN DataQ.DataText = -1 * ColQ:DataText.
    END
    ADD(DataQ)
!------------------------------------
CbVlbPreviewClass.VLBprc PROCEDURE(LONG xRow, USHORT xCol)
DataQ   &CbVlbDataQueueType
  CODE
  CASE xRow
  OF -1 ; RETURN SELF.RowCnt !Rows
  OF -2 ; RETURN SELF.ClmCnt !Columns
  OF -3 ; RETURN SELF.Changed
  END
  DataQ &= SELF.DataQ
  DataQ.RowNo = xRow
  DataQ.ColNo = xCol
  GET(DataQ,DataQ.RowNo,DataQ.ColNo)
  IF ERRORCODE() THEN RETURN '?VLBproc:'& xRow&','& xCol .
  IF DataQ.IsLong THEN RETURN DataQ.DataLong.
  RETURN DataQ.DataText
!--------------------------------------------
CbVlbPreviewClass.Sample_AtN PROCEDURE(STRING Picture, <*STRING OutSign>)!,STRING
N       DECIMAL(21)
I       DECIMAL(2)
NewTry      CSTRING(32)
Worked_N    LIKE(N)     !Last Try w/o ####
Commas_N    LIKE(N)     !Last try #,###,### with most Commas (Grouping ,._)
CommaCnt    BYTE        !Want Max Commas, RTL will use Comma space for #
NewCount    BYTE
CPicture    CSTRING(25)
picFill     PSTRING(2)  ! _=Space *=**** 0=0000 kills Grouping
picGrouping PSTRING(2)  !1000s Grouping . _=Space   Default Comma
picPlaceSep PSTRING(2)  !Pennies . , v
picDecimals PSTRING(3)  !Number Decimal Digits e.g. 2 if @n9.2
picSign     STRING(1)   !- or (
    CODE
    Picture=lower(Picture)
    CPicture=CLIP(Picture)
    NumQ:Picture=CPicture
    GET(NumQ,NumQ:Picture)
    IF ~ERRORCODE() THEN GOTO ReturnQLabel: .
    DO ParseForCommaPeriodRtn
    N=1 ; I=2
    LOOP 20 TIMES
        NewTry=CLIP(LEFT(FORMAT(N,CPicture)))
        IF INSTRING('#',NewTry) THEN BREAK.   !Overflow shows #'s
        Worked_N = N
        IF LEN(picGrouping) THEN !Want Most Commas ','
           NewCount=ChrCount(NewTry,picGrouping) ! ','
           IF NewCount >= CommaCnt THEN
              Commas_N = Worked_N ; CommaCnt = NewCount
           END
        END
        N *= 10 ; N += I ; I += 1 ; IF I > 9 THEN I=0.
    END
    NumQ:Sample=CHOOSE(~Commas_N,Worked_N,Commas_N) + .12345
    NumQ:Sign=picSign
    ADD(NumQ,NumQ:Picture)
ReturnQLabel:
    IF ~OMITTED(OutSign) THEN OutSign=NumQ:Sign.
    RETURN NumQ:Sample
!-----------------------------
ParseForCommaPeriodRtn ROUTINE
    DATA
PX        SHORT
PicChr    STRING(1)
InTilde   BOOL        ! ~ currency ~
b4Numbers BOOL(1)     ! Before Numbers is Fill
    CODE
!@N [currency] [sign] [ fill ]  size  [ grouping ] [places] [sign] [currency] [B]
!       $ ~~     -(     0 _ *    12    . _(space)   .`v 2     -)      ~xxx~

    LOOP PX=3 TO LEN(CPicture) BY 1   !Forwards
        PicChr=CPicture[PX]
        IF PicChr='~' THEN InTilde=1-InTilde ; CYCLE.
        IF InTilde THEN CYCLE.                          !Ignore Inside ~ currency ~
        IF b4Numbers THEN      !Before Numbers is Fill
           ![fill]  Specifies leading zeros, spaces, or asterisks (*) in any leading zero positions, and suppressesdefault grouping. If the fill is omitted, leading zeros are suppressed.
           CASE PicChr
           OF '_'        ; picFill='_'                   !_ (underscore) Produces leading spaces
           OF '*'        ; picFill='*'                   !* (asterisk) Produces leading asterisks
           OF '0'        ; picFill='0' ; b4Numbers=False !0 (zero) Produces leading zeroes
           OF '1' TO '9' ; b4Numbers=False
           OF   '-'
           OROF '('      ; picSign=PicChr
           END
           CYCLE
        END

        ! [size]  The size is required to specify the total number of significant digits to display, including the number of digits in the places indicator and any formatting characters.
        ! [grouping] A grouping symbol, other than a comma (the default), can appear right of the size indicator to specify a three digit group separator.
        !   . (period) Produces periods
        !   _ (underscore) Produces spaces
        !
        ! [places]  Specifies the decimal separator symbol and the number of decimal digits. The number of decimal digits must be less than the size. The decimal separator may be a period (.), grave accent (' ) (produces periods grouping unless overridden), or the letter
        !  v (used only for STRING field storage declarations--not for display).
        !  . (period) Produces a period
        !  ` (grave accent) Produces
        !Not implemented is double .. `` __ that make Grouping and Decimal the same
        CASE PicChr
        OF '.'  ; IF ~picGrouping THEN picGrouping='.' ELSE picPlaceSep='.'.  !Canhave .. nonsense
        OF '_'  ; picGrouping='_'
        OF '`'  ; picPlaceSep=',' ; IF ~picGrouping THEN picGrouping='.'.     !Comma decimal defualts to decimal grouping
        OF 'v'  ; picPlaceSep='v'
        OF '0' TO '9'
                  IF ~picPlaceSep THEN          !No Decimal Point ... but
                     IF picGrouping='.' |       !Period alone then numbers is Decimal
                     OR picGrouping='_' THEN    !Underscore alone is Space for Decimal
                           picPlaceSep=picGrouping   !So decimal is '.' or '_'
                           picGrouping=','                        !
                     END
                  END
                  IF picGrouping OR picPlaceSep THEN
                     picDecimals=picDecimals & PicChr
                  END
        OF '-' ; picSign=PicChr  !No ) catch ( before
        END

    END
    IF    picFill THEN         picGrouping=''   !Any Fill _*0 Removes Grouping
    ELSIF ~picGrouping THEN    picGrouping=','  !Default Group is comma
    ELSIF picGrouping='_' THEN picGrouping=' '  !_ is ' ' Space
    END
    IF picDecimals=0 THEN picPlaceSep=''.
    EXIT
!-------------------------------------
CbVlbPreviewClass.Sample_AtP PROCEDURE(STRING Picture)!,STRING
    CODE
    RETURN SUB(n1234567890,1,ChrCount(Picture,'<#'))
!-------------------------------------
CbVlbPreviewClass.Sample_AtK PROCEDURE(STRING Picture)!,STRING
LenPic USHORT,AUTO
X USHORT,AUTO
Sample STRING(SIZE(Picture)),AUTO
S USHORT
Ch1 STRING(1)
N10 STRING('1234567890')
AZ STRING(sAtoZ)
    CODE
    LenPic=LEN(CLIP(Picture))
    IF UPPER(Picture[LenPic])='B' THEN LenPic-=1. !Cutoff "B" blank, assume K on end
    LOOP X=3 TO LenPic-1    !@K 3456789 KB
        CASE Picture[X]         !@K[@][#][<][x][\][?][^][_][|]K[B]
        OF '#'                  ! #   Specifies an integer 0 through 9.
      OROF '<<' ; Ch1=N10       ! <   Specifies an integer that is blank for high order zeros.
                      N10=N10[2:10] & N10[1]

        OF '^' ; Ch1=UPPER(AZ)  ! ^   Specifies only uppercase alphabetic characters in this position.
      OF   '_'                  ! _   Underscore specifies only lowercase alphabetic characters in this position.
      OROF '@' ; Ch1=AZ         ! @   Specifies only uppercase and lowercase alphabetic characters.

        OF '\' ; X+=1           ! \   Indicates the following character is a display character. This allows you to include any of the picture formatting characters (@,#,<,\,?,^,_,|) within the string as a display character.
                 Ch1=Picture[X+1]
        OF '|' ; CYCLE          ! |   Allows the operator to "stop here" if there are no more characters to input. Only the data entered and any display characters up to that point will be in the string result.
        OF '?' ; Ch1='?'        ! ?   Specifies any input character may be placed in this position.
        ELSE ; Ch1=Picture[X]  ! x   Represents optional constant display characters (any displayable character). These characters appear in the final result string.
        END !Case
        S+=1 ; Sample[S]=Ch1
        IF lower(Ch1)=AZ[1] THEN AZ=AZ[2:26] & AZ[1].
    END !Loop
    RETURN SUB(Sample,1,S)

!===================================================================

CbVlbAllPreviewClass.Construct PROCEDURE()
    CODE
    SELF.VlbClassQ &= NEW(CbVlbAllPreviewQueueType)
    RETURN
CbVlbAllPreviewClass.Destruct  PROCEDURE()
Ndx USHORT,AUTO
    CODE
    IF SELF.VlbClassQ &= NULL THEN RETURN.
    LOOP Ndx=RECORDS(SELF.VlbClassQ) TO 1 BY 1
        GET(SELF.VlbClassQ,Ndx)
        DISPOSE(SELF.VlbClassQ.VlbCls)
    END
    DISPOSE(SELF.VlbClassQ)
    RETURN

CbVlbAllPreviewClass.Init PROCEDURE()
FEQ LONG,AUTO
    CODE
    IF SELF.ListIsInit THEN RETURN. ; SELF.ListIsInit=1
    IF BAND(KEYSTATE(),0100h) THEN RETURN.  !If Shift is Down skip
    FEQ=0
    LOOP
        FEQ=0{PROP:NextField,FEQ} ; IF ~FEQ THEN BREAK.
        CASE FEQ{PROP:Type}
        OF CREATE:list OROF CREATE:combo
        ELSE ; CYCLE
        END
        IF ~FEQ{PROP:Format} THEN CYCLE.
        CLEAR(SELF.VlbClassQ)
        SELF.VlbClassQ.ListFEQ=FEQ
        SELF.VlbClassQ.Fmt=FEQ{PROP:Format}
        SELF.VlbClassQ.VlbCls &= NEW(CbVlbPreviewClass)
        ADD(SELF.VlbClassQ)
        SELF.VlbClassQ.VlbCls.Init(FEQ)
    END
    RETURN

CbVlbAllPreviewClass.EntryInit PROCEDURE()  !Setup ENTRY controls with 1234
FEQ LONG,AUTO
Picture STRING(32),AUTO
!Change2 STRING(32),AUTO
VlbCls  CbVlbPreviewClass
CapText EQUATE('The Quick Brown Fox Jumps Over The Lazy Dog')
UprText EQUATE('THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG')
    CODE
    IF SELF.EntryIsInit THEN RETURN. ; SELF.EntryIsInit=1
    IF BAND(KEYSTATE(),0100h) THEN RETURN.  !If Shift is Down skip
    FEQ=0
    LOOP
        FEQ=0{PROP:NextField,FEQ} ; IF ~FEQ THEN BREAK.
        CASE FEQ{PROP:Type}
        OF CREATE:Entry OROF CREATE:Spin OROF CREATE:SString OROF CREATE:Combo
        ELSE ; CYCLE
        END
        Picture=FEQ{PROP:Text}
        IF Picture[1]<>'@' THEN Picture='@' & Picture.
        CASE lower(Picture[2])
        OF 'n' ; CHANGE(FEQ,VlbCls.Sample_AtN(Picture))
        OF 'p' ; CHANGE(FEQ,VlbCls.Sample_AtP(Picture))
        OF 'k' ; CHANGE(FEQ,VlbCls.Sample_AtK(Picture)) ! ; FEQ{PROP:Tip}=Picture &'<13,10>'& CONTENTS(FEQ)
        OF 's' ; IF FEQ{PROP:Upr} THEN
                    CHANGE(FEQ,UprText)
                 ELSIF FEQ{PROP:Cap} THEN 
                    CHANGE(FEQ,CapText)
                 END 
        END
    END
    DISPLAY
    RETURN
