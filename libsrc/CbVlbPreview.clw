                    MEMBER()
!-------------------------------------------------------------------------
! CbVlbPreviewClass
! Improve Clarion Window Designer Previews with VLB to provide sample data
!-------------------------------------------------------------------------
! 04-Apr-2021   First Release, based in part on List Format Parser Preview
! 05-Apr-2021   Cache Number 123 in Queue, Color Equates at Top
! 05-Apr-2021   ENTRY(@s) with UPR or CAP get sample 'BROWN FOX'/'Brown Fox'  in .EntryInit()
! 06-Apr-2021   Right-Click List Popup menu, Double Click Edit Value 
! 08-Apr-2021   Override IDE Z Style Font changes
! 08-Apr-2021   Edit Data [...] .PopupSample() data. UPR tied to Column
! 09-Apr-2021   Edit Data improvements - show bytes, date input spin
! 09-Apr-2021   Muliple LISTs. Turn off Mark. Added Tree Click2 then commented. Negative Samples.
!-------------------------------------------------------------------------

    INCLUDE('KEYCODES.CLW'),ONCE
    INCLUDE('CbVlbPreview.INC'),ONCE
DbugQs EQUATE(0)
!    INCLUDE('cbWndPreview.inc')
!DbugPrv CbWndPreviewClass    
    MAP
ChrCount PROCEDURE(STRING Text2Scan, STRING ChrList),LONG
    END
COLOR_C_FG  EQUATE(COLOR:Maroon) ! * Text Color like a Red Shirt washed in HOT Water
COLOR_C_BG  EQUATE(0FAFAFFH)     ! * Back Pinkish White
COLOR_C_SFG EQUATE(COLOR:None)   ! * Selected normal
COLOR_C_SBG EQUATE(COLOR:None)
COLOR_Z_FG  EQUATE(Color:Green)  !Z Col  Style Text 
COLOR_Z_BG  EQUATE(0E0FFFFH)     !Z Col  Style Back  Lt Yellow
COLOR_Y_FG  EQUATE(Color:Teal)   !Y Cell Style
COLOR_Y_BG  EQUATE(0E0FFFFH)     !Y Cell Style Back  Lt Yellow
n1234567890 EQUATE('1234567890123456789012345678901234567890')
sAtoZ       EQUATE('abcdefghijklmnopqrstuvwxyz') !Must be 26 bytes
QikBrownCap EQUATE('The Quick Brown Fox Jumps Over The Lazy Dog ')
QikBrownFox EQUATE('The quick brown fox jumps over the lazy dog ')
NumQ QUEUE,PRE(NumQ)  !Cache sample numbers for Pictures
Picture CSTRING(24)
Sample  DECIMAL(27,5)
Sign    STRING(1)
     END 
G:Alter BYTE(1)     
!====================================================
ChrCount PROCEDURE(STRING Txt, STRING ChrList)!LONG
X LONG,AUTO
C LONG
  CODE
  IF SIZE(ChrList)=1 THEN 
    LOOP X=1 TO SIZE(Txt)
        IF VAL(Txt[X])=VAL(ChrList[1]) THEN C += 1.
    END
  ELSE
    LOOP X=1 TO SIZE(Txt)
        IF INSTRING(Txt[X],ChrList) THEN C += 1.
    END
  END
  RETURN C
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
    ListFEQ{PROP:Mark}='' !Mark requires Q so disable. Seemed to work, needs Queue.
    SELF.ClmCnt = SELF.LoadColumnQ() ; IF SELF.ClmCnt=0 THEN RETURN.
    SELF.LoadDataQ()
    ListFEQ{PROP:VLBval} =ADDRESS(SELF)
    ListFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc)
    LOOP K=255 TO 1 BY -1 ; ListFEQ{PROP:Alrt,K}=0 ; END  !Mouseleft can screw things uo
    REGISTEREVENT(EVENT:NewSelection, ADDRESS(SELF.TakeEvent), ADDRESS(SELF),,ListFEQ)  
!    REGISTEREVENT(EVENT:Expanded,  ADDRESS(SELF.TakeEvent),ADDRESS(SELF),,ListFEQ)  Let Tree Click 2
!    REGISTEREVENT(EVENT:Contracted,ADDRESS(SELF.TakeEvent),ADDRESS(SELF),,ListFEQ)  expand & contract  
    RETURN
!----------------------------------------------------
CbVlbPreviewClass.LoadColumnQ PROCEDURE()
ListFEQ LONG,AUTO
VlbCnt  USHORT(1)
ColX    USHORT,AUTO
FieldX  USHORT,AUTO
ColPointer USHORT,AUTO
ColMods    STRING(5),AUTO  !Modifiers for Column
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
     ColQ:PicType = lower(ColQ:Picture[2])
     ColQ:Format  = ListFEQ{PROPLIST:Format, ColX}  !for Debug in Q view
     CASE ColQ:PicType
     OF 'n' ; ColQ:DataText = SELF.Sample_AtN(ColQ:Picture, ColQ:PicSign)
     OF 'e' ; ColQ:IsLong=1 ; ColQ:DataLong=1234500
     OF 'd' ; ColQ:IsLong=1 ; ColQ:DataLong=DateNow
     OF 't' ; ColQ:IsLong=1 ; ColQ:DataLong=TimeNow
     OF 'p' ; ColQ:DataText=SELF.Sample_AtP(ColQ:Picture)
     OF 'k' ; ColQ:DataText=SELF.Sample_AtK(ColQ:Picture)
     ELSE !'s'
            ColQ:DataText=UPPER(AZ[1]) & AZ[2:26] & AZ ; AZ=AZ[2:26] & AZ[1]
            IF ColQ:PicType='s' AND DEFORMAT(ColQ:Picture) <=3 THEN
               ColQ:DataText=UPPER(ColQ:DataText)
            END
     END
     DO AddColumnQ ; ColPointer=POINTER(ColQ) ; ColMods=''
    !    ---Sequence in Queue-------------------------------
    !    1. LIST Column data to          Various                   ModCol
    !    2. * Color Foreground           LONG  PROPLIST:Color      1 *
    !    3. * Color Background           LONG  PROPLIST:Color
    !    4. * Color Selected Foreground  LONG  PROPLIST:Color
    !    5. * Color Selected Background  LONG  PROPLIST:Color
    !  = 6. I Icon in {Prop:IconList,#}  LONG  PROPLIST:Icon       2 I
    !  = 6. J Icon in {Prop:IconList,#}  LONG  PROPLIST:IconTrn    2 J
    !    7. T Tree Level                 LONG  PROPLIST:Tree       3 T
    !    8. Y Style Number for Cell      LONG  PROPLIST:CellStyle  4 Y  5 Z
    !    9. P Tool Tip for Cell        STRING  PROPLIST:Tip        

     IF ListFEQ{PROPLIST:Color, ColX} THEN       !*Colors
        CLEAR(ColQ)
        ColQ:ModFld='*' ; ColMods[1]='*'
        ColQ:IsLong=1
        ColQ:DataLong=COLOR_C_FG  ; DO AddColumnQ  !Text
        ColQ:DataLong=COLOR_C_BG  ; DO AddColumnQ  !Back
        ColQ:DataLong=COLOR_C_SFG ; DO AddColumnQ  !Selected FG
        ColQ:DataLong=COLOR_C_SBG ; DO AddColumnQ  !SBG
     END
     IF ListFEQ{PROPLIST:Icon, ColX} THEN        !Icon
        CLEAR(ColQ)
        ColQ:ModFld='I' ; ColMods[2]='I'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     ELSIF ListFEQ{PROPLIST:IconTrn, ColX} THEN  !Tran Icon J
        CLEAR(ColQ)
        ColQ:ModFld='J' ; ColMods[2]='J'
        ColQ:IsLong=1
        ColQ:DataLong=2    ; DO AddColumnQ
     END
     IF ListFEQ{PROPLIST:Tree, ColX} THEN        !Tree
        CLEAR(ColQ)
        ColQ:ModFld='T' ; ColMods[3]='T'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     END

     StyleZ=ListFEQ{PROPLIST:ColStyle,ColX}      !Z(#) Col Style
     IF StyleZ THEN      ;   ColMods[5]='Z'
        ListFEQ{PROPSTYLE:TextColor,StyleZ}=COLOR_Z_FG
        ListFEQ{PROPSTYLE:BackColor,StyleZ}=COLOR_Z_BG
        ListFEQ{PROPSTYLE:FontName ,StyleZ}=ListFEQ{PROP:FontName}  !Undo IDE Preview Z
        ListFEQ{PROPSTYLE:FontSize ,StyleZ}=ListFEQ{PROP:FontSize}
        ListFEQ{PROPSTYLE:FontStyle,StyleZ}=ListFEQ{PROP:FontStyle}
        ListFEQ{PROPSTYLE:TextSelected,StyleZ}=Color:None
        ListFEQ{PROPSTYLE:BackSelected,StyleZ}=Color:None                       
     END
     IF ListFEQ{PROPLIST:CellStyle, ColX} THEN   !Y Cell Style
        CLEAR(ColQ)
        ColQ:ModFld='Y' ; ColMods[4]='Y'
        ColQ:IsLong=1
        ColQ:DataLong=255    ; DO AddColumnQ
        ListFEQ{PROPSTYLE:TextColor,255}=COLOR_Y_FG
        ListFEQ{PROPSTYLE:BackColor,255}=COLOR_Y_BG
     END
     IF ListFEQ{PROPLIST:Tip, ColX} THEN         !P Tip
        CLEAR(ColQ)
        ColQ:ModFld='P'
        ColQ:DataText='Tip for Column ' & ColX ; DO AddColumnQ
     END 
     IF ColMods THEN 
        GET(ColQ,ColPointer) 
        ColQ:ModsCol = ColMods
        PUT(ColQ) 
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
    Date5[2]=Date( 2, 2,Yr)
    Date5[3]=Date( 6,23,Yr)
    Date5[4]=Date(11, 4,Yr)
    Date5[5]=Date(12,25,Yr)
    Time5[1]=CLOCK()  !DEFORMAT('07:01:11',@t4)
    Time5[2]=DEFORMAT('09:02:12',@t4)
    Time5[3]=DEFORMAT('10:33:23',@t4)
    Time5[4]=DEFORMAT('11:04:34',@t4)
    Time5[5]=DEFORMAT('22:55:45',@t4)

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
       IF RowMod5=1 THEN Date5[1]+=32.
    END
    SORT(SELF.DataQ,SELF.DataQ.RowNo,SELF.DataQ.ColNo)
    RETURN
CellRtn ROUTINE
    DataQ.RowNo    = RowX
    DataQ.ColNo    = ColQ:FldNo   !not ColQ:ColNo
    DataQ.PicType  = ColQ:PicType
    DataQ.ModFld   = ColQ:ModFld
    DataQ.IsLong   = ColQ:IsLong
    DataQ.DataLong = ColQ:DataLong
    DataQ.DataText = ColQ:DataText
    CASE ColQ:ModFld
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
CbVlbPreviewClass.Sample_AtN PROCEDURE(STRING Picture, <*STRING OutSign>, BOOL Negate=0)!,STRING
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
    IF Negate AND NumQ:Sign THEN RETURN '-' & NumQ:Sample.
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
!----------------------------------------------------
CbVlbPreviewClass.TakeEvent PROCEDURE() !Register Event Calls
X        LONG,AUTO
mdRow    LONG,AUTO
mdColumn LONG,AUTO
QFieldNo LONG,AUTO
ColQ     &CbVlbColumnQueueType
DataQ    &CbVlbDataQueueType
DataGrp  GROUP(CbVlbDataQueueType),PRE(DataG). !VLB may change DataQ so keep Group
PopNo    SHORT
ClrMods  PSTRING(4)
ClrIcon  BYTE
DBugPop  PSTRING(32)
    CODE
    CASE EVENT()
    OF EVENT:NewSelection !OROF EVENT:Expanded OROF EVENT:Contracted
       CASE KEYCODE()
       OF MouseRight ; PopNo = 1
       OF MouseLeft2 ; PopNo = 2
       END
    END
    IF ~PopNo THEN RETURN 0.
    SETKEYCODE(0)
    ColQ &= SELF.ColumnQ ; DataQ &= SELF.DataQ
    mdRow   =SELF.FEQ{PROPLIST:MouseDownRow}
    mdColumn=SELF.FEQ{PROPLIST:MouseDownField}
    QFieldNo=SELF.FEQ{PROPLIST:FieldNo,mdColumn}
    ColQ.ColNo = mdColumn
    GET(ColQ,ColQ.ColNo)
    IF ERRORCODE() THEN Message('Failed GET ColNo ' & mdColumn).
    DataQ.RowNo = mdRow
    DataQ.ColNo = QFieldNo
    GET(DataQ,DataQ.RowNo,DataQ.ColNo)
    IF ERRORCODE() THEN Message('Failed GET DataQ ' & mdRow&','& QFieldNo) ; PopNo=0.
!The VLB may get "Events" and change DataQ while Edit Window is Open so save in Local Group
    DataGrp=DataQ
    EXECUTE PopNo
      DO PopupRtn
      DO EditRtn
    END
    RETURN 0 !Level:B9
PopupRtn ROUTINE
 COMPILE('!**DBugQ END**', DbugQs)
    DBugPop='|-|Debug{{Column Q|Data Q}' 
 !**DBugQ END**
    PopNo=POPUP('Remove Colors' & |
                '{{' & |
                   '* Cell Colors' & |   !#1
                   '|Y Cell Style' & |   !#2
                   '|Z Column Style' & | !#3
                   '|-' & |
                   '|All Colors * Y Z' & |      !#4
                '}' & |
              '|-|Remove All Icons' & |  !#5
                '|Check Box Icons'  & |  !#6
              '|-|Edit Data<9>Click 2' & DBugPop)  !#7 
    CASE PopNo
    OF  1 ; ClrMods='*'                   ! # 1  * Cell Colors
    OF  2 ; SELF.ClearYZ(255,255)         ! # 2  Y Cell Style
    OF  3 ; SELF.ClearYZ(1,254)           ! # 3  Z Column Style
    OF  4 ; SELF.ClearYZ(1,255) ; ClrMods='*'  ! # 4  All Colors * Y Z
    OF  5 ; ClrMods='IJ'                  ! # 5  Remove Icons
    OF  6 ; ClrMods='IJ' ; ClrIcon=1      ! # 6  Icon
    OF  7 ; DO EditRtn ; EXIT             ! # 7  Edit Data
 COMPILE('!**DBugQ END**', DbugQs)
    OF  8 ; DBugPrv.QueueReflection(ColQ,'ColumnQ')
    OF  9 ; DBugPrv.QueueReflection(DataQ,'DataQ')
 !**DBugQ END**
    ELSE  ; EXIT
    END
    IF ClrMods THEN DO ClrRtn.
    SELF.Changed=1
    EXIT
ClrRtn ROUTINE
    LOOP X=1 TO RECORDS(DataQ)
        GET(DataQ,X)
        IF ~INSTRING(DataQ.ModFld,ClrMods,1) THEN CYCLE.
        CASE DataQ.ModFld
        OF '*'   ; DataQ.DataLong=COLOR:None
        OF 'I'
        OROF 'J' ; DataQ.DataLong=ClrIcon ; IF ClrIcon THEN ClrIcon=3-ClrIcon.
        END
        PUT(DataQ)
    END
EditRtn ROUTINE
    DATA
Txt STRING(255)
UprTxt &BYTE
EditWn WINDOW('Edit'),AT(,,250,70),GRAY,SYSTEM,FONT('Segoe UI',10),RESIZE
        STRING('?'),AT(7,4,209),USE(?Pmt)
        BUTTON('&Data...'),AT(217,2,28,11),USE(?PickBtn),SKIP,FONT(,9),HIDE,TIP('Sample Data Popup')
        ENTRY(@s255),AT(8,16,,14),FULL,USE(Txt)
        SPIN(@d2),AT(8,16,226,14),USE(Txt,, ?DateTxt),HIDE,HVSCROLL
        CHECK('&UPR'),AT(7,32),USE(?UprTxt),SKIP,DISABLE,FONT(,9),TIP('Upper Case')
        STRING(''),AT(57,32),USE(?Bytes),FONT(,9)
        BUTTON('&OK'),AT(7,47,40,14),USE(?OKBtn),DEFAULT
        BUTTON('Cancel'),AT(57,47,40,14),USE(?CanBtn),STD(STD:Close)
        OPTION('Alter'),AT(113,31,123,32),USE(G:Alter),BOXED
            RADIO('&All Rows'),AT(119,39),USE(?Alter:1)
            RADIO('&Cell Only'),AT(119,49),USE(?Alter:2)
            RADIO('Rows Abov&e'),AT(169,39),USE(?Alter:3)
            RADIO('Rows Belo&w'),AT(169,49),USE(?Alter:R4)
        END
    END
CPicture CSTRING(25)
IsOk BYTE
aRow1 LONG
aRow2 LONG
ColHead STRING(30),AUTO
W LONG,DIM(4),AUTO
L LONG,DIM(4),AUTO
    CODE
    Txt=CHOOSE(~DataG:IsLong,DataG:DataText,''&DataG:DataLong)
    ColHead=SELF.FEQ{PROPLIST:Header,mdColumn}
    GETPOSITION(0,W[1],W[2]) ; GETPOSITION(SELF.FEQ,L[1],L[2],L[3],L[4]) ; L[1]+=W[1] ; L[2]+=W[2]
    OPEN(EditWn)
    GETPOSITION(0,W[1],W[2],W[3],W[4])
    W[1]=L[1]+(L[3]-W[3])/2    ; IF W[1]<L[1] THEN W[1]=L[1].
    W[2]=L[2]+(L[4]-W[4])/2-24 ; IF W[2]<L[2] THEN W[2]=L[2].
    SETPOSITION(0,W[1],W[2])
    0{PROP:MinWidth}=250 ; 0{PROP:MinHeight}=70 ; 0{PROP:MaxHeight}=70
    0{PROP:Text}='Edit Data Row: ' & mdRow &', Column: ' & mdColumn &', Field: '& QFieldNo & |
                 ' - ' & ColHead
    ?Pmt{PROP:Text}='Column ' & mdColumn & ' - Picture ' & CLIP(ColQ.Picture) &' - '& ColHead
    CPicture=CLIP(ColQ.Picture)
    CASE DataG:PicType 
    OF 'd' ; ?DateTxt{PROP:Text}=CPicture ; UNHIDE(?DateTxt) ; HIDE(?Txt) ; SELECT(?DateTxt)
    OF 'p' ; X=ChrCount(CPicture,'<#') ; IF X=0 THEN X=1. ; CPicture='@s' & X
    OF 's' ; UNHIDE(?PickBtn) ; ENABLE(?UprTxt) 
             UprTxt &= ColQ.UprTxt ; ?UprTxt{PROP:Use}=ColQ.UprTxt
             IF UprTxt THEN POST(EVENT:Accepted,?UprTxt).
             ?Txt{PROP:Imm}=1 ; POST(EVENT:NewSelection) !Bytes typing
    END
    ?Txt{PROP:Text}=CPicture
    ACCEPT
        IF EVENT()=EVENT:Rejected THEN
           DISPLAY ; SELECT(?) ; CYCLE
        END
        CASE ACCEPTED()
        OF ?OkBtn   ; IsOk=1 ; BREAK
        OF ?UprTxt  ; ?Txt{PROP:Upr}=UprTxt ; IF UprTxt THEN Txt=UPPER(Txt) ; DISPLAY. ; PUT(ColQ)
        OF ?PickBtn ; SELF.PopupSample(?Txt,CPicture,UprTxt) ; SELECT(?Txt)
        END
        IF DataG:PicType='s' THEN
           CASE EVENT()
           OF EVENT:Accepted     ; ?Bytes{PROP:Text}=LEN(CLIP(Txt))&' bytes'
           OF EVENT:NewSelection ; ?Bytes{PROP:Text}=LEN(CLIP(?Txt{PROP:ScreenText}))&' bytes'
           END
        END
    END
    CLOSE(EditWn)
    IF ~IsOk THEN EXIT.
    aRow1=1 ; aRow2=SELF.RowCnt
    CASE G:Alter
    OF 2 ; aRow1=mdRow ; aRow2=mdRow
    OF 3 ; aRow2=mdRow
    OF 4 ; aRow1=mdRow
    END
    LOOP X=1 TO RECORDS(DataQ)
         GET(DataQ,X)
         IF DataQ.ColNo <> QFieldNo THEN CYCLE.
         IF ~INRANGE(DataQ.RowNo,aRow1,aRow2) THEN CYCLE.
         IF DataQ.IsLong THEN
            DataQ.DataLong=Txt
         ELSE
            DataQ.DataText=Txt
         END
         PUT(DataQ)
    END
    SELF.Changed=1 
    DISPLAY
    EXIT
!------------------------
CbVlbPreviewClass.ClearYZ PROCEDURE(USHORT Style1, USHORT Style2)
X USHORT,AUTO
    CODE
    LOOP X=Style1 TO Style2
        SELF.FEQ{PROPSTYLE:TextColor,X}=COLOR:None
        SELF.FEQ{PROPSTYLE:BackColor,X}=COLOR:None
    END
    RETURN
!------------------------
CbVlbPreviewClass.PopupSample PROCEDURE(LONG TxtFEQ,STRING Picture,BYTE IsUpr)
AlphaBeta EQUATE('Alpha Beta Charlie Delta Echo Foxtrot Golf Hotel India Juliet Kilo Lima Mike November ')
OscarPapa EQUATE('Oscar Papa Quebec Romeo Sierra Tango Uniform Victor Whiskey Xray Yankee Zulu ')
LoremIp   EQUATE('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua ut ad. ')
PopMenu ANY
P PSTRING(40) ,DIM(18)
D PSTRING(256),DIM(18)
N BYTE
X BYTE,AUTO
B LONG,DIM(4),AUTO
    CODE !If you have any ideas on some sample data please let me know
    N+=1 ; P[N]='Bartholomew'   !Christopher also 11 bytes but no W or M
    N+=1 ; P[N]='Rostenkowsky'
    N+=1 ; P[N]='Bartholomew W. Rostenkowsky'
    N+=1 ; P[N]='Environmental Protection Agency'
    N+=1 ; P[N]='-|1200 Pennsylvania Avenue, N.W.'
    N+=1 ; P[N]='Manchester by the Sea'         !Carpentersville
    N+=1 ; P[N]='Manchester by the Sea, MA 01944'
    N+=1 ; P[N]='Massachussetts'
    N+=1 ; P[N]='-|Nnnnn Nnnnn ...'     ; D[N]=ALL('Nnnnn ',255)
    N+=1 ; P[N]='12345 67890 ...'       ; D[N]=ALL('12345 67890 ',255)
    N+=1 ; P[N]='1234567890...'         ; D[N]=ALL('1234567890',255)
    N+=1 ; P[N]='Alpha Beta Charlie ...'; D[N]=ALL(AlphaBeta,255)
    N+=1 ; P[N]='Oscar Papa Quebec ...' ; D[N]=ALL(OscarPapa,255)
    N+=1 ; P[N]='Lorem Ipsum ...'       ; D[N]=LoremIp&LoremIp
    N+=1 ; P[N]=sAtoZ                   ; D[N]=ALL(sAtoZ,255)
    N+=1 ; P[N]=QikBrownFox             ; D[N]=ALL(QikBrownFox,255)
    N+=1 ; P[N]=QikBrownCap             ; D[N]=ALL(QikBrownCap,255)
    LOOP X=1 TO N
        PopMenu=CHOOSE(X=1,'',PopMenu&'|') & P[X]
        IF ~D[X] THEN
            D[X]=P[X]
            IF SUB(D[X],1,2)='-|' THEN D[X]=SUB(D[X],3,999).
        END
    END
    GETPOSITION(TxtFEQ,B[1],B[2],,B[4])
    X=POPUP(PopMenu,B[1],B[2]+B[4],1)
    IF X THEN
       IF IsUpr THEN D[X]=UPPER(D[X]).
       CHANGE(TxtFEQ,D[X])
    END
    RETURN

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
        OF 'n' ; CHANGE(FEQ,VlbCls.Sample_AtN(Picture,,1))
        OF 'p' ; CHANGE(FEQ,VlbCls.Sample_AtP(Picture))
        OF 'k' ; CHANGE(FEQ,VlbCls.Sample_AtK(Picture)) ! ; FEQ{PROP:Tip}=Picture &'<13,10>'& CONTENTS(FEQ)
        OF 's' ; IF FEQ{PROP:Upr} THEN
                    CHANGE(FEQ,UPPER(QikBrownFox))
                 ELSIF FEQ{PROP:Cap} THEN 
                    CHANGE(FEQ,QikBrownCap)
                 END 
        END
    END
    DISPLAY
    RETURN
