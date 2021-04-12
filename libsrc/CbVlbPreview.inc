!------------------------------------------------------------
! CBVlbPreviewClass (c) Carl Barnes 2021 - MIT License
! Download: https://github.com/CarlTBarnes/WindowPreview
!-----------------------------------------------------------------------------
! For the Window Preview feed the LIST from VLB to replace generated Queue
! Sets * Colors simply (Red on Pink), Styles Green on Yellow, plus Trees, Icons
! Sets sample data for Numbers to max 123,456.12, @d variety, @t @p @s 
! "All Preview" class can set @n @p @k sample data to 123,456.12 with .EntryInit()
!-----------------------------------------------------------------------------
! Steps to Implement
!   1. Edit Accessory\LibSrc\Win\WindowPreview.INC (or take from my GitHub)
!   2. After  SECTION('Includes')       ------  add lines: --------
!                INCLUDE('CbVlbPreview.INC'),ONCE
!             ListPreview CbVlbAllPreviewClass
!             FixListPrv  BYTE
!   3. After  SECTION('InAccept')       ------  add lines: --------
!             IF ~FixListPrv THEN           !Setup Preview Data VLB
!                 FixListPrv=1
!                 ListPreview.Init()        !LIST Data Preview
!                 ListPreview.EntryInit()   !ENTRY with 1,234
!             END 
!   FYI: If Shift is Down then Init does not happen, you get normal preview
!-----------------------------------------------------------------------------
    OMIT('_EndOfInclude_',_IFDef_CbVlbPreview_)
_IFDef_CbVlbPreview_  EQUATE(1)

CbVlbColumnQueueType QUEUE,TYPE !Queue Defines Columns for VLB based on Format()
ColNo     SHORT           !ColQ:ColNo     List Column
FldNo     SHORT           !ColQ:FldNo     Queue Field No   [12345]
ModsCol   STRING(5)       !ColQ:ModsCol   Modifiers in Column  *IJTYP
ModFld    STRING(1)       !ColQ:ModFld    Modifier in Q Field or Blank for Data
Picture   STRING(24)      !ColQ:Picture OVER(Pic Group)
PicType   STRING(1)       !ColQ:PicType  n e s d t p P  Picture[2]
PicSign   STRING(1)       !ColQ:PicSign  -( if @n-9.2 or @n(9) allows negative
IsLong    BOOL            !ColQ:IsLong
DataLong  LONG            !ColQ:DataLong   Data LONG for VLB
DataText  STRING(60)      !ColQ:DataText   Data STRING for VLB 
UprTxt    BYTE            !ColQ:UprTxt
Format    STRING(128)     !ColQ:Format     Col Format shows on Data window
        END !ColQ

CbVlbDataQueueType QUEUE,TYPE    !Queue feeds VLB Data
RowNo     SHORT           !VlbQ:ROwNo
ColNo     SHORT           !VlbQ:ColNo     VLB asks for Col but its FieldNo
PicType   STRING(1)       !VlbQ:PicType   Picture n d t s p k if Data Column 
ModFld    STRING(1)       !VlbQ:ModFld    Modifier if Field *YIJTP
IsLong    BOOL            !VlbQ:IsLong
DataLong  LONG            !VlbQ:DataLong  Data LONG for VLB
DataText  STRING(255)     !VlbQ:DataText  Data STRING for VLB
        END
CbVlbMarkQueueType QUEUE,TYPE !Queue feeds MARK()
Mark BYTE
    END
!----------------- CbVlbPreviewClass -----------------
CbVlbPreviewClass   CLASS,TYPE,MODULE('CbVlbPreview.CLW'),LINK('CbVlbPreview.CLW')
Init        PROCEDURE(LONG ListFEQ, LONG RowCount=0)  !Does Everything

!---------- Internal Properties
FEQ         LONG    !List FEQ
Items       LONG
RowCnt      LONG
ClmCnt      USHORT
Changed     BOOL
ColumnQ     &CbVlbColumnQueueType
DataQ       &CbVlbDataQueueType
MarkQ       &CbVlbMarkQueueType

!Internal Methods
!---------- Internal
Construct   PROCEDURE()
Destruct    PROCEDURE()
LoadColumnQ PROCEDURE(),LONG
LoadDataQ   PROCEDURE()
VLBprc      PROCEDURE(LONG xRow, USHORT xCol),STRING
Sample_AtN  PROCEDURE(STRING sPicture, <*STRING OutSign>, BOOL Negate=0),STRING  !@n#.# sample
Sample_AtP  PROCEDURE(STRING sPicture),STRING                     !@p###p
Sample_AtK  PROCEDURE(STRING sPicture),STRING                     !@k###k
PopupSample PROCEDURE(LONG TxtFEQ,STRING Picture,BYTE IsUpr),VIRTUAL
TakeEvent   PROCEDURE(),BYTE,PRIVATE
ClearYZ     PROCEDURE(USHORT Style1, USHORT Style2)
                    END

!========================== CbVlbAllPreviewClass ==========================
CbVlbAllPreviewQueueType QUEUE,TYPE
ListFEQ   LONG
Fmt       STRING(255)        !For Debug
VlbCls    &CbVlbPreviewClass
                END

CbVlbAllPreviewClass   CLASS,TYPE,MODULE('CbVlbPreview.CLW'),LINK('CbVlbPreview.CLW')
Init        PROCEDURE()     !Setup All LIST with Sample Data and Modifiers using VLB
EntryInit   PROCEDURE()     !Setup ENTRY(@n)(@p)(@k) controls with 1,234.12 sample

!---------- Internal
Construct   PROCEDURE()
Destruct    PROCEDURE()
VlbClassQ   &CbVlbAllPreviewQueueType
EntryIsInit BYTE
ListIsInit  BYTE
          END

    _EndOfInclude_

