!------------------------------------------------------------
! CBVlbPreviewClass (c) Carl Barnes 2021 - MIT License
! Download: https://github.com/CarlTBarnes/WindowPreview
!-----------------------------------------------------------------------------
! For the Window Preview feed the LIST from VLB to replace generated Queue
! This does * Colors simply (Red on Pink), Styles are Green and Yellow
! "All Preview" class can set @n @p @k sample data to 123,456.12 with .EntryInit()
!-----------------------------------------------------------------------------
! Steps to Implement
!   1. Create in LibSrc WindowPreview.INC   (or Edit existing)
!   2. After  SECTION('Includes')   add line:  INCLUDE('CbVlbPreview.INC'),ONCE
!                                  plus line:  ListPreview  CbVlbAllPreviewClass
!   3. After  SECTION('AfterOpen')  add line:  ListPreview.Init()       !LIST Data Preview
!   4. After  SECTION('InAccept')   add line:  ListPreview.EntryInit()  !ENTRY with 1,234
!   FYI: If Shift is Down the Init does not happen, you get normal preview
!-----------------------------------------------------------------------------
    OMIT('_EndOfInclude_',_IFDef_CbVlbPreview_)
_IFDef_CbVlbPreview_  EQUATE(1)

CbVlbColumnQueueType QUEUE,TYPE !Queue Defines Columns for VLB based on Format()
ColNo     SHORT           !ColQ:ColNo     List Column
FldNo     SHORT           !ColQ:FldNo     Queue Field No
Mod       STRING(1)       !ColQ:Mod       Modifier or Blank for Data
Picture   STRING(24)      !ColQ:Picture OVER(Pic Group)
PicType   STRING(1)       !ColQ:PicType  n e s d t p P  Picture[2]
PicSign   STRING(1)       !ColQ:PicSign  -( if @n-9.2 or @n(9) allows negative
IsLong    BOOL            !ColQ:IsLong
DataLong  LONG            !ColQ:DataLong   Data LONG for VLB
DataText  STRING(60)      !ColQ:DataText   Data STRING for VLB
Format    STRING(96)      !ColQ:Format     Col Format for Debug
        END !ColQ

CbVlbDataQueueType QUEUE,TYPE    !Queue feeds VLB Data
RowNo     SHORT           !VlbQ:ROwNo
ColNo     SHORT           !VlbQ:ColNo  VLB asks for Col but its FieldNo
PicMod    STRING(2)       !VlbQ:PicMod     Debug Cell
IsLong    BOOL            !VlbQ:IsLong
DataLong  LONG            !VlbQ:DataLong   Data LONG for VLB
DataText  STRING(60)      !VlbQ:DataText   Data STRING for VLB
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

!Internal Methods
!---------- Internal
Construct   PROCEDURE()
Destruct    PROCEDURE()
LoadColumnQ PROCEDURE(),LONG
LoadDataQ   PROCEDURE()
VLBprc      PROCEDURE(LONG xRow, USHORT xCol),STRING
Sample_AtN  PROCEDURE(STRING sPicture, <*STRING OutSign>),STRING  !@n#.# sample
Sample_AtP  PROCEDURE(STRING sPicture),STRING                     !@p###p
Sample_AtK  PROCEDURE(STRING sPicture),STRING                     !@k###k
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

