  SECTION('Includes')
  !Put variable and equate declarations here
  INCLUDE('CBWndPreview.INC'),ONCE
  INCLUDE('CbVlbPreview.INC'),ONCE  !04/04/2021 Class for LIST Sample data and @n @p @k
! FixNum      BYTE               !04/04/21 n/a with new EntryInit()  !Must wait to fix number until InAccept. There is no SECTION for BeforeAccept or AfterData
WndPrvCls   CBWndPreviewClass
ListPreview CbVlbAllPreviewClass !FYI: If Shift is Down the Init does not happen
FixListPrv  BYTE                 !Must wait until InAccept. There is no SECTION for BeforeAccept or AfterData
!---------------------------------
  SECTION('AfterOpen')
  !Put code that needs to be executed after the window is opened, but before entering the ACCEPT loop
  WndPrvCls.Init(2)             !2=Secret Button Across Top; 1=Upper Left Corner 8x8 (default)
!Optional--> WndPrvCls.AllOffHDRS(1,0,0,0) !All controls 1=Turn off (Hide,Disable,Readonly,Skip) - Not done if Ctrl key down
  0{PROP:System} = 1            !Carl likes a SYSTEM Menu to close window because below set: windowPreviewerCanExit__=False
 ! LOOP K#=1 TO 255 ; 0{PROP:Alrt,K#}='' ; END !So ESC can close and use Mouse, ALRTs cannot work in a Preview?
  LOOP K#=255 TO 1 BY -1 ; 0{PROP:Alrt,K#}=0 ; END  !Let ESC close window and use Mouse if Window ALRTs. ALRTs do NOT work in a Preview.  
 !TODO hash out clear ALL Keys ... or call Class Method
!---------------------------------  
  SECTION('InAccept')
  IF ~FixListPrv THEN           !04/04/2021 Setup VLB to supply Preview Data
      FixListPrv=1 
      ListPreview.Init()        !Do after Generated preview sets up LIST.
      ListPreview.EntryInit()   !Setup ENTRY with 1,234.12.
  END 
 !Optional-->  IF ~FixNum THEN FixNum=1 ; WndPrvCls.AllFixNumbers() ; END !Fix Previewer ####.## numbers to 9999.99
  windowPreviewerCanExit__ = FALSE  !Carl likes button click NOT closing window to test Alt+Keys. Can close with SYSTEM Red X (turned on above) or Escape (if not alrt). 
!Put code here that is executed at the top of the ACCEPT loop
!if you set windowPreviewerCanExit__ to FALSE, the generated code will not
!check to see if a button was pressed and exit the preview
!Note that windowPreviewerCanExit__ is reset to TRUE every iteration of the ACCEPT loop
