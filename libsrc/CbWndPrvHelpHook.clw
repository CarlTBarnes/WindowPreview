  MEMBER
HlpEngMaxIndex  EQUATE(16)          !Max SYSTEM{PROP:HelpEngine,Index},  Undocumented found by simple tests that {,17} fails
extCHEK         EQUATE('CHEK')      !EXT dded to Chm so ChmCHEK or HlpCHEK or any XxxxCHEK

  INCLUDE('CbWndPrvHelpHook.inc'),ONCE 
  INCLUDE('KEYCODES.CLW')
  INCLUDE('CbWndPreview.INC'),ONCE

HlpEngTlz   CLASS    !Help Engine Tools, a Helper Class for dealing with Help Engines
FindMatch     PROCEDURE(STRING HelpFileExten),LONG      !Search 16 slots for Match(EXT) and return Index
FindEngine    PROCEDURE(*HelpEngine Engine2Find),LONG   !Search 16 slots for = HelpEngine returns INDEX 
FindFreeIndex PROCEDURE(LONG StartIndex=3),LONG         !Search 16 slots and return an Empty Index 
HCmdName      PROCEDURE(LONG HCmd),STRING    
FileExtOnly   PROCEDURE(string InFN),string       !Find EXT x:\xxx\xxx\flename.EXT
            END

  MAP 
HexLong              PROCEDURE(LONG ALong),STRING,PRIVATE        !####h ### 
WndPreviewCaller     PROCEDURE(LONG _HWnd),PRIVATE  !Calls one of the below
WndPreviewCallerQue  PROCEDURE(LONG _HWnd),PRIVATE  !Mainstains a queue of CbPrv classes
WndPreviewCallerLite PROCEDURE(LONG _HWnd),PRIVATE  !A Lite version that does not persist
    MODULE('win32')
        OutDbg(*cstring dMsg),PASCAL,RAW,NAME('OutputDebugStringA'),dll(1)
     END 
  END 

 !============================================================== 
 !Note labels say "Chm" but it can be any kind of Help File, HLP, WWW, etc
CbWndPrvHelpHookClass.Init  PROCEDURE(STRING InChmFile, BOOL ShowDbgMsg=0)!,LONG,PROC,VIRTUAL 
cDbg        CSTRING(512)
    CODE
    SELF.ChmFileName = CLIP(InChmFile) 
    SELF.ChmFileEXT  = UPPER(HlpEngTlz.FileExtOnly(InChmFile))    !Might not be CHM, could be HLP or WWW or ANY    
    !IF ~EXISTS(InChmFile) THEN RETURN -2.              !This Engine can be used to hook any Engine which Web Help would not have a file on disk
    HELP(InChmFile)                                     !Be sure RTL loads CHM Engine (or whatever engine the file used e.g. HLP or WWW
    !SELF.ChmEngIndex = 1   !First thing could assume CHM will be #1, but better to Search the 16 for a Match(CHM)
    SELF.ChmEngIndex = HlpEngTlz.FindMatch(SELF.ChmFileEXT) 
    IF ~SELF.ChmEngIndex THEN  
        RETURN 1                                        !#1 problem - No Engine for CHM, or more accurately Help File EXT
    END
    SELF.ChmEngine &= SYSTEM{PROP:HelpEngine,SELF.ChmEngIndex}   !Save the Engine to open CHM Help in HelpCmd() for ChmCHEK
      
    !--Setup HELP(CHEK) as MyHelp.ChmCHEK
    SELF.ChekFileName = UPPER(SELF.ChmFileName & extCHEK)  !MyHelp .CHMCHEK
    SELF.ChekFileExt  = SELF.ChmFileEXT & extCHEK          !        CHMCHEK  
    SELF.ChekEngIndex = Hlpengtlz.FindEngine(SELF.HelpEngine)   !Was this Engine Previously Inited i.e. Indexed?
    IF ~SELF.ChekEngIndex THEN                                  !Nope, we need to find a free Index
        SELF.ChekEngIndex =Hlpengtlz.FindFreeIndex(5)           !Find a free slot (could assume 5 to 16 are free)
        IF ~SELF.ChekEngIndex THEN
           RETURN 2                                             !#2 problem - No Engine free, very unlikely
        ELSE
           SYSTEM{PROP:HelpEngine,SELF.ChekEngIndex} = ADDRESS(SELF.HelpEngine) 
        END
    END 
    HELP(SELF.ChekFileName)
    SELF.IsInited = True 
    
    cDbg ='CbWndPrvHelpHookClass.Init( ' & InChmFile & | 
            '<13,10>.ChmFileName='    & SELF.ChmFileName & |
            '<13,10>.ChmFileExt='     & SELF.ChmFileEXT & |
            '<13,10>.ChmEngine = '    & HexLong(ADDRESS( SELF.ChmEngine)) & |
            '<13,10>.ChmEngIndex = '  & SELF.ChmEngIndex & | 
            '<13,10>' & |
            '<13,10>.ChekFileName='   & SELF.ChekFileName & |
            '<13,10>.ChekFileExt='    & SELF.ChekFileExt & |  !            '<13,10>.ChekEngine = '   & HexLong(ADDRESS( SELF.ChekEngine)) & |
            '<13,10>.ChekEngIndex = ' & SELF.ChekEngIndex & |
            '<13,10>.HelpEngine = '   & HexLong(ADDRESS( SELF.HelpEngine)) & |
            '<13,10>'
    OutDbg(cDbg) 
    IF ShowDbgMsg THEN Message(cDbg,'CbWndPrvHelpHookClass.Init',,,,MSGMODE:CANCOPY ).
    RETURN 0    
!--------------------------------------------
CbWndPrvHelpHookClass.Kill            PROCEDURE()     !If IsInited sets Help(ChmFilename) and removes Index
    CODE
    IF SELF.IsInited THEN
       SELF.IsInited = 0
       HELP(SELF.ChmFileName)       !Set HELP() to normal file so it might work
       IF SELF.ChekEngIndex THEN
          SYSTEM{PROP:HelpEngine,SELF.ChekEngIndex} = 0
       END 
    END
    RETURN
!=================================================================================    
CbWndPrvHelpHookClass.HelpEngine.Match    PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL
    CODE
    RETURN SELF.MATCH(_HelpFileEXT) 
!------------------------------------    
CbWndPrvHelpHookClass.HelpEngine.HelpCmd  PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC
    CODE
    RETURN SELF.HelpCmd(_HWnd,_HelpFile,_HCmd,_Data)
!=================================================================================    
!Called by HelpEngine Interface to do the Real Work
CbWndPrvHelpHookClass.Match PROCEDURE(CONST *CSTRING _HelpFileEXT)!,BOOL,VIRTUAL,PROTECTED
    CODE
    IF UPPER(_HelpFileEXT)=SELF.ChekFileEXT THEN 
       RETURN True 
    END     
    RETURN False
!----------------------------------------------------------    
CbWndPrvHelpHookClass.HelpCmd PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0)!,BOOL,PROC,VIRTUAL,,PROTECTED
cChmFile        CSTRING(261)    !_HelpFile with CHEK cutoff of ChmChek
LnStr           LONG 
cDbg            CSTRING(512)
cData           &CSTRING
cBlank          CSTRING(2)   
RetBool         BOOL 
    CODE
    IF BAND(KeyState(),0700h)=0300h THEN    !If Ctrl+Shift are down open a Window that shows all the HLP on the Under Window
       !Message('HelpCmd=' & _HWnd &'|Target=' & SYSTEM{PROP:Target}) 
       WndPreviewCaller(_HWnd)
       return true
    END

    !#1 - Send HelpCmd() parms to DebugView so can see Topic.HTM, helps Author, Developer or Tester
    cData &= cBlank
    IF OMITTED(_HelpFile) THEN                      !No File passed, that's odd
       cDbg='ChekHelp ' & HlpEngTlz.HCmdName(_HCmd) &'  '& _Data  &'  ?.Chm?NoFilePassed?' 
       OutDbg(cDbg)
       RETURN SELF.ChmEngine.HelpCmd(_HWnd, , _HCmd, _Data)       
    END 
    LnStr=LEN(_HelpFile)                            !File passed  .ChmChek
    cChmFile=SUB(_HelpFile,1,LnStr - LEN(extCHEK))  !  Change to  .Chm     to pass to ChmEngine
    IF _HCmd=HCMD_SHOWTOPIC           |             !Cmd is F1 Showing HLP()
    AND (_Data > 0FFFFh OR _Data < 0) THEN          !0 to 64KB is invalid pointer
       cData &= (_Data)                             !defef pointer to CSTRING
    END
    cDbg='ChekHelp ' & HlpEngTlz.HCmdName(_HCmd) &' '& CHOOSE(~cData, ''&_Data, cData) &'  '&  cChmFile  !Choose(,Long,String) returns Long so ''& makes (,Strubg,String)
    OutDbg(cDbg)        

    !#2 - Open Help Viwer by calling the CHM Engine found in .Init() and pass adjusted file name + original parms
    RetBool = SELF.ChmEngine.HelpCmd(_HWnd, cChmFile , _HCmd, _Data)   
   
    !#3 - If Ctrl+Shift are down open a Window that shows all the HLP on the Under Window
!    IF BAND(KeyState(),0700h)=0300h THEN
!       HELP(cChmFile)
!       CtrlShiftShowHelp() 
!       HELP(SELF.ChekFileName)      
!    END 
    RETURN RetBool

!============================= Help Engine Tools ===============================
!Find an Engine that Match()s on EXTension and return its SYSTEM HlpEng Index  
HlpEngTlz.FindMatch   PROCEDURE(STRING HelpFileExten) !,LONG returns INDEX of Engine.Match(EXT)
cHelpEXT        CSTRING(16),AUTO
Engine2Test     &HelpEngine 
EngAddress      LONG,AUTO 
Index           LONG,AUTO 
IndexMatched    LONG
    CODE
    cHelpEXT = CLIP(UPPER(HelpFileExten))
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
         EngAddress = SYSTEM{PROP:HelpEngine,Index} 
         IF EngAddress = 0 THEN CYCLE.                  !Index assigned Engine? 
         Engine2Test &= SYSTEM{PROP:HelpEngine,Index} 
         IF Engine2Test.Match(cHelpEXT) THEN            !Call HelpEngine.Match() 
            IndexMatched = Index
            BREAK
         END
    END
    RETURN IndexMatched
!--------------------------------------------------    
!Find an Engine that is same Address as Engine2Find and return its  SYSTEM HlpEng Index, to see if Engine already indexed  
HlpEngTlz.FindEngine   PROCEDURE(*HelpEngine Engine2Find) !,LONG returns INDEX of 
Index           LONG,AUTO 
IndexMatched    LONG
    CODE
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
         IF SYSTEM{PROP:HelpEngine,Index} = ADDRESS(Engine2Find) THEN       !Index assigned Engine? 
            IndexMatched = Index
            BREAK
         END
    END
    RETURN IndexMatched
!--------------------------------------------------
!Find a SYSTEM HE Index that is zero so I can use it    
HlpEngTlz.FindFreeIndex   PROCEDURE(LONG StartIndex=3) !,LONG Search 16 slots for an Empty
EngAddress      LONG,AUTO 
Index           LONG,AUTO 
ReturnIndex     LONG
    CODE
    LOOP Index=1 TO HlpEngMaxIndex                      !Look thru 16 engines
         EngAddress = SYSTEM{PROP:HelpEngine,Index} 
         IF EngAddress <> 0 THEN CYCLE.                 !This Index is used
         ReturnIndex = Index
         IF Index >= StartIndex THEN BREAK.             !Leave 1 and 2 open for CHM and HLP (no real need I just like to)
    END
    RETURN ReturnIndex
!--------------------------------------------------    
HlpEngTlz.HCmdName    PROCEDURE(LONG HCmd) !,STRING
    CODE 
    RETURN CHOOSE(HCmd+1, 'HCMD_CLOSE','HCMD_SHOWTOPIC','HCMD_SHOWINDEX','HCMD_SHOWTOC','HCMD_SHOWSEARCH','HCMD_HELPONHELP','HCMD_' & HCmd)
!-------------------------------------------------------
HlpEngTlz.FileExtOnly     PROCEDURE(string InFN)!,string      !Find EXT x:\xxx\xxx\flename.EXT
X   LONG,AUTO
Prd LONG 
    CODE
    LOOP X=SIZE(InFN) TO 1 BY -1  
         IF InFN[X]='\' THEN BREAK. 
         IF InFN[X]='.' ; Prd=X ; BREAK. 
    END 
    RETURN CHOOSE(Prd=0,'',CLIP(SUB(InFN,Prd+1,20)))
!=================================================================================================
HexLong   PROCEDURE(LONG ALong)!,STRING
i       UNSIGNED,AUTO
A       UNSIGNED,AUTO
S       STRING(8),AUTO
DIGITS  STRING('0123456789ABCDEF')
  CODE
  A = ALong
  LOOP i=8 TO 1 BY -1 ; S[i] = DIGITS[BAND (A, 0Fh) + 1] ; A = BSHIFT (A, -4) ; END
  RETURN S &'h ' & ALong


!================================== Ctrl+Shift Help Window =====================================
!Rather than always make a new Class try to have a Queue to preserve the settings made and static
!Maybe it would be simpler to have THREAD on QUEUE, but then every START makes one
!To make this lighter weight change the QUEUE to a GROUP to just preserve the last one... done below

WndPreviewCaller PROCEDURE(LONG _HWnd)
    CODE
    WndPreviewCallerQue(_HWnd)
    !WndPreviewCallerLite(_HWnd)
    RETURN
WndPreviewCallerQue PROCEDURE(LONG _HWnd)
WndRefLng   LONG,AUTO
WndPrvQ     QUEUE,STATIC
ThrdNo          LONG
WndHnd          LONG
WndRef          LONG
WndPrvCls       &CBWndPreviewClass
            END
!ThisWndPrvCls   &CBWndPreviewClass
  CODE
  WndRefLng=SYSTEM{PROP:Target}
  IF ~_HWnd OR ~WndRefLng THEN
     Message('Unexpected _HWnd=' & _HWnd &' Wndref=' & WndRefLng) ; RETURN
  END
  !Message('HelpCmd=' & _HWnd &'|Target=' & SYSTEM{PROP:Target},'WndPreviewCaller') 
  CLEAR(WndPrvQ)
  WndPrvQ.ThrdNo = THREAD()
  WndPrvQ.WndHnd = _HWnd
  WndPrvQ.WndRef = WndRefLng
  GET(WndPrvQ,WndPrvQ.ThrdNo,WndPrvQ.WndHnd,WndPrvQ.WndRef)
  IF ERRORCODE() THEN
     WndPrvQ.WndPrvCls &= NEW(CBWndPreviewClass)
     WndPrvQ.WndPrvCls.Init(2)
     ADD(WndPrvQ,WndPrvQ.ThrdNo,WndPrvQ.WndHnd,WndPrvQ.WndRef)
  END
  WndPrvQ.WndPrvCls.Reflection()
  RETURN

WndPreviewCallerLite PROCEDURE(LONG _HWnd)  !A  Lite version that does not persist
WndRefLng   LONG,AUTO
WndPrvGrp   GROUP,STATIC
ThrdNo          LONG
WndHnd          LONG
WndRef          LONG
WndPrvCls       &CBWndPreviewClass
            END
ThisWndPrvCls   &CBWndPreviewClass
  CODE
  WndRefLng=SYSTEM{PROP:Target}
  IF ~_HWnd OR ~WndRefLng THEN
     Message('Unexpected _HWnd=' & _HWnd &' Wndref=' & WndRefLng) ; RETURN
  END
  !Message('HelpCmd=' & _HWnd &'|Target=' & SYSTEM{PROP:Target},'WndPreviewCaller') 
  CLEAR(WndPrvGrp)
  IF  WndPrvGrp.ThrdNo = THREAD() |
  AND WndPrvGrp.WndHnd = _HWnd    |
  AND WndPrvGrp.WndRef = WndRefLng |
  AND NOT WndPrvGrp.WndPrvCls &= NULL THEN
      !The object is good
  ELSE
     WndPrvGrp.WndPrvCls &= NEW(CBWndPreviewClass)
     WndPrvGrp.WndPrvCls.Init(2)
     WndPrvGrp.ThrdNo = THREAD()
     WndPrvGrp.WndHnd = _HWnd
     WndPrvGrp.WndRef = WndRefLng
  END
  WndPrvGrp.WndPrvCls.Reflection()
  RETURN 
 