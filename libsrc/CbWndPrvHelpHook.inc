!============================================================================================ 
! HlpEng Originally from DevCon CIDC 2017 by Carl Barnes, Barrington, Illinois www.CarlBarnes.com  carl@carlbarnes.com
! This uses that method to make the CBWndPreview class available in all windows by just adding code in frame
!-------------------------------------------------------------------------------------------
! Disclaimer
! Version 0.1 so YMMV
! The HelpEngine documentation is limited to:
!       LibSrc CWHELP.INT file 
!       Property.CLW new line PROP:HelpEngine         EQUATE (7D40H) ! array[integer]
!       MSDN for HtmlHelp() and WinHelp() are the target of HelpEngine.HelpCmd
! Based on the above I created the DevCon Help Engine examples.
! They work for me. What is here may not be correct, use at your own risk
!--------------------------------------------------------------------------------------------
! CbWndPrvHelpHookClass allows Ctrl+Shift F1 to open a CBWndPreview Class Window
! showing CB Window Preview list of all the controls in the current Window
! 
! Implement this code and do NOT have another "HELP('file.xxx')" coded because it will disable this
! It works by making the Help('YourFile.ChmCHEK'). This Engine Matches on 'xxxCHEK'
! After OutputDebugString HelpCmd() parms the CHM topic is opened normally using the Chm Engine.
! This can work with any help engine e,g, HLP or any other you add. It "hooks" them.
!
! This does NOT require a real CHM file to work, but you must supply a name in Step 2
! In my testing this DOES NOT seem to require a window to have an HLP('xxx'), surprising. Wizard normally adds HLP so it is frequent.

    OMIT('##### ** Add to this Class to an APP ** #####')

! 1. Global Embeds - Global Includes or Data
       INCLUDE('CbWndPrvHelpHook.INC'),ONCE             !Include the CB Window Preview Help Hook Class
       Help2WndPreviewCls    &CbWndPrvHelpHookClass     !Declare the object globally, make it a &REF              
              
! 2. Frame Accepted Embed for a Secret Menu Item to Enable the Hook
      MENU('File'),USE(?File)
        ITEM('Enable F1 Hook to show CB Window Preview '),USE(?CbHlpHookEnableItem) ,HIDE
      END

    OPEN(Window)
    IF SomethingSecret THEN UNHIDE(?CbHlpHookEnableItem).  !Show selected users the secret menu item
          
    CASE ACCEPTED()
    OF ?CbHlpHookEnableItem
    IF Help2WndPreviewCls &= NULL THEN   !<><><><><><><> CB Window Preview Hook <><><><><><>
       Help2WndPreviewCls &= NEW(CbWndPrvHelpHookClass) 
       IF ~Help2WndPreviewCls.IsInited THEN
          RV# = Help2WndPreviewCls.Init('YourHelp.CHM')  !Help file does NOT need to exist, but need a Name
          IF RV# THEN 
             Message('Help2WndPreviewCls failed reason ' & RV# )
             DISABLE(?) ; EXIT
          END
       END
    END    
    Message('Press Ctrl+Shift+F1 on Open Windows to see CB Window Preview Reflection List.' & |
            '||Note: Any windows OPEN now MUST be CLOSED and REOPENED to work.')

!       End of     OMIT('##### ** Add to this Class to an APP ** #####') 

!
!--------------------------------------------------------------------------------------------
!============================================================================================

 OMIT('_EndOfInclude_',_CbWndPrvHelpHookChek_)
_CbWndPrvHelpHookChek_ EQUATE(1)                
    INCLUDE('CWHELP.INT'),ONCE
CbWndPrvHelpHookClass CLASS,TYPE,IMPLEMENTS(HelpEngine),MODULE('CbWndPrvHelpHook.clw'), LINK('CbWndPrvHelpHook.clw',1),DLL(0)  !Ment to always be linked in

IsInited        BYTE            !Init worked and not Killed 

ChmFileName     CSTRING(261)    !,PROTECTED  !File.CHM but really any File.HLP or File.XXX of your own
ChmFileEXT      CSTRING(16)     !,PROTECTED  !Real EXT probably CHM
ChmEngine       &HelpEngine     !,PROTECTED        
ChmEngIndex     LONG            !,PROTECTED   

ChekFileName    CSTRING(261)    !,PROTECTED  !MyHelp.ChmCHEK     
ChekFileEXT     CSTRING(16)     !,PROTECTED  !       CHMCHEK always UPPER
ChekEngIndex    LONG            !,PROTECTED

! Init Parameters
!      Returns:  0=Worked Out in Temp; Failed 1=Not Network  <0 is Error  -2=File not found  -3=TempPathNotFound
Init            PROCEDURE(STRING HelpFile, BOOL ShowDbgMsg=0),LONG,PROC,VIRTUAL !0=Ok else Problem
Kill            PROCEDURE()     !If IsInited sets Help(ChmFilename) and removes Index

!Called by HelpEngine Interface to do the Real Work so can inherit, and are virtual
Match           PROCEDURE(CONST *CSTRING _HelpFileEXT),BOOL,VIRTUAL,PROTECTED  !Do we match .ChekFileEXT ?
HelpCmd         PROCEDURE(UNSIGNED _HWnd, <CONST *CSTRING _HelpFile>, UNSIGNED _HCmd, LONG _Data=0),BOOL,PROC,VIRTUAL,PROTECTED

              END

_EndOfInclude_

