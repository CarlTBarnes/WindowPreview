!An example of using CBWndPreview.ClassReflection()
!
!Released under MIT License Copyright (c) 2021 Carl Barnes - This is an Example not for production
!Much of this code is from PrivatePropertyTest.clw from GitHub JSlarve MIT License Copyright (c) 2019 Jeff Slarve
!The Test Example code was just what I needed to test my code. Thanks Jeff!
!   Also Demos - Click List Headers Sort using BrwExt SortHeaderClassType
!   Also Demos - Auto Size Columns by Double Click Resize using BrwExt AutoSizeColumnClassType
!
!Click "Select" and shows the JSPrivate Data Queue enumerating the Class Data
!Click "Class Reflection" button to view CBWndPreview ClassReflection() 
!  In that List Right-Click on Queues and Pick "Reference Reflection" e.g. Resize ControlQueue
!
    OMIT('** Example **') 
!Usage: When I want to see the values in a Class Object I can code:    
ViewResizeClassData ROUTINE
   DATA
PrvCls     CBWndPreview
   CODE
   PrvCls.ClassReflection(ResizeCls)
   
    !end of OMIT('** Example **')

  PROGRAM
  INCLUDE('CBWndPreview.inc'),ONCE      !From https://github.com/CarlTBarnes/WindowPreview
  INCLUDE('JSPrivateClass.inc'),ONCE    !From https://github.com/jslarve/PrivateClass
  INCLUDE('ABPOPUP.INC'),ONCE  !PopupClass has Properties (some private) and Q PopupItems / INIMgr cls 
  INCLUDE('ABUtil.inc'),ONCE   !INIClass only used for Reflection
  INCLUDE('ABRESIZE.inc'),ONCE !WindowResizeClass has ControlQueue and GROUPs
  INCLUDE('RTFCtl.inc'),ONCE   !RTFControlClass only used for Reflection
  INCLUDE('BrwExt.inc'),ONCE   !SortHeaderClassType AutoSizeColumnClassType 

  MAP
CBReflectionTest   PROCEDURE()  
  END 
  CODE
  CBReflectionTest()  
!------------------------------------------------------  
CBReflectionTest   PROCEDURE()
  
WndPrvCls   CBWndPreviewClass
JSP         JSPrivateClass
IniManager  INIClass
LocalPopup  CLASS(PopupClass)  !As *GROUP Error "No matching prototype available"
            end 
LocalPopupRef &LocalPopup      !See Carl Note - FYI A Diff Instance than MyPopup
MyPopup     LocalPopup         !See Jeff Note - FYI A Diff Instance than LocalPopup
  
RTFctrl         RTFControlClass 
ResizeCls       WindowResizeClass

BrwExt_SortHdr  SortHeaderClassType       !Click on LIST Headers to Sort
BrwExt_ColSize  AutoSizeColumnClassType   !Double Click Resize Zone for Auto Size - Nice!
BrwExt_LFM      ListFormatManagerClass    !Not used, just for Reflection

QueueRef_PopupItems  &QUEUE               !For JSP
PopupFailQueue        QUEUE
ControlID                SIGNED 
                      END
QueueRef_Rsz_ControlQueue  &QUEUE
QueueRef_Rsz_ResizeList    &QUEUE     

PetsQueue  QUEUE                            !12/28/21 Capesoft Reflection Example Queue with Extended NAME() ... modified
id            Long,name('Id | attribute')   !         https://clarionhub.com/t/a-proposed-convention-for-the-extended-use-of-the-name-attribute/2792
Name          Group,name('Name')
WholeName       String(20),name('WholeName')
NickName        String(20),name('Nickname | attribute')
              End
DateOfBirth   Long,name('Birthday | @d6 | date | Optional ')                 
            End
!PetsQueue      QUEUE(PetsQueueType).

PozGroup    GROUP(PositionGroup),PRE(Poz1) .    !Group to test 
TestGroup   GROUP(ControlQueue) .               !Q as Group to test .GroupReflection
EnumItemsQ  QUEUE(JSPC_MemberEnumQ),PRE(EnumQ)  !To copy JSP.EnumQ
            END
Window WINDOW('Class Reflection Test CBWndPreviewClass'),AT(,,382,204),CENTER,GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        STRING('Test CBWndPreviewClass.ClassReflection(*GROUP ClassObject, NAME ClassName)'),AT(42,1),USE(?Heading), |
                FONT('Consolas',10)
        BUTTON('Select Class'),AT(3,15,57,13),USE(?ClassSelectBtn),TIP('Select Class to View in List below using JS Priv' & |
                'ate Data,<13,10>then push Class Reflection button')
        BUTTON('View .ClassReflection() of CBWndPrv'),AT(71,15,,13),USE(?ClassReflectBtn),TIP('Open CBWndPreview.ClassReflection')
        BUTTON('Queue Reflection'),AT(233,15,,13),USE(?QueueBtn),TIP('View CBWndPreviewClass.QueueReflection(*QUEUE Q,STRING NameOfQueue [,BOOL ShowsRecords] )')
        BUTTON('Group Reflection'),AT(306,15,,13),USE(?GroupBtn),TIP('View CBWndPreviewClass.GroupReflection(*GROUP G,STRING NameOfGroup)<13,10>Shows GROUP(ControlQueue) with child group  LIKE(PositionGroup)')
        PROMPT('JSPrivateClass Enumeration of the Class Properties ...'),AT(5,31),USE(?EnumPrompt),TRN
        LIST,AT(5,42,372,156),USE(?EnumItemsList),from(EnumItemsQ),VSCROLL,FORMAT('138L(2)|M~JSPrivateClass   -   Label ~L(2)@s60@54L(2)|M~Da' & |
                'ta Type~C(0)@s60@38R(2)|M~Size~C(0)@n20@48R(2)|M~Address~C(0)@n20@25C(2)|M~NULL~C(0)@n1@20C(2)|M~Refere' & |
                'nce~C(0)@n1@'),TIP('Click Headers to Sort - BrwExt SortHeaderClassType<13,10>Double-Click Resize to Auto Fit - BrwExt AutoSizeColumnClassType')
    END
DOO     CLASS
ClassSelect PROCEDURE()
ViewClass   PROCEDURE(*GROUP pClass, STRING pName) 
ViewQueue   PROCEDURE()
        END 
EnumPrompt      PSTRING(64) 
ViewClassRef    &GROUP   
ViewClassName   STRING(64)

  CODE
  SYSTEM{7A58h}=1  !PROP:PropVScroll
  OPEN(Window)
  EnumPrompt=?EnumPrompt{PROP:Text}
  ?EnumPrompt{PROP:FontColor}=8000001Ah  ! COLOR:HotLight
  WndPrvCls.Init()  ; WndPrvCls.ReflectionBtn{PROP:Text}='?'
  IniManager.Init('.\ClsRefTest.ini')
  ResizeCls.Init(,1)

  MyPopup.Init(IniManager)
  LocalPopup.Init(IniManager) 
  LOOP M#=1 TO 8 ; !Adding some random popup items
        MyPopup.AddItem('MyPopup Item ' & M#,FORMAT(M#+TODAY(),@D3)) 
        LocalPopup.AddItem('LocalPopup ' & FORMAT(M#+TODAY(),@D8)) 
  END 
  LocalPopupRef &= LocalPopup      !See Carl Note - FYI A Diff Instance than MyPopup
!Jeff NOTE:
!   IMPORTANT: In the following code, notice that it's using MyPopup, which is a declaration of LocalPopup.
!   A locally defined class (LocalPopup) cannot be passed as a *GROUP, but re-declaring the class as MyPopup DOES work.
!   Keep that in mind if your code won't compile.
!Carl NOTE: 
!   If passing (Class) to (*GROUP) gets Compiler error "No matching prototype available" 
!   Try declaring a &ClassRef reference and pass that e.g. LocalPopupRef &LocalPopup  in data 
!   Do NOT forget to ClassRef &= RealClass e.g. LocalPopupRef &= LocalPopup

  QueueRef_PopupItems       &= JSP.GetFieldAddress(MyPopup,'PopupItems') !Setting a local Queue Reference to the PRIVATE PopupItems Queue in the PopupClass
  QueueRef_Rsz_ControlQueue &= JSP.GetFieldAddress(ResizeCls,'ControlQueue')
  QueueRef_Rsz_ResizeList   &= JSP.GetFieldAddress(ResizeCls,'ResizeList')
  IF QueueRef_PopupItems &= NULL                     !Make sure we have a good reference before trying to use it.
    STOP('PopupItems Class member not found in MyPopup')
    QueueRef_PopupItems &= PopupFailQueue
  END
  GETPOSITION(?EnumItemsList,Poz1:XPos,Poz1:YPos,Poz1:Width,Poz1:Height) 
    TestGroup.Pos = PozGroup  ; TestGroup.ID=1234 ; TestGroup.Type=56 ; TestGroup.ID=7890
  WndPrvCls.InitList(?EnumItemsList,EnumItemsQ,'JSP.EnumQ')    !Not needed in 11.13505 
  !WndPrvCls.InitList(?EnumItemsList,JSP.EnumQ,'JSP.EnumQ')    !Not needed in 11.13505 
  !Instead Declared ! ?EnumItemsList{PROP:From} = JSP.EnumQ !Setting the FROM of the listbox to the EnumQ of the JSPrivateClass.
  DOO.ViewClass(ResizeCls,'WindowResize (has ControlQueue)')

  BrwExt_SortHdr.Init(EnumItemsQ,?EnumItemsList)  !  BrwExt_SortHdr.Init(JSP.EnumQ,?EnumItemsList) 
  BrwExt_SortHdr.NoCase=0             !if =1 class writes order like '+UPPER(Field)' that fails for SORT(Q,'+UPPER(Q.Field)')
  BrwExt_SortHdr.UsePictureForCase=0  !if =1 also SORT(Q,'+UPPER(Field)') for @s
  BrwExt_ColSize.Init()
  BrwExt_ColSize.AddListBox(?EnumItemsList,JSP.EnumQ)  

  ACCEPT 
      CASE EVENT()
      OF EVENT:Sized    ; POST(EVENT:DoResize,0,THREAD()) 
      OF EVENT:DoResize ; ResizeCls.Resize()
      END
      CASE FIELD()
      OF ?EnumItemsList   ; BrwExt_SortHdr.TakeEvents()      
                            BrwExt_ColSize.TakeEvents()      
      END
      CASE ACCEPTED()
      OF ?ClassSelectBtn  ; DOO.ClassSelect()
      OF ?ClassReflectBtn ; WndPrvCls.ClassReflection(ViewClassRef,ViewClassName) 
      OF ?QueueBtn        ; DOO.ViewQueue()
      OF ?GroupBtn        ; WndPrvCls.GroupReflection(TestGroup,'TestGroup ControlQueue')
      END
  END  
  CLOSE(Window)
!---------------------------
DOO.ClassSelect   PROCEDURE()
X LONG  
Y LONG  
H LONG
    CODE
    GETPOSITION(?ClassSelectBtn,X,Y,,H)
    EXECUTE 1+POPUP('~[31763(700)]Select Class to View|-' & |
              '|WindowResizeClass (has GROUPs and QUEUE)' & |     
              '|My POPUP Class (PopupItems Q, INIMgr Cls)' & |
              '|LocalPOPUP Class as &&LocalPopupREF' & |
              '|IniManager  INIClass' & |     
              '|CBWndPreviewClass (has GROUPs)' & |     
              '|SortHeaderClassType BrwExt_SortHdr (Qs ListQueue QueueListHeader QueueListSort)' & |  
              '|AutoSizeColumnClassType BrwExt_ASC (QueueAutoSizeListBox) ' & |
              '|ListFormatManagerClass ' & |
              '|RTFControl Class' & |
              '|LocalPopupRef &= LocalPopup' & |
              '',X,Y+H,1) 
      return !Click Off =0              
      return !Header Select...             
      DOO.ViewClass(ResizeCls,'WindowResize (has ControlQueue)')
      DOO.ViewClass(MyPopup,'ABPOPUP My Class (has PopupItems Q)')
      DOO.ViewClass(LocalPopupRef,'LocalPopupRef (has PopupItems Q)')
      DOO.ViewClass(IniManager,'INIClass IniManager ')
      DOO.ViewClass(WndPrvCls,'CBWndPreviewClass')
      DOO.ViewClass(BrwExt_SortHdr,'SortHeaderClassType (Qs ListQueue QueueListHeader QueueListSort)')
      DOO.ViewClass(BrwExt_ColSize,'AutoSizeColumnClassType (QueueAutoSizeListBox)')
      DOO.ViewClass(BrwExt_LFM,'ListFormatManagerClass')
      DOO.ViewClass(RTFctrl,'RTFControlClass')
      ELSE ; Message('POPUP did not find EXECUTE','Popup')
    END  
!---------------------------    
DOO.ViewClass   PROCEDURE(*GROUP pClass, STRING pName) 
QX LONG
    CODE 
    BrwExt_SortHdr.ClearSort()
    ?EnumPrompt{PROP:TExt}=pName & ' - ' & EnumPrompt  !JS Enumeration of the Class Properties ... '  
    JSP.EnumMembers(pClass)
    FREE(EnumItemsQ)
    LOOP QX=1 TO RECORDS(JSP.EnumQ)
        GET(JSP.EnumQ,QX)
        EnumItemsQ = JSP.EnumQ
        ADD(EnumItemsQ)
    END
    DISPLAY
    ViewClassRef &= pClass  
    ViewClassName = pName
    RETURN  
!---------------------------
DOO.ViewQueue   PROCEDURE() 
X LONG  
Y LONG  
H LONG  
    CODE
    GETPOSITION(?QueueBtn,X,Y,,H)
    EXECUTE POPUP('~[31763(700)]Select Queue to View|-' & |
              '|Resize ControlQueue (has GROUPs)' & |
              '|JS Class .ENumQ Class Queue' & |
              '|EnumItemsQ PRE(ENumQ) Local Queue' & |
              '|POPUP Class Queue' & |
              '|Resize ResizeList' & |
              '|PetsQueue with Extended NAME() Attributes' & |
              '',X,Y+H,1) 
      return
      WndPrvCls.QueueReflection(QueueRef_Rsz_ControlQueue,'Resize ControlQueue') 
      WndPrvCls.QueueReflection(JSP.EnumQ,'JSP.EnumQ')          
      WndPrvCls.QueueReflection(EnumItemsQ,'EnumItemsQ PRE(ENumQ)')          
      WndPrvCls.QueueReflection(QueueRef_PopupItems,'Popup Items Queue')          
      WndPrvCls.QueueReflection(QueueRef_Rsz_ResizeList  ,'Resize ResizeList')          
      WndPrvCls.QueueReflection(PetsQueue                ,'PetsQueue')          
    END 
    