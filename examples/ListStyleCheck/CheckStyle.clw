!A CheckBox in a List can be done using a Dingbat/Wingding symbol and a ColStyle or CellStyle. 
!Example by Carl Barnes for CIDC 2019
!---------------------------------
!This method allows NOT needing a Icon linked into the project so for class is lighter weight
!Using a Font for Checkbox may have advantages when scaling for different display DPIs
!Windows uses the Marlett font for some scalable icons: https://docs.microsoft.com/en-us/typography/font-list/marlett
!When Unicode comes there will be many many more Dingbat and Symbols to use
!Not limited to just CheckBox, can use for various icons in a LIST. e.g. Up/Down arrows to indicate Increase/Decrease.
!You can have "Z(#)" Column Style in the Format so the Queue does not need a Style code
!----
! One tip is to set the style selected colors so it does not invert e.g.
!    ?ListCB{PROPSTYLE:TextSelected,1} = COLOR:WindowText  !COLOR:Black
!    ?ListCB{PROPSTYLE:BackSelected,1} = COLOR:Window      !COLOR:White 

    PROGRAM
    INCLUDE 'KeyCodes.CLW'

!============================= CB Window Preview =========================
             !Set to (1) to Include on Window the Secret CB Window Preview Class button in Upper Left of Windows
Inc_CbWndPrv   EQUATE(1) !Set (0) if you do NOT have CbWndPreview.INC 
!    COMPILE('!-INC-CbWndPrv',Inc_CbWndPrv=1)
    OMIT('!-INC-CbWndPrv',Inc_CbWndPrv=0)
        INCLUDE('CbWndPreview.INC'),ONCE  !File was in CIDC 2019 Download. Put in ths folder or Accessory\LibSrc\Win
    !-INC-CbWndPrv

    OMIT('!-Dummy-CbWndPrv',Inc_CbWndPrv)
CBWndPreviewClass    CLASS,TYPE     !Dummy so below can have code without class
Init            PROCEDURE(BYTE SecretBtnType=1, LONG SecretKey=1879)
                     END
    !-Dummy-CbWndPrv
!============================= CB Window Preview =========================

    MAP
Menu                        PROCEDURE()    
SimpleString1               PROCEDURE()  !Minimal code 
SimpleString1BIG            PROCEDURE()  !Scale test,made BIG
FlexibleStringAndByte       PROCEDURE()  !Flexible can Changes Font, Colors, Characters, add Chars
Check2StyleQRedGreen_Basic  PROCEDURE()  !Got rid of all the flexibility, so this is just what you need
Check2StyleQRedGreen_Flex   PROCEDURE()  !Lets change all kinds of stuff

CheckboxMap     PROCEDURE() 
DingbatMap      PROCEDURE() 
ReplaceInto     PROCEDURE(*string into, string find,string replace)
    END

    
Ndx                 LONG
D4                  STRING(20)  
Wingdings_Check0    STRING('<168>|o|p|<0A8h>|<0A1h>|<0A2h>| |')   !0 must end | |'
Wingdings_Check1    STRING('<0FEh>|<0FDh>|x|n|<0A4h>|l|<0FCh>')
Wingdings2_Check0   STRING('<163>|<164>|<129>|<153>|<154>|<155>|O|<175>| |')
Wingdings2_Check1   STRING('S|P|R|Q|T|U|V|<176>|<177>|<178>|<162>|<167>|<168>|<169>|<157>|8|<158>')          
Wingdings3_Check0   STRING('w|v|s|r| |')
Wingdings3_Check1   STRING('u|t|p|q|<162>|<226>')
DingbatMapThread    LONG         
CheckboxMapThread   LONG         
    code
    Menu()
    return
Menu   PROCEDURE
CbWndPrv &CBWndPreviewClass ! CbWndPrv.Init()  
Window WINDOW('Style Checkbox Example'),AT(10,10,145,224),GRAY,SYSTEM,FONT('Segoe UI',9),ALRT(CtrlShiftW)
        BUTTON('Simplest - STRING(1)'),AT(6,4,131),USE(?Simple1)
        BUTTON('Flexible - STRING(1) and Byte'),AT(6,23,131),USE(?Flexible1)
        BUTTON('2 Q Styles - Colors - Code Like Icon'),AT(6,43,131),USE(?QStyles_Bsc)
        BUTTON('2 Q Styles - Colors - Flexible Config'),AT(6,60,131),USE(?QStyles_Flx)
        BUTTON('Big Font - Scale Test'),AT(6,80,131),USE(?Simple1BIG)
        BUTTON('Dingbat Map'),AT(6,100,60),USE(?DingMapBtn)
        BUTTON('Checkbox Map'),AT(77,100,60),USE(?CheckboxMap)
        BUTTON('Close'),AT(6,121,60),USE(?CloseBtn),STD(STD:Close)
        BUTTON('HALT'),AT(77,121,60),USE(?HaltBtn)
        PROMPT('Press Ctrl+Shift+W to add CBWndPreview Button at Top Left. This button is on all oth' & |
                'er windows without the hot key.'),AT(6,143,131,27),USE(?CbWndPrvFYI),CENTER
        PROMPT('Example of a LIST Checkbox column using LISTSTYLE and a Wingding.<13,10><13,10>For C' & |
                'IDC 2019 by Carl Barnes - www.CarlBarnes.com'),AT(6,176,131,42),USE(?FYI),CENTER
    END
    CODE 
    SYSTEM{PROP:PropVScroll}=1
    OPEN(Window)
    0{PROP:text}=clip(0{PROP:text}) &' - Library ' & system{PROP:LibVersion,2} &'.'& system{PROP:LibVersion,3}
    ACCEPT
        IF EVENT()=EVENT:AlertKey AND KEYCODE()=CtrlShiftW THEN 
           IF CbWndPrv &= NULL THEN 
              CbWndPrv &= NEW(CBWndPreviewClass)
              CbWndPrv.Init()
           END
           ?CbWndPrvFYI{PROP:Text}='To use CBWnPreview hover your mouse over the top-left corner and a secret flat button will popup. This is on all windows.'
           Message('To use CBWnPreview hover your mouse over the top-left corner and a secret flat button will popup.' & |
                    '||This button is also on all other windows without pressing Ctrl+Shift+W') 
        END 
        CASE ACCEPTED()
        OF ?Simple1     ; START(SimpleString1)
        OF ?Simple1BIG  ; START(SimpleString1BIG)
        OF ?Flexible1   ; START(FlexibleStringAndByte)
        OF ?QStyles_Bsc ; START(Check2StyleQRedGreen_Basic)
        OF ?QStyles_Flx ; START(Check2StyleQRedGreen_Flex)
        OF ?DingMapBtn  ; START(DingbatMap) 
        OF ?CheckboxMap ; START(CheckboxMap) 
        OF ?HaltBtn     ; HALT() 
        END
    END         
!========================================================================================
SimpleString1   PROCEDURE()
!Method 1 - Limits to single Font so Style for Column Z(1) can be in FORAMAT not Queue
!Queue has STRING(1) for Wingding Character. !It toggles between 2 string values so there is no index
CheckFnt STRING('Wingdings 2')
Check0   STRING('<163>')   !I like these Wingding 2's for best looking
Check1   STRING('<83>') 
CheckQ   QUEUE,PRE(ChkQ)
ChkBox      STRING(1)           !ChkQ:ChkBox
Name        STRING(20)          !ChkQ:Name
         END
FixColors  BYTE(1),STATIC         
Window WINDOW('Style Checkbox List - STRING(1) Z(1)'),AT(,,169,195),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        LIST,AT(10,2,150,125),USE(?ListCB),FROM(CheckQ),FORMAT('15C|~Tag~C@s1@Z(1)91L(2)|M~Name~@s32@')
        PROMPT('Style Z(1) in FORMAT to set column 1 to Wingdings 2. It sets Selected colors to Norm' & |
                'al so selecting the row does not change box colors.<13,10>Only 1 STRING(1) is used,' & |
                ' but the negative is a string comparision versus byte 1/0. '),AT(10,132,150,59), |
                USE(?PROMPT:Simple)
        BUTTON('Wingdings 1'),AT(10,178),USE(?Wing1),FONT(,8),TIP('Switch to Wingding 1. IMO seem ki' & |
                'nd of BOLD.')
        CHECK('Fix Selected Colors'),AT(71,178),USE(FixColors),TIP('Uncheck to see Color:None Selected Colors')
    END
CbWndPrv CBWndPreviewClass ! CbWndPrv.Init()                
    CODE
    OPEN(Window)
    CbWndPrv.Init() 
    ?ListCB{PROP:LineHeight} = 1 + ?ListCB{PROP:LineHeight} 
    DO LoadQRtn
    ACCEPT
        CASE FIELD()
        OF ?ListCB 
           CASE EVENT()
           OF EVENT:NewSelection  !Checkbox toggle code
              IF  ?ListCB{PROPLIST:MouseDownField}=1               |
              AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:field THEN
                 GET(CheckQ,CHOICE(?ListCB))
                 ChkQ:ChkBox = CHOOSE(ChkQ:ChkBox<>Check0,Check0,Check1)
                 PUT(CheckQ)
              END
           END          
        END 
        CASE ACCEPTED()  !NOT needed for CheckBox, just to change the look
        OF ?Wing1 ; Check0='<168>' ;  Check1='<254>' ; CheckFnt='Wingdings' 
                    DISABLE(?) ; DO LoadQRtn 
        OF ?FixColors ; DO LoadQRtn                   
        END         
    END
    CLOSE(Window)
LoadQRtn ROUTINE
    ?ListCB{PROPSTYLE:FontName,1}=CheckFnt
    ?ListCB{PROPSTYLE:FontSize,1}=12
    ?ListCB{PROPSTYLE:TextSelected,1} = CHOOSE(~FixColors,-1,COLOR:WindowText)  !COLOR:Black
    ?ListCB{PROPSTYLE:BackSelected,1} = CHOOSE(~FixColors,-1,COLOR:Window)      !COLOR:White 
    !Setup so Selecting row does not change colors else they invert and will not appear correctly 
    FREE(CheckQ)
    LOOP Ndx=1 TO 12
         D4=FORMAT(DATE(Ndx,1,2018),@d4) 
         ChkQ:Name=SUB(D4,1,INSTRING(' ',D4))
         ChkQ:ChkBox = CHOOSE(Random(1,2),Check0,Check1) 
         ADD(CheckQ)
    END  
    EXIT 
!========================================================================================
SimpleString1BIG   PROCEDURE() !Scale Big test
CheckFnt STRING('Wingdings 2')
CheckSiz LONG(40)
Check0   STRING('<163>')   !I like these Wingding 2's for best looking
Check1   STRING('<83>') 
CheckQ   QUEUE,PRE(ChkQ)
ChkBox          STRING(1)           !ChkQ:ChkBox
Name            STRING(20)          !ChkQ:Name 
Icon            STRING(1)           !ChkQ:Icon 
IconNo          LONG                !ChkQ:IconNo 
            END
FN          STRING(32)
FS          LONG            
FC          LONG            
FY          LONG            
Window WINDOW('Style Checkbox List BIG for Touch. Easy to Scale'),AT(,,169,150),GRAY,SYSTEM,FONT('Segoe UI',16,,FONT:regular),RESIZE
        MENUBAR,USE(?MENUBAR1)
            ITEM('Wingdings1'),USE(?Wing1)
            ITEM(' TagSize'),USE(?TagSize)
            ITEM(' ListSize'),USE(?ListFont)
        END
        LIST,AT(1,1),FULL,USE(?ListCB),VSCROLL,FONT(,20),FROM(CheckQ),FORMAT('25C|~Tag~@s1@Z(1)115L(2)|M~Name~@s32@999L(' & |
                '2)I~Icon~@P P@')
    END
CbWndPrv CBWndPreviewClass
    CODE
    OPEN(Window)
    CbWndPrv.Init()
    DO LoadQRtn
    ACCEPT
        CASE FIELD()
        OF ?ListCB 
           CASE EVENT()
           OF EVENT:NewSelection  !Checkbox toggle code
              IF (?ListCB{PROPLIST:MouseDownField}=1 AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:Field) |
              OR (?ListCB{PROPLIST:MouseDownField}=3 AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:Icon) THEN
                 GET(CheckQ,CHOICE(?ListCB))
                 ChkQ:IconNo = 3-ChkQ:IconNo
                 ChkQ:ChkBox = CHOOSE(ChkQ:IconNo,Check0,Check1)
                 PUT(CheckQ)
              END
           END          
        END 
        CASE ACCEPTED()  !NOT needed ofr CheckBox, just to change the look
        OF ?Wing1 ; CheckFnt='Wingdings' 
                    Check0='<168>' !'o'=111
                    Check1='<254>'
                    DISABLE(?) ; DO LoadQRtn   !Need to reload the queue, it relies on 2 specific string values  
        OF ?TagSize 
           FN=CheckFnt
           IF FONTDIALOG('Select Checkbox Wingding Font Size',FN,CheckSiz) THEN 
              HIDE(?LISTCB) ; ?ListCB{PROPSTYLE:FontSize,1}=CheckSiz ; UNHIDE(?LISTCB) ; DISPLAY
           END
        OF ?ListFont
           GETFONT(0,FN,FS,FC,FY) 
           IF FONTDIALOG('Select List Font',FN,FS,FC,FY) THEN 
              HIDE(?LISTCB)
                ?ListCB{PROP:LineHeight} = ''
                SETFONT(0,FN,FS,FC,FY)  ;   SETFONT(?ListCB,FN,FS,FC,FY) 
                ?ListCB{PROP:LineHeight} = 3 + ?ListCB{PROP:LineHeight}
              UNHIDE(?LISTCB) ; DISPLAY
           END        
        END         
    END
    CLOSE(Window)
LoadQRtn ROUTINE
    ?ListCB{PROP:LineHeight} = 3 + ?ListCB{PROP:LineHeight}
    !These are normal Icons for normal 8,9,10 font sizes.
    ?LISTCB{PROP:IconList,1}='~Checkoff.ico' ;  PRAGMA('link(Checkoff.ico)') 
    ?LISTCB{PROP:IconList,2}='~Checkon.ico'  ;  PRAGMA('link(Checkon.ico)') 

    ?ListCB{PROPSTYLE:FontName,1}=CheckFnt
    ?ListCB{PROPSTYLE:FontSize,1}=CheckSiz       !<-- Scale to any size needed
    ?ListCB{PROPSTYLE:TextSelected,1} = COLOR:WindowText  !COLOR:Black
    ?ListCB{PROPSTYLE:BackSelected,1} = COLOR:Window      !COLOR:White 
    !Selecting does not change colors else they invert and will not appear correctly 
    FREE(CheckQ)
    LOOP Ndx=1 TO 12
         D4=FORMAT(DATE(Ndx,1,2018),@d4) 
         ChkQ:Name=SUB(D4,1,INSTRING(' ',D4))
         ChkQ:IconNo = Random(1,2)
         ChkQ:ChkBox = CHOOSE(ChkQ:IconNo,Check0,Check1) 
         ADD(CheckQ)
    END  
    EXIT    
!======================================================================================== 
FlexibleStringAndByte    PROCEDURE  
!Limits to single Font so Style for Column Z(1) can be in FORMAT not Queue
!Queue has STRING(1) for Winging Character to Show and BYTE for Checked 1/0 
!Seems like LIST would draw faster if it does not have to change styles
CheckFnt    STRING('Wingdings 2') 
Check0      STRING(1)
Check1      STRING(1)
CheckQ      QUEUE,PRE(ChkQ)
CBChar          STRING(1)       !ChkQ:CBChar
Name            STRING(20)      !ChkQ:Name
CBByte          BYTE            !ChkQ:CBByte       !Normally not seen
            END
              
Window WINDOW('Style Checkbox List - String(1) and Byte Index in Queue'),AT(,,264,169),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        LIST,AT(8,5,150,125),USE(?ListCB),FROM(CheckQ),FORMAT('16C|~Tag~C@s1@Z(1)91L(2)|M~Name~@s32@50L(2)|M~Byte~@n3@')
        PROMPT('Style Z(1)  in FORMAT to set column 1 to Wingdings 2.  This limits Checkbox to a single font but is simp' & |
                'ler not needing Style code in the Queue.'),AT(6,139,237,25),USE(?PROMPT:Byte)
        PROMPT('From ...'),AT(166,7),USE(?WD2PickHead)
        PROMPT('UnChecked'),AT(166,21),USE(?Checked0:Prompt)
        LIST,AT(167,35,39,12),USE(Check0),VSCROLL,FONT('Wingdings 2',11),DROP(15)
        PROMPT('Checked'),AT(215,21),USE(?Checked1:Prompt)
        LIST,AT(215,35,39,12),USE(Check1),VSCROLL,FONT('Wingdings 2',11),DROP(15)
        BUTTON('Wingdings 1'),AT(166,60),USE(?Wing1),FONT(,8),TIP('Switch to Wingding 1. IMO kind of bold.')
    END
CbWndPrv CBWndPreviewClass
    CODE
    OPEN(WINDOW)
    CbWndPrv.Init()
    ?Check0{PROP:From}=Wingdings2_Check0
    ?Check1{PROP:From}=Wingdings2_Check1     
    ?Check0{PROP:Selected}=1 ; ?Check1{PROP:Selected}=1 
    ?ListCB{PROP:LineHeight} = 1 + ?ListCB{PROP:LineHeight} 
    DO LoadQRtn
    ACCEPT
        CASE FIELD()    
        OF ?ListCB 
           CASE EVENT()
           OF EVENT:NewSelection   !Check / Uncheck Code
              IF  ?ListCB{PROPLIST:MouseDownField}=1               |
              AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:field THEN           
                  GET(CheckQ,CHOICE(?ListCB))
                  ChkQ:CBByte = 1 - ChkQ:CBByte
                  ChkQ:CBChar = CHOOSE(~ChkQ:CBByte,Check0,Check1) 
                  PUT(CheckQ)
              END
           END
        END 
       !------------------- try Diff Looks
        CASE ACCEPTED() 
        OF ?Check0 OROF ?Check1 ; DO LoadQRtn 
        OF ?Wing1 ; DISABLE(?) 
                    CheckFnt = 'Wingdings'
                    ?Check0{PROP:Selected}=1 ; ?Check1{PROP:Selected}=1                     
                    ?Check0{PROP:From}=Wingdings_Check0  ; ?Check0{PROP:FontName}=CheckFnt 
                    ?Check1{PROP:From}=Wingdings_Check1  ; ?Check1{PROP:FontName}=CheckFnt    
                    DO LoadQRtn ; DISPLAY 

        END        
    END
    CLOSE(WINDOW)

LoadQRtn ROUTINE 
    ?WD2PickHead{PROP:Text}='From ' & CheckFnt
    ?ListCB{PROPSTYLE:FontName,1}=CheckFnt
    ?ListCB{PROPSTYLE:FontSize,1}=12
    ?ListCB{PROPSTYLE:TextSelected,1} = COLOR:WindowText  !COLOR:Black
    ?ListCB{PROPSTYLE:BackSelected,1} = COLOR:Window      !COLOR:White 
    CLEAR(CheckQ) ; FREE(CheckQ)
    LOOP Ndx=1 TO 12
         D4=FORMAT(DATE(Ndx,1,2018),@d4) 
         ChkQ:Name=SUB(D4,1,INSTRING(' ',D4))
         ChkQ:CBByte = Random(0,1)
         ChkQ:CBChar = CHOOSE(~ChkQ:CBByte,Check0,Check1) 
         ADD(CheckQ)
    END
!======================================================================================== 
!======================================================================================== 
Check2StyleQRedGreen_Basic  PROCEDURE  !Got rid of all the flexibility, so this is exactly what you need
!This  example works like Icon where you have a STRING(1) followed by a LONG with the PROP:IconList number
!The CBStyle LONG refers to 2 Styles with a picture @pXp where X are the Wingding Chars for Checked/Unchecked 
CheckQ      QUEUE,PRE(ChkQ)
CBChar          STRING(1)       !ChkQ:CBChar
CBStyle         LONG            !ChkQ:CBStyle ! 1=Unchecked 2=Checked
Name            STRING(20)      !ChkQ:Name
            END
Check0      STRING('<161>') !1)  l
Check1      STRING('<164>') !1)  n
Color0      LONG(Color:Gray) !Red)
Color1      LONG(Color:Green)
FontName    STRING('Wingdings  ')

Window WINDOW('Style Checkbox List - Q Style Works like Icons - Simple Window'),AT(,,190,215),GRAY,SYSTEM,FONT('Segoe UI',9), |
            RESIZE
        LIST,AT(8,5,150,125),USE(?ListCB),FROM(CheckQ),FORMAT('16C|Y~Tag~@s1@91L(2)|M~Name~@s32@50L(2)|M~Style#~@n3@#2#')
        BUTTON('W 2'),AT(162,4,23),USE(?Wing2),FONT(,8),TIP('Switch to Wingdings 2.')
        PROMPT('Check/Icon comes from Style Number in Queue. 2 Styles use @PxP picture to render Checked/Unchecked Symbo' & |
                'ls. This also allows Check/Icon to come from multiple fonts with different colors.<13,10><13,10>Code wo' & |
                'rks like Icons, Style LONG toggles between 1/2 to change "icon".'),AT(7,136,151,70),USE(?PROMPT1:2)
        STRING('Checks'),AT(165,25,14,200),USE(?Checkz),ANGLE(2700) ,FONT(,10)
    END
CbWndPrv CBWndPreviewClass    
    CODE    
    OPEN(WINDOW)
    CbWndPrv.Init()    
    ?ListCB{PROP:LineHeight} = 1 + ?ListCB{PROP:LineHeight} 
    DO LoadQRtn
    ACCEPT 
        CASE FIELD()    
        OF ?ListCB     !-------- Check/Uncheck Code
           CASE EVENT()
           OF EVENT:NewSelection
              IF  ?ListCB{PROPLIST:MouseDownField}=1               |
              AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:field THEN   
                  GET(CheckQ,CHOICE(?ListCB))
                  ChkQ:CBStyle  = 3 - ChkQ:CBStyle
                  PUT(CheckQ)
              END  
           END
        OF ?Wing2 ; DISABLE(?) 
            Check0='<163>'
            Check1='<168>'  !1689 Larger
            FontName='Wingdings 2'
            DO LoadQRtn ; DISPLAY
        END
    END
    CLOSE(WINDOW)
    
LoadQRtn ROUTINE
    ?Checkz{PROP:Text}='UnCheck = ' & VAL(Check0) & '  -  Checked = ' & VAL(Check1) &'  - '&  FontName
    LOOP Ndx=1 TO 2        !Config 2 Styles: 1=Checked, 2=UnChecked
        ?ListCB{PROPSTYLE:Picture,Ndx}=CHOOSE(Ndx,'@P' & Check1 &'P','@P' & Check0 &'P') 
        ?ListCB{PROPSTYLE:FontName,Ndx}=FontName  !'Wingdings'
        ?ListCB{PROPSTYLE:FontSize,Ndx}=12
        ?ListCB{PROPSTYLE:TextColor,Ndx} =    CHOOSE(Ndx,Color1,Color0)
        ?ListCB{PROPSTYLE:BackColor,Ndx} =    COLOR:White 
        ?ListCB{PROPSTYLE:TextSelected,Ndx} = CHOOSE(Ndx,Color1,Color0)
        ?ListCB{PROPSTYLE:BackSelected,Ndx} = COLOR:White 
    END 
    FREE(CheckQ) ; CLEAR(CheckQ)
    LOOP Ndx=1 TO 12
         D4=FORMAT(DATE(Ndx,1,2018),@d4) 
         ChkQ:Name=SUB(D4,1,INSTRING(' ',D4))
         ChkQ:CBStyle = Random(1,2)
         ADD(CheckQ)
    END  
!======================================================================================== 
Check2StyleQRedGreen_Flex    PROCEDURE  
!This  example works like Icon where you have a STRING(1) followed by a LONG with the PROP:IconList number
!The CBStyle LONG refers to 2 Styles with a picture @pXp where X are the Wingding Chars for Checked/Unchecked 
CheckQ      QUEUE,PRE(ChkQ)
CBChar          STRING(1)       !ChkQ:CBChar
CBStyle         LONG            !ChkQ:CBStyle ! 1=Unchecked 2=Checked
Name            STRING(20)      !ChkQ:Name
            END
Check0      STRING(1)
Check1      STRING(1) 
Check0Byte  BYTE,OVER(Check0)
Check1Byte  BYTE,OVER(Check1)
Color0      LONG(Color:Red)
Color1      LONG(Color:Green)
From0       PSTRING(256),DIM(5)
From1       PSTRING(256),DIM(5)
FontNoNow   BYTE
FontNo      BYTE(0)
FontName    STRING('Wingdings  ')
AddByte     BYTE
AddChr      STRING(1),OVER(AddByte)        
Window WINDOW('Style Checkbox List - Q Style Works like Icons - Flexible Window'),AT(,,251,196),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        LIST,AT(8,5,150,125),USE(?ListCB),FROM(CheckQ),FORMAT('16C|Y~Tag~@s1@91L(2)|M~Name~@s32@50L(2)|M~Style#~@n3@#2#')
        PROMPT('Check/Icon comes from Style Number in Queue. 2 Styles use @PxP picture to render Checked/Unchecked Symbo' & |
                'ls. This also allows Check/Icon to come from multiple fonts with different colors.<13,10><13,10>Code wo' & |
                'rks like Icons, Style LONG toggles between 1/2 to change "icon".'),AT(6,142,233,44),USE(?PROMPT1:2)
        STRING(@s12),AT(169,5,73),USE(FontName),CENTER
        PROMPT('&UnChecked'),AT(169,16),USE(?Check0:Prompt)
        LIST,AT(169,28,34,12),USE(Check0),VSCROLL,FONT('Wingdings',14,COLOR:Red),DROP(15),FROM('n|l|p|o|<0A8h>|<0A1h>|' & |
                '<0A2h>|<0FEh>|<0FDh>|<0A4h>|J|K|L')
        STRING(@n3),AT(169,43),USE(Check0Byte)
        STRING(@s1),AT(184,43),USE(Check0,, ?Check0:Chr)
        PROMPT('&Checked'),AT(208,16),USE(?Check1:Prompt)
        LIST,AT(208,28,34,12),USE(Check1),VSCROLL,FONT('Wingdings',14,COLOR:Green),DROP(15)
        STRING(@n3),AT(208,43),USE(Check1Byte)
        STRING(@s1),AT(223,43),USE(Check1,, ?Check1:Chr)
        BUTTON('Color'),AT(169,57,34),USE(?Color0Btn),SKIP
        BUTTON('Color'),AT(208,57,34),USE(?Color1Btn),SKIP
        BUTTON('Color Gray / Black'),AT(169,75,74),USE(?GrayBlackBtn),SKIP,TIP('Unchecked Gray, Checked Black<13,10>Done' & |
                ' to make Checked stand out.')
        BUTTON('&Font'),AT(169,97),USE(?FontBtn),SKIP
        PROMPT('Add Chr:'),AT(169,120),USE(?AddByte:Prompt)
        ENTRY(@n3b),AT(200,118,19),USE(AddByte),TIP('Add Character to Lists')
        BUTTON('&Map'),AT(223,117,22),USE(?MapBtn),SKIP
    END
CbWndPrv CBWndPreviewClass
    CODE
    OPEN(WINDOW)
    CbWndPrv.Init()    
    DO SetCheckRtn 
    ?ListCB{PROP:LineHeight} = 1 + ?ListCB{PROP:LineHeight} 
    DO LoadQRtn
 !PROP:LineCount tells how many items are in a FROM('string') ... but why did I want to know?
    ACCEPT 
        CASE FIELD()    
        OF ?ListCB     !-------- Check/Uncheck Code
           CASE EVENT()
           OF EVENT:NewSelection
              IF  ?ListCB{PROPLIST:MouseDownField}=1               |
              AND ?ListCB{PROPLIST:MouseDownZone} = LISTZONE:field THEN   
                  GET(CheckQ,CHOICE(?ListCB))
                  ChkQ:CBStyle  = 3 - ChkQ:CBStyle
                  PUT(CheckQ)
              END  
           END  
        END
        !---------------------------- Config Look
        CASE ACCEPTED() 
        OF ?Check0 ; ?ListCB{PROPSTYLE:Picture,2}='@P'& Check0 &'P' ; DO ColorRtn !Hide/Unhide redraws
        OF ?Check1 ; ?ListCB{PROPSTYLE:Picture,1}='@P'& Check1 &'P' ; DO ColorRtn 
        OF ?Color0Btn ; IF COLORDIALOG(,Color0) THEN DO ColorRtn.
        OF ?Color1Btn ; IF COLORDIALOG(,Color1) THEN DO ColorRtn.
        OF ?GrayBlackBtn ; Color0=COLOR:Silver ; Color1=COLOR:Black ; DO ColorRtn 
        OF ?FontBtn ; DO FontRtn 
        OF ?AddByte ; DO AddByteRtn
        OF ?MapBtn  ; START(DingbatMap)
        END        
    END
    CLOSE(WINDOW)

FontRtn  ROUTINE
    FontNo = POPUP('Wingdings{{1|2|3}|-|Webdings|Symbol') ; IF ~FontNo THEN EXIT.    
    FontName=CHOOSE(FontNo,'Wingdings','Wingdings 2','Wingdings 3','Webdings','Symbol')    
    ?Check0{PROP:FontName}=FontName
    ?Check1{PROP:FontName}=FontName
    DO SetCheckRtn
    DO ColorRtn
SetCheckRtn ROUTINE 
    IF FontNo=0 THEN  !Store From so can retian AddChr if Font change
       FontNo=1
       From0[1] = 'l|n|J|K|L|' & Wingdings_Check0 & Wingdings_Check1
       From1[1] = 'n|l|J|K|L|' & Wingdings_Check1 &'|'& Wingdings_Check0    
       From0[2] = Wingdings2_Check0 & Wingdings2_Check1
       From1[2] = Wingdings2_Check1 &'|'& Wingdings2_Check0    
       From0[3] = Wingdings3_Check0 & Wingdings3_Check1
       From1[3] = Wingdings3_Check1 &'|'& Wingdings3_Check0  
       From0[4] = 'c| |g|<<|=|r|<163>'
       From1[4] = 'g|<<|=|r|<163>' &'|'& 'c| |' 
       From0[5] = '<224>| |' &  '<168>|<196>|<197>|<183>|C'
       From1[5] = '<168>|<196>|<197>|<183>|C' &'|'& '<224>| |'         
    END
    ?Check0{PROP:From} = From0[FontNo]
    ?Check1{PROP:From} = From1[FontNo]
    IF FontNo <> FontNoNow THEN 
       ?Check0{PROP:Selected}=1 ; ?Check1{PROP:Selected}=1
       FontNoNow = FontNo
    END   
AddByteRtn ROUTINE
    IF AddByte < 32 THEN 
       AddByte = 32
    ELSE  
        From0[FontNo] = AddChr &'|'& From0[FontNo] 
        From1[FontNo] = AddChr &'|'& From1[FontNo]
        DO SetCheckRtn ; AddByte=0
    END
    DISPLAY ; SELECT(?AddByte)
    
ColorRtn ROUTINE  !Hide/Unhide to repaint
    HIDE(?ListCB)   ; HIDE(?Check0)  ; HIDE(?Check1)  
    ?Check0{PROP:FontColor}=Color0 ; (?Check0{PROP:ListFEQ}){PROP:FontColor}=Color0
    ?Check1{PROP:FontColor}=Color1 ; (?Check1{PROP:ListFEQ}){PROP:FontColor}=Color1
    DO LoadQRtn
    UNHIDE(?ListCB) ; UNHIDE(?Check0) ; UNHIDE(?Check1) 
    DISPLAY
    
LoadQRtn ROUTINE 
    LOOP Ndx=1 TO 2        !Config 2 Styles: 1=Checked, 2=UnChecked
        ?ListCB{PROPSTYLE:Picture,Ndx}=CHOOSE(Ndx,'@P' & Check1 &'P','@P' & Check0 &'P') 
        ?ListCB{PROPSTYLE:FontName,Ndx}=FontName  !'Wingdings'
        ?ListCB{PROPSTYLE:FontSize,Ndx}=12
        ?ListCB{PROPSTYLE:TextColor,Ndx} =    CHOOSE(Ndx,Color1,Color0)
        ?ListCB{PROPSTYLE:BackColor,Ndx} =    COLOR:White 
        ?ListCB{PROPSTYLE:TextSelected,Ndx} = CHOOSE(Ndx,Color1,Color0)
        ?ListCB{PROPSTYLE:BackSelected,Ndx} = COLOR:White 
    END 
    FREE(CheckQ) ; CLEAR(CheckQ)
    LOOP Ndx=1 TO 12
         D4=FORMAT(DATE(Ndx,1,2018),@d4) 
         ChkQ:Name=SUB(D4,1,INSTRING(' ',D4))
         ChkQ:CBStyle = Random(1,2)
         ADD(CheckQ)
    END  
!========================================================================================
CheckboxMap PROCEDURE()
DingQ     QUEUE,PRE(DQ)
N0          BYTE            !DQ:N0
A0          STRING(1)       !DQ:A0
C0          STRING(1)       !DQ:C0
Y0          LONG            !DQ:Y0  Style
C1          STRING(1)       !DQ:C1  
Y1          LONG            !DQ:Y1  Style
A1          STRING(1)       !DQ:A1
N1          BYTE            !DQ:N1 
Font        STRING(32)      !DQ:Font
            END
Window WINDOW('Checkbox Character Ideas'),AT(,,200,300),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        LIST,AT(0,1),FULL,USE(?List),VSCROLL,FONT('Consolas',10),FROM(DingQ),FORMAT('[16C~No~@n3@14C~Ch~@s1@16C|Y~Bx~@s' & |
                '1@]|~Unchecked~[16CY~Bx~@s1@14C~Ch~@s1@18C|~No~@n3@]|~Checked~60L(2)F~<13,10>Font Name~@s32@')
    END
CbWndPrv CBWndPreviewClass
    CODE
    IF CheckboxMapThread THEN ; POST(EVENT:User,,CheckboxMapThread) ; RETURN ; END ; CheckboxMapThread=THREAD()
    OPEN(WINDOW)
    CbWndPrv.Init()    
    ?List{PROP:LineHeight} = 1 + ?List{PROP:LineHeight} 
    DO LoadQRtn
    ACCEPT ; IF EVENT()=EVENT:User THEN 0{PROP:Active}=1. ; END
    CLOSE(WINDOW)
    CheckboxMapThread=0
LoadQRtn ROUTINE
    DATA
SFont       STRING(32)
Check0      STRING(64)
Check1      STRING(64)
    CODE
    LOOP Ndx=1 TO 6
        SFont=CHOOSE(Ndx,'Wingdings','Wingdings 2','Wingdings 3','Webdings','Symbol','Segoe UI')
        ?List{PROPSTYLE:FontName,Ndx}=SFont
        ?List{PROPSTYLE:FontSize,Ndx}=14
        ?List{PROPSTYLE:TextSelected,Ndx} = COLOR:WindowText  !COLOR:Black
        ?List{PROPSTYLE:BackSelected,Ndx} = COLOR:Window      !COLOR:White 
        CASE Ndx
        OF 1  ; Check0 = Wingdings_Check0  ; Check1 = Wingdings_Check1
        OF 2  ; Check0 = Wingdings2_Check0 ; Check1 = Wingdings2_Check1
        OF 3  ; Check0 = Wingdings3_Check0 ; Check1 = Wingdings3_Check1
        OF 4  ; Check0 = 'c| |' ; Check1 = 'g|<<|=|r|<163>'                 ! Webding
        OF 5  ; Check0 = '<224>| |' ; Check1 = '<168>|<196>|<197>|<183>|C'  ! Symbol
        OF 6  ; Check0 = ' |' ; Check1 = 'X'                                ! Segoe UI
        END
        LOOP WHILE Check0 OR Check1  
            CLEAR(DingQ)
            DQ:A0=Check0 ; DQ:C0=DQ:A0 ; DQ:N0=VAL(DQ:A0) ; DQ:Y0=Ndx
            DQ:A1=Check1 ; DQ:C1=DQ:A1 ; DQ:N1=VAL(DQ:A1) ; DQ:Y1=Ndx            
            DQ:Font=SFont
            ADD(DingQ) 
            Check0=SUB(Check0,3,99)
            Check1=SUB(Check1,3,99)
        END 
    END 
!======================================================================================== 
DingbatMap PROCEDURE()
DingQ     QUEUE,PRE(DQ)
N1          BYTE            !DQ:N1
N2          BYTE            !DQ:N2 
N3          BYTE            !DQ:N3
N4          BYTE            !DQ:N4
C1          STRING(1)       !DQ:C1 
T1          STRING(5)       !DQ:T1 
C2          STRING(1)       !DQ:C2
T2          STRING(5)       !DQ:T2
C3          STRING(1)       !DQ:C3
T3          STRING(5)       !DQ:T3 
C4          STRING(1)       !DQ:C4
T4          STRING(5)       !DQ:T4
          END
DingQOvr  GROUP,PRE(),OVER(DingQ)
DQ:N        BYTE       ,DIM(4)
DQ:CT       STRING(1+5),DIM(4)
          END            
Window WINDOW('Dingbat Map - Tooltip shows Val() and Chr()'),AT(,,480,300),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        LIST,AT(0,1),FULL,USE(?List),HVSCROLL,FONT('Consolas',10),FROM(DingQ),COLUMN,|
            FORMAT('[15C@n3@16C@n3@16C@n3@16C@n3@]|~Char Numbers~' & |
                '[14C@s1@PZ(1)14C@s1@PZ(1)14C@s1@PZ(1)16CF@s1@PZ(1)]|~Consolas~' & |
                '[18L(3)@s1@PZ(2)#5#16L(1)@s1@PZ(2)#7#16L(1)@s1@PZ(2)#9#16L(1)@s1@PZ(2)#11#]|~Wingdings~')
       END
FontName    PSTRING(20),AUTO    
GroupZ2     STRING(128),AUTO    
GrpFmt      STRING(128),AUTO    
FmtAdd      STRING(128 * 5)
C           USHORT,AUTO
CbWndPrv CBWndPreviewClass 
    CODE
    IF DingbatMapThread THEN ; POST(EVENT:User,,DingbatMapThread) ; RETURN ; END ; DingbatMapThread=THREAD()
    OPEN(WINDOW)
    CbWndPrv.Init()    
    ?List{PROP:LineHeight} = 1 + ?List{PROP:LineHeight} 
    DO LoadQRtn
    ACCEPT ; IF EVENT()=EVENT:User THEN 0{PROP:Active}=1. ; END
    CLOSE(WINDOW)
    DingbatMapThread=0
LoadQRtn ROUTINE
    GroupZ2 = ?List{PROPLIST:Group + PROPLIST:Format, 9}
    LOOP Ndx=1 TO 7                                               
        FontName=CHOOSE(Ndx,'Consolas','Wingdings','Wingdings 2','Wingdings 3','Webdings','Symbol','Segoe UI')
        ?List{PROPSTYLE:FontName,Ndx}=FontName
        ?List{PROPSTYLE:FontSize,Ndx}=CHOOSE(Ndx=1 OR Ndx=7,10,14) 
        IF Ndx < 3 THEN CYCLE.
        GrpFmt = GroupZ2
        ReplaceInto(GrpFmt,'~Wingdings~', '~'& FontName &'~')
        ReplaceInto(GrpFmt,'Z(2)', 'Z('& Ndx &')') 
        FmtAdd=CLIP(FmtAdd) & GrpFmt
    END
    ?List{PROP:Format} = ?List{PROP:Format} & FmtAdd    
    LOOP Ndx=32 TO 32+56-1 
        LOOP C=1 TO 4
            DQ:N[C]  = Ndx + (C-1)*56
            DQ:CT[C] = CHR(DQ:N[C]) & DQ:N[C] &' '& CHR(DQ:N[C])
        END     
        ADD(DingQ)
    END  
    EXIT
!========================================================================================
ReplaceInto  PROCEDURE(*string into, string find,string replace)   !From ABError, tweaked
L   LONG,AUTO
    CODE
    find = UPPER(find)
    IF find<>UPPER(replace)
       LOOP
         L = INSTRING(find,UPPER(into),1,1)
         IF ~L THEN BREAK.
         into = SUB(into,1,L-1) & replace & SUB(into,L + SIZE(find),SIZE(into))
       END
    END
    RETURN

    OMIT('!-Dummy-CbWndPrv',Inc_CbWndPrv)
CBWndPreviewClass.Init  PROCEDURE(BYTE SecretBtnType=1, LONG SecretKey=1879)   !Dummy so below can have code without class
    CODE
    Message('CBWndPreviewClass is NOT compiled into this program.||To add it change (0) to (1) in: Inc_CbWndPrv EQUATE(1)')
          !-Dummy-CbWndPrv    