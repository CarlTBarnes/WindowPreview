!Project for PPT on Window Previewer CIDC 2019 by Carl Barnes
  PROGRAM
  INCLUDE('KEYCODES.CLW')
  INCLUDE('CbWndPreview.INC'),ONCE  !File was in CIDC 2019 Download. Put in ths folder or Accessory\LibSrc\Win
  MAP
Main         PROCEDURE()
LoginTest    PROCEDURE()
BrowsePRF005 PROCEDURE()
UpdatePRF005 PROCEDURE()
  END
PRF005        GROUP,PRE(TYP)
Code                     SHORT
Desc                     STRING(30)
CertifiedPersonnel       STRING(1)
StudentCategory          STRING(1)
EEOCReportable           STRING(1)
TeacherSubstitute        BYTE
TrsJobCategory           STRING(1)
NormalHrsPerDay          DECIMAL(3,2)
BrdTrsRate               DECIMAL(5,5),DIM(3)
EmpTrsRate               DECIMAL(5,5),DIM(3)
BrdTrsInsRate            DECIMAL(6,6),DIM(3)
EmpTrsInsRate            DECIMAL(6,6),DIM(3)
BrdTrsInsRateOtherPay    DECIMAL(6,6),DIM(3)
EmpTrsInsRateOtherPay    DECIMAL(6,6),DIM(3)
PayGroup                 SHORT
BenefitGroup             STRING(1)
                      END
  CODE
  Main()

Main    PROCEDURE()  
Window WINDOW('PPT Window Preview Code'),AT(,,110,129),CENTER,GRAY,SYSTEM,FONT('Segoe UI',9)
        BUTTON('Login Window'),AT(23,8,65),USE(?LoginBtn)
        BUTTON('Sample Browse'),AT(23,28,65),USE(?BrowseBtn)
        BUTTON('Sample Form'),AT(23,46,65),USE(?FormBtn)
        BUTTON('Halt'),AT(39,75),USE(?HaltBtn)
        PROMPT('Ctrl+Shift+W on these turns on CB Wnd Preview. A secret flat button is at the top-left.'), |
                AT(2,98,106,29),USE(?PROMPT1)
    END
    CODE
    OPEN(Window)
    ACCEPT
        CASE ACCEPTED()
        OF ?LoginBtn  ; START(LoginTest)
        OF ?BrowseBtn ; START(BrowsePRF005)
        OF ?FormBtn   ; START(UpdatePRF005)
        OF ?HaltBtn ; HALT
        END
    END
!=============================================
LoginTest    PROCEDURE()
ShowYes EQUATE('1')  !,VALUE(ShowYes,ShowNo)
ShowNo  EQUATE('0')
bShowName STRING(1)
User STRING(20)  
Pwd  STRING(20)
Window WINDOW('Login'),AT(,,187,101),CENTER,GRAY,SYSTEM,FONT('Segoe UI',9)
        PROMPT('User Name:'),AT(14,11),USE(?Name:Pmt)
        ENTRY(@s20),AT(54,11,50),USE(User),PASSWORD
        PROMPT('Password:'),AT(14,29),USE(?Pwd:Pmt)
        ENTRY(@s20),AT(54,29,50),USE(Pwd),PASSWORD
        !,VALUE(ShowYes,ShowNo)
        CHECK('&Show Name'),AT(108,11),USE(bShowName),SKIP
        BUTTON('Login'),AT(50,49),USE(?LoginBtn),STD(STD:Close)
        BUTTON('Cancel'),AT(93,49),USE(?CancelBtn),STD(STD:Close)
        GROUP,AT(16,65,155,18),USE(?Secret),BOXED,HIDE
            BUTTON('Backdoor'),AT(34,69,38,11),USE(?BackDoorBtn)
            BUTTON('User Edit'),AT(74,69,38,11),USE(?UEditBtn)
            BUTTON('Convert'),AT(114,69,38,11),USE(?ConvertBtn)
        END
    END
CbWndPrv CBWndPreviewClass  !Simply make this one always
    CODE
    OPEN(Window)
    CbWndPrv.Init(2)
    ACCEPT
        CASE ACCEPTED()
        OF ?bShowName ; ?User{PROP:Password}=1-bShowName ; DISPLAY 
        END
    END
!==================================================================
BrowsePRF005 PROCEDURE
MoveThisYrTrsDataToLastYr   BYTE 
ShowTrsRatesOnly            BYTE
                       
TeacherSub           STRING(1)
Queue:Browse         QUEUE,PRE()                           ! Browsing Queue
BRW1::TYP:Code         LIKE(TYP:Code)                      ! Queue Display field
BRW1::TYP:Desc         LIKE(TYP:Desc)                      ! Queue Display field
BRW1::TYP:NormalHrsPerDay LIKE(TYP:NormalHrsPerDay)        ! Queue Display field
BRW1::TYP:NormalHrsPerDay:NormalFG LONG                    ! Normal Foreground
BRW1::TYP:NormalHrsPerDay:NormalBG LONG                    ! Normal Background
BRW1::TYP:NormalHrsPerDay:SelectedFG LONG                  ! Selected Foreground
BRW1::TYP:NormalHrsPerDay:SelectedBG LONG                  ! Selected Background
BRW1::TYP:CertifiedPersonnel LIKE(TYP:CertifiedPersonnel)  ! Queue Display field
BRW1::TeacherSub       LIKE(TeacherSub)                    ! Queue Display field
BRW1::TYP:TrsJobCategory LIKE(TYP:TrsJobCategory)          ! Queue Display field
BRW1::TYP:StudentCategory LIKE(TYP:StudentCategory)        ! Queue Display field
BRW1::TYP:PayGroup     LIKE(TYP:PayGroup)                  ! Queue Display field
BRW1::TYP:BrdTrsRate_1_ LIKE(TYP:BrdTrsRate[1])            ! Queue Display field
BRW1::TYP:EmpTrsRate_1_ LIKE(TYP:EmpTrsRate[1])            ! Queue Display field
BRW1::TYP:BrdTrsInsRate_1_ LIKE(TYP:BrdTrsInsRate[1])      ! Queue Display field
BRW1::TYP:EmpTrsInsRate_1_ LIKE(TYP:EmpTrsInsRate[1])      ! Queue Display field
BRW1::TYP:BrdTrsRate_2_ LIKE(TYP:BrdTrsRate[2])            ! Queue Display field
BRW1::TYP:EmpTrsRate_2_ LIKE(TYP:EmpTrsRate[2])            ! Queue Display field
BRW1::TYP:BrdTrsInsRate_2_ LIKE(TYP:BrdTrsInsRate[2])      ! Queue Display field
BRW1::TYP:EmpTrsInsRate_2_ LIKE(TYP:EmpTrsInsRate[2])      ! Queue Display field
BRW1::TYP:EEOCReportable LIKE(TYP:EEOCReportable)          ! Queue Display field
                     END                                   ! END (Browsing Queue)

Window WINDOW('Employee Types'),AT(,,686,254),CENTER,GRAY,SYSTEM,FONT('Microsoft Sans Serif',8), |
            ALRT(CtrlShiftW)
        SHEET,AT(5,3,677,245),USE(?SHEET1)
            TAB(' By Code '),USE(?TAB1)
            END
            TAB(' By Name '),USE(?TAB2)
            END
            TAB(' EEOC Only '),USE(?TAB3)
            END
        END
        LIST,AT(8,25,668,179),USE(?BrowseEmpTypes),IMM,VSCROLL,MSG('Browsing Records'),FROM(Queue:Browse), |
                FORMAT('21C|M~Type~@n3@140L(3)|~Description~L(1)@s30@27R(3)|*~Normal<0DH,0AH>Hours~C' & |
                '(0)@n4.2@29C|~Certified~@s1@30C|~Teacher<0DH,0AH>Sub.~@s1@24C|~TRS<0DH,0AH>Job~@s1@' & |
                '29C(3)|~Student ~C(0)@s1@26R(6)|~Pay<0DH,0AH>Group~C(0)@n_2@[33C(3)|~Brd.~C(0)@n6.5' & |
                'b@37R(3)|~Emp.~C(0)@n6.5b@](67)|~TRS Rates This Yr~[37R(3)|~Brd.~C(0)@n7.6b@37R(3)|' & |
                '~Emp.~C(0)@n7.6b@]|~TRS Ins. Rates This Yr~[37R(3)|~Brd~C(0)@n6.5b@37R(3)|~Emp.~C(0' & |
                ')@n6.5b@]|~TRS Rates Next Yr~[37R(3)|~Brd.~C(0)@n7.6b@37R(3)|~Emp.~C(0)@n7.6b@]|~TR' & |
                'S Ins. Rates Next Yr.~27C(3)|~EEOC~C(0)@s1@')
        BUTTON('&Select'),AT(205,209,33,14),USE(?Select)
        BUTTON('&View'),AT(241,209,33,14),USE(?View)
        BUTTON('&Insert'),AT(277,209,33,14),USE(?Insert),KEY(InsertKey)
        BUTTON('Co&py'),AT(317,209,33,14),USE(?CopyBtn),KEY(CtrlInsert)
        BUTTON('&Change'),AT(353,209,33,14),USE(?Change),KEY(CtrlEnter)
        BUTTON('&Delete'),AT(389,209,33,14),USE(?Delete),KEY(DeleteKey)
        BUTTON('Close'),AT(428,209,33,14),USE(?Close)
        BUTTON('Print With TRS Rates'),AT(501,225,79,14),USE(?PrintEmployeeTypesWithTrsRatesButton)
        CHECK('Move This Year''s Trs Data To Last Year.'),AT(12,230),USE(MoveThisYrTrsDataToLastYr)
        BUTTON('Print Employee Types'),AT(501,209,79),USE(?BUTTON1)
        BUTTON('Update Employee Master With TRS Job Types'),AT(583,209,93,31),USE(?UpdatrPrf100WithJobTypes) |
                
        CHECK('Show Types With TRS Rates'),AT(12,209,149),USE(ShowTrsRatesOnly),SKIP
    END
Ndx LONG
CbWndPrv &CBWndPreviewClass  !In live code I implement with a Hot key CtrlShiftW that NEWs the Object so it ca nhave no effect
    CODE
    DO LoadQRtn
    OPEN(Window)  
    ACCEPT
        IF EVENT()=EVENT:AlertKey AND KEYCODE()=CtrlShiftW THEN 
           IF CbWndPrv &= NULL THEN 
              CbWndPrv &= NEW(CBWndPreviewClass)
              CbWndPrv.Init()
           END !ELSE
              CbWndPrv.Reflection()     !Display it on CtrlShiftW
           !END
        END
    END
    CLOSE(Window) 
    IF NOT CbWndPrv &= NULL THEN DISPOSE(CbWndPrv). !Not absolutely required, may be small leak but its just for debug
    RETURN
LoadQRtn ROUTINE
    CLEAR(Queue:Browse)
    BRW1::TYP:NormalHrsPerDay:NormalFG=Color:Red
    BRW1::TYP:NormalHrsPerDay:NormalBG=Color:White
    BRW1::TYP:NormalHrsPerDay:SelectedFG=-1
    BRW1::TYP:NormalHrsPerDay:SelectedBG=-1
    LOOP Ndx=1 TO 20 
        BRW1::TYP:Code = Ndx
        BRW1::TYP:Desc = 'Code #' & Ndx
        BRW1::TYP:NormalHrsPerDay = 7.5
        BRW1::TYP:CertifiedPersonnel = CHOOSE(Ndx % 2 + 1,'Y','N')
        BRW1::TeacherSub = BRW1::TYP:CertifiedPersonnel
        !BRW1::TYP:TrsJobCategory = CHOOSE(Ndx % 3 + 1,'T','A','C')
        BRW1::TYP:TrsJobCategory = CHOOSE(Ndx%6+1,'T','A','C','O','S','N')
        BRW1::TYP:StudentCategory = CHOOSE(Ndx % 2 + 1,'N','Y') 
        BRW1::TYP:PayGroup = Ndx %5 + 3
        BRW1::TYP:BrdTrsRate_1_ = .01
        BRW1::TYP:EmpTrsRate_1_ = .02
        BRW1::TYP:BrdTrsInsRate_1_ = .03
        BRW1::TYP:EmpTrsInsRate_1_ = .04
        BRW1::TYP:BrdTrsRate_2_ = .05
        BRW1::TYP:EmpTrsRate_2_ = .06
        BRW1::TYP:BrdTrsInsRate_2_ = .07
        BRW1::TYP:EmpTrsInsRate_2_ = .08
        BRW1::TYP:EEOCReportable = CHOOSE(Ndx % 2 + 1,'Y','N')
        ADD(Queue:Browse)
    END         
!===================================
UpdatePRF005 PROCEDURE 
TIME:Description    STRING(40) 
Window WINDOW('Update Employee Type'),AT(,,479,266),GRAY,SYSTEM,FONT('Microsoft Sans Serif',8),CENTER,ALRT(CtrlShiftW)
        ENTRY(@n3),AT(37,7,16,10),USE(TYP:Code)
        STRING('TYPE:'),AT(8,7),USE(?String2)
        PROMPT('DESC.:'),AT(68,7,24),USE(?TYP:Desc:Prompt)
        ENTRY(@s30),AT(99,6,138,10),USE(TYP:Desc),UPR
        OPTION('TRS JOB CATEGORY'),AT(9,38,281,36),USE(TYP:TrsJobCategory),BOXED
            RADIO('TEACHER'),AT(14,49),USE(?TYP:TrsJobCategory:Radio1),VALUE('T')
            RADIO('ADMINISTRATOR'),AT(14,60),USE(?TYP:TrsJobCategory:Radio3),VALUE('A')
            RADIO('COUNSELING'),AT(101,47),USE(?TYP:TrsJobCategory:Radio4),TIP('Counseling includes ' & |
                    'School Counselors, Social Workers, <0DH,0AH>School Nurses,Speech / LanguagePath' & |
                    'ologist, etc.'),VALUE('C')
            RADIO('OTHER'),AT(101,60),USE(?TYP:TrsJobCategory:Radio5),VALUE('O')
            RADIO('SUBSTITUTE TEACHER'),AT(178,47),USE(?TYP:TrsJobCategory:Radio2),VALUE('S')
            RADIO('NOT TRS REPORTABLE'),AT(178,60),USE(?TYP:TrsJobCategory:Radio6),VALUE('N')
        END
        CHECK('TEACHER SUBSTITUTE? '),AT(9,79),USE(TYP:TeacherSubstitute)
        PROMPT('The data entered on this form is used when adding a new Employee, Contract or Hourly' & |
                ' Rate.'),AT(7,22,465,14),USE(?Prompt6),FONT(,,COLOR:Blue),CENTER
        OPTION('CERTIFIED PERSON'),AT(9,92,83,24),USE(TYP:CertifiedPersonnel),BOXED
            RADIO('YES'),AT(17,102),USE(?TYP:CertifedPersonnel:Radio1),VALUE('Y')
            RADIO('NO'),AT(54,102),USE(?TYP:CertifedPersonnel:Radio2),VALUE('N')
        END
        OPTION('STUDENT TYPE '),AT(107,92,75,24),USE(TYP:StudentCategory),BOXED
            RADIO('YES'),AT(111,102),USE(?TYP:StudentCategory:Radio1),VALUE('Y')
            RADIO('NO'),AT(149,102),USE(?TYP:StudentCategory:Radio2),VALUE('N')
        END
        OPTION('EEOC REPORTABLE'),AT(197,92,89,24),USE(TYP:EEOCReportable),BOXED
            RADIO('YES'),AT(209,102),USE(?TYP:EEOCReportable:Radio1),VALUE('Y')
            RADIO('NO'),AT(243,102),USE(?TYP:EEOCReportable:Radio2),VALUE('N')
        END
        PROMPT('NORMAL HOURS/DAY:'),AT(9,127),USE(?TYP:NormalHrsPerDay:Prompt)
        ENTRY(@n4.2),AT(95,127,25,10),USE(TYP:NormalHrsPerDay),REQ
        PROMPT('NORMAL PAY GROUP:'),AT(9,143,79,10),USE(?TYP:PayGroup:Prompt)
        ENTRY(@n_2),AT(95,143,16,10),USE(TYP:PayGroup),TIP('When these person will be paid, e.g. Jul' & |
                'y to June, Sept. to Aug., etc.')
        BUTTON('...'),AT(115,143,10,10),USE(?CPCSLookupDisplay)
        STRING(@s40),AT(134,143,183,10),USE(TIME:Description)
        OPTION('INS. GROUPING FOR REPORTS'),AT(329,38,138,86),USE(TYP:BenefitGroup),BOXED
            RADIO('ADMINISTRATION'),AT(335,49),USE(?TYP:BenefitGroup:Radio1),VALUE('1')
            RADIO('CERTIFIED PERSONS'),AT(335,61),USE(?TYP:BenefitGroup:Radio2),VALUE('2')
            RADIO('NON-CERTIFIED PERSONS'),AT(335,73),USE(?TYP:BenefitGroup:Radio3),VALUE('3')
            RADIO('TRANSPORTATION PERSONS'),AT(335,84),USE(?TYP:BenefitGroup:Radio4),VALUE('4')
            RADIO('RETIREES/COBRA'),AT(335,96),USE(?TYP:BenefitGroup:Radio5),VALUE('5')
            RADIO('NOT REPORTABLE'),AT(335,108),USE(?TYP:BenefitGroup:Radio8),VALUE('9')
        END
        STRING('PLEASE DO NOT ENTER NEXT FISCAL YEAR''S RATES UNTIL AFTER THE START OF THE YEAR.'), |
                AT(32,158,415,10),USE(?String6),HIDE,CENTER,FONT(,,COLOR:Blue,FONT:regular,CHARSET:ANSI)
        GROUP('THIS FISCAL YEAR''S RATES'),AT(33,172,141,66),USE(?TrsDataThisYrGrp),BOXED
            STRING('T.R.S.'),AT(47,183,21,10),USE(?String3)
            STRING('T.H.I.S.'),AT(94,183),USE(?String4:5)
            STRING('T.H.I.S.'),AT(137,183),USE(?String4)
            STRING('ALL PAY'),AT(41,192),USE(?String10:2)
            STRING('OTHER PAY'),AT(129,192),USE(?String9:2)
            STRING('CONTRACT'),AT(85,192),USE(?String9:5)
            ENTRY(@n6.5B),AT(41,206,39,10),USE(TYP:BrdTrsRate[1])
            ENTRY(@n6.5B),AT(41,219,39,10),USE(TYP:EmpTrsRate[1])
            ENTRY(@n7.6b),AT(85,206,39,10),USE(TYP:BrdTrsInsRate[1])
            ENTRY(@n7.6b),AT(85,219,39,10),USE(TYP:EmpTrsInsRate[1])
            ENTRY(@n7.6B),AT(131,206,39,10),USE(TYP:BrdTrsInsRateOtherPay[1])
            ENTRY(@n7.6B),AT(131,219,39,10),USE(TYP:EmpTrsInsRateOtherPay[1])
        END
        GROUP('NEXT FISCAL YEAR''S RATES'),AT(180,172,141,66),USE(?TrsDataNextYrGrp),BOXED
            STRING('T.R.S.'),AT(188,183,21,10),USE(?String3:2)
            STRING('T.H.I.S.'),AT(282,183,25,10),USE(?String4:2)
            STRING('T.H.I.S.'),AT(236,183,25,10),USE(?String4:6)
            STRING('ALL PAY'),AT(184,192,29,10),USE(?String10:3)
            STRING('OTHER PAY'),AT(274,192,41,10),USE(?String9:3)
            STRING('CONTRACT'),AT(229,192,39,10),USE(?String9:6)
            ENTRY(@n6.5B),AT(184,206,39,10),USE(TYP:BrdTrsRate[2])
            ENTRY(@n6.5B),AT(184,220,39,10),USE(TYP:EmpTrsRate[2])
            ENTRY(@n7.6b),AT(229,206,39,10),USE(TYP:BrdTrsInsRate[2])
            ENTRY(@n7.6b),AT(229,220,39,10),USE(TYP:EmpTrsInsRate[2])
            ENTRY(@n7.6B),AT(278,206,39,10),USE(TYP:BrdTrsInsRateOtherPay[2])
            ENTRY(@n7.6B),AT(278,220,39,10),USE(TYP:EmpTrsInsRateOtherPay[2])
        END
        GROUP('LAST FISCAL YEAR''S RATES'),AT(326,172,141,66),USE(?TrsDataLastYrGrp),BOXED
            STRING('T.R.S.'),AT(336,183,21,10),USE(?String3:3)
            STRING('T.H.I.S.'),AT(382,183,25,10),USE(?String4:4)
            STRING('T.H.I.S.'),AT(429,183,25,10),USE(?String4:3)
            STRING('ALL PAY'),AT(334,192),USE(?String10)
            STRING('OTHER PAY'),AT(421,192),USE(?String9)
            STRING('CONTRACT'),AT(374,192),USE(?String9:4)
            ENTRY(@n6.5B),AT(330,206,39,10),USE(TYP:BrdTrsRate[3])
            ENTRY(@n6.5B),AT(330,219,39,10),USE(TYP:EmpTrsRate[3])
            ENTRY(@n7.6b),AT(376,206,39,10),USE(TYP:BrdTrsInsRate[3])
            ENTRY(@n7.6b),AT(376,219,39,10),USE(TYP:EmpTrsInsRate[3])
            ENTRY(@n7.6B),AT(422,206,39,10),USE(TYP:BrdTrsInsRateOtherPay[3])
            ENTRY(@n7.6B),AT(422,219,39,10),USE(TYP:EmpTrsInsRateOtherPay[3])
        END
        PROMPT('BRD.:'),AT(9,207),USE(?TYP:BrdTrsRate:Prompt:4)
        BUTTON('OK'),AT(192,244,45,14),USE(?OK),DEFAULT,REQ
        BUTTON('Cancel'),AT(241,244,45,14),USE(?Cancel)
        PROMPT('EMP.:'),AT(9,217),USE(?TYP:EmpTrsRate:Prompt:4)
        PROMPT('I know all UPPER is UGLY. I didn''t make this.'),AT(163,127),USE(?PROMPT1)
    END
Ndx SHORT                     
CbWndPrv &CBWndPreviewClass  !In live code I implement with a Hot key CtrlShiftW that NEWs the Object so it ca nhave no effect
  CODE
    DO LoadFileRtn
    OPEN(Window)  
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow
        END
        IF EVENT()=EVENT:AlertKey AND KEYCODE()=CtrlShiftW THEN 
           IF CbWndPrv &= NULL THEN 
              CbWndPrv &= NEW(CBWndPreviewClass)
              CbWndPrv.Init(2)  !2=Top wide button
           END !ELSE
              CbWndPrv.Reflection()     !Display it on CtrlShiftW
           !END
        END

    END
    CLOSE(Window) 
    IF NOT CbWndPrv &= NULL THEN DISPOSE(CbWndPrv). !Not absolutely required, may be small leak but its just for debug    
    RETURN
LoadFileRtn ROUTINE
    Ndx=RANDOM(1,99)
         TYP:Code = Ndx
         TYP:Desc = 'Code #' & Ndx
         TYP:NormalHrsPerDay = 7.5
         TYP:CertifiedPersonnel = CHOOSE(Ndx % 2 + 1,'Y','N')
         TYP:TrsJobCategory=CHOOSE(Ndx%6+1,'T','A','C','O','S','N')
         TYP:StudentCategory = CHOOSE(Ndx % 2 + 1,'N','Y') 
         TYP:PayGroup = Ndx %5 + 3
         TYP:PayGroup = Ndx %5 + 3
         TYP:BrdTrsRate[1] = .01
         TYP:EmpTrsRate[1] = .02
         TYP:BrdTrsInsRate[1] = .03
         TYP:EmpTrsInsRate[1] = .04
         TYP:BrdTrsRate[2] = .05
         TYP:EmpTrsRate[2] = .06
         TYP:BrdTrsInsRate[2] = .07
         TYP:EmpTrsInsRate[2] = .08
         TYP:EEOCReportable = CHOOSE(Ndx % 2 + 1,'Y','N')
         TIME:Description='24 Pays'
         TYP:TrsJobCategory=CHOOSE(Ndx%6+1,'T','A','C','O','S','N')
         TYP:BenefitGroup=Ndx%6+1 ; IF TYP:BenefitGroup=6 THEN TYP:BenefitGroup=9.

    