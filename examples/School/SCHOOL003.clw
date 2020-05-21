

   MEMBER('SCHOOL.clw')                                    ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
UpdateGrades PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
CurrentTab           STRING(80)                            ! 
RecordFiltered       LONG                                  ! 
FullName             STRING(25)                            ! 
EditColumn           BYTE                                  ! 
DummyColumn          STRING(1)                             ! 
BRW1::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
                       PROJECT(ENR:ClassNumber)
                       PROJECT(ENR:StudentNumber)
                       JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:ClassNumber)
                         PROJECT(CLA:CourseNumber)
                         JOIN(COU:KeyNumber,CLA:CourseNumber)
                           PROJECT(COU:Description)
                           PROJECT(COU:Number)
                         END
                       END
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:Number)
                       END
                     END
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
COU:Description_NormalFG LONG                         !Normal forground color
COU:Description_NormalBG LONG                         !Normal background color
COU:Description_SelectedFG LONG                       !Selected forground color
COU:Description_SelectedBG LONG                       !Selected background color
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
CLA:ScheduledTime_NormalFG LONG                       !Normal forground color
CLA:ScheduledTime_NormalBG LONG                       !Normal background color
CLA:ScheduledTime_SelectedFG LONG                     !Selected forground color
CLA:ScheduledTime_SelectedBG LONG                     !Selected background color
FullName               LIKE(FullName)                 !List box control field - type derived from local data
FullName_NormalFG      LONG                           !Normal forground color
FullName_NormalBG      LONG                           !Normal background color
FullName_SelectedFG    LONG                           !Selected forground color
FullName_SelectedBG    LONG                           !Selected background color
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
DummyColumn            LIKE(DummyColumn)              !List box control field - type derived from local data
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !Browse hot field - type derived from field
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !Browse key field - type derived from field
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
STU:Number             LIKE(STU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('Update Grades for a Class'),AT(,,288,140),CENTER,GRAY,MDI,HLP('~UpdateGrades'),SYSTEM
                       LIST,AT(5,5,275,116),USE(?List),HVSCROLL,COLUMN,FORMAT('67L(1)|_M*~Class~@S30@44L(1)|_M' & |
  '*~Time~@s20@51L(1)|_M*~Student~@s25@33R(8)|_M~Midterm~L(1)@n3@25R(8)|_M~Final~L(1)@n' & |
  '3@41R(8)_~Term Paper~L(1)@n3@4R(2)_@s1@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Change'),AT(75,70,42,12),USE(?Change),HIDE
                       BUTTON('Close'),AT(240,125,40,12),USE(?Close)
                       ENTRY(@n3),AT(128,126),USE(?EditEntry),RIGHT(7),HIDE
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
SetQueueRecord         PROCEDURE(),DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::EIPManager     BrowseEIPManager                      ! Browse EIP Manager for Browse using ?List

  CODE
? DEBUGHOOK(Enrollment:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateGrades')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('FullName',FullName)                                ! Added by: BrowseBox(ABC)
  BIND('DummyColumn',DummyColumn)                          ! Added by: BrowseBox(ABC)
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open()                                 ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?List,Queue:Browse,'Queue:Browse') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowNumeric)     ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,ENR:SeqStu)      ! Add the sort order for ENR:SeqStu for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,ENR:ClassNumber,1,BRW1)        ! Initialize the browse locator using  using key: ENR:SeqStu , ENR:ClassNumber
  BRW1.SetFilter('(CLA:ClassNumber <<> 0)')                ! Apply filter expression to browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ScheduledTime,BRW1.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW1.AddField(FullName,BRW1.Q.FullName)                  ! Field FullName is a hot field or requires assignment from browse
  BRW1.AddField(ENR:MidtermExam,BRW1.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:FinalExam,BRW1.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:TermPaper,BRW1.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW1.AddField(DummyColumn,BRW1.Q.DummyColumn)            ! Field DummyColumn is a hot field or requires assignment from browse
  BRW1.AddField(ENR:ClassNumber,BRW1.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(ENR:StudentNumber,BRW1.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ClassNumber,BRW1.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Enrollment.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UGrades
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  SELF.EIP &= BRW1::EIPManager                             ! Set the EIP manager
  SELF.AddEditControl(,1)
  SELF.AddEditControl(,6)
  SELF.AddEditControl(,11)
  SELF.AddEditControl(,19)
  SELF.DeleteAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.ChangeControl=?Change
  END


BRW1.SetQueueRecord PROCEDURE

  CODE
  FullName = CLIP(STU:LastName) & ', ' & STU:FirstName
  PARENT.SetQueueRecord
  
  SELF.Q.COU:Description_NormalFG = -2147483645            ! Set color values for COU:Description
  SELF.Q.COU:Description_NormalBG = -1
  SELF.Q.COU:Description_SelectedFG = -2147483645
  SELF.Q.COU:Description_SelectedBG = -1
  SELF.Q.CLA:ScheduledTime_NormalFG = -2147483645          ! Set color values for CLA:ScheduledTime
  SELF.Q.CLA:ScheduledTime_NormalBG = -1
  SELF.Q.CLA:ScheduledTime_SelectedFG = -2147483645
  SELF.Q.CLA:ScheduledTime_SelectedBG = -1
  SELF.Q.FullName_NormalFG = -2147483645                   ! Set color values for FullName
  SELF.Q.FullName_NormalBG = -1
  SELF.Q.FullName_SelectedFG = -2147483645
  SELF.Q.FullName_SelectedBG = -1
  SELF.Q.FullName = FullName                               !Assign formula result to display queue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
CourseEnrollment PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
FinalLetterGrade     STRING(1)                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                           PROJECT(ENR:FinalExam)
                           PROJECT(ENR:MidtermExam)
                           PROJECT(ENR:StudentNumber)
                           PROJECT(ENR:TermPaper)
                           JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                             PROJECT(STU:LastName)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,1000,6500,7833),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
REPORTBrk              BREAK(LocalRequest),USE(?BREAK1)
                         HEADER,AT(0,0),USE(?GROUPHEADER1),ALONE
                           STRING('Enrollment Report'),AT(1948,2552,2958,406),USE(?STRING1),FONT('Times New Roman',20, |
  COLOR:Black,FONT:bold+FONT:italic),CENTER
                         END
DescriptionBREAK         BREAK(COU:Description),USE(?BREAK2)
                           HEADER,AT(0,0,,833),USE(?GROUPHEADER2),FONT('Arial',10,COLOR:Black,FONT:bold),PAGEBEFORE(-1)
                             STRING(@s40),AT(0,0),USE(COU:Description),FONT(,18,COLOR:Black)
                           END
ClassBREAK                 BREAK(CLA:ClassNumber),USE(?BREAK3)
                             HEADER,AT(0,0,,1313),USE(?GROUPHEADER3),FONT('Arial',10,COLOR:Black,FONT:bold)
                               GROUP('Class'),AT(1010,94,1594,760),USE(?GROUP1),BOXED
                                 STRING(@n4),AT(1844,323),USE(CLA:RoomNumber)
                                 STRING('Room'),AT(1156,323),USE(?String25)
                                 STRING(@s20),AT(1156,563,1094),USE(CLA:ScheduledTime)
                               END
                               GROUP('Teacher'),AT(4458,94,1594,760),USE(?GROUP2),BOXED
                                 STRING(@s20),AT(4521,563),USE(TEA:LastName)
                                 STRING(@s20),AT(4521,323),USE(TEA:FirstName)
                               END
                               LINE,AT(1021,1219,5500,0),USE(?LINE1)
                               STRING('Midterm Exam'),AT(2323,958),USE(?STRING2)
                               STRING('Final Exam'),AT(3479,958),USE(?STRING3)
                               STRING('Term Paper'),AT(4427,958),USE(?STRING4)
                               STRING('Final Grade'),AT(5417,958),USE(?STRING5)
                               STRING('Student Number'),AT(1063,958),USE(?STRING6)
                             END
Detail1                      DETAIL,AT(0,0,,250),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                               STRING(@n-13),AT(1052,0,1073),USE(ENR:StudentNumber)
                               STRING(@n-6),AT(2323,0,958),USE(ENR:MidtermExam)
                               STRING(@n-6),AT(3479,0,750),USE(ENR:FinalExam)
                               STRING(@n-6),AT(4427,0,781),USE(ENR:TermPaper)
                               STRING(@s20),AT(31,0,948),USE(STU:LastName)
                               STRING(@n5.2),AT(5417,0,740),USE(FinalGrade)
                               STRING(@s1),AT(5938,10),USE(FinalLetterGrade)
                               LINE,AT(10,208,6490,0),USE(?LINE2)
                             END
                             FOOTER,AT(0,0,,458),USE(?GROUPFOOTER1),WITHPRIOR(1)
                               STRING(@n5),AT(156,125),USE(Cnt_StudentNumber_2),CNT,RESET(ClassBREAK)
                               STRING('Students Enrolled in this Class'),AT(708,125),USE(?STRING7)
                             END
                           END
                           FOOTER,AT(0,0,,448),USE(?GROUPFOOTER2),WITHPRIOR(1)
                             STRING(@n5),AT(156,104),USE(Cnt_StudentNumber),CNT,RESET(DescriptionBREAK)
                             STRING('Students Enrolled in this Course'),AT(708,104),USE(?STRING8)
                           END
                         END
                       END
                       FOOTER,AT(1000,8833,,583),USE(?FOOTER1),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292),USE(?STRING9)
                         STRING(@n5),AT(6115,292),USE(?STRING10),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Courses:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('CourseEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, ProgressMgr, COU:Description)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FinalGrade = (ENR:MidtermExam + (ENR:FinalExam * 2) + ENR:TermPaper) / 4
  CASE (INT(FinalGrade / 10))
  OF 9
    FinalLetterGrade = 'A'
  OF 8
    FinalLetterGrade = 'B'
  OF 7
    FinalLetterGrade = 'C'
  OF 6
    FinalLetterGrade = 'D'
  ELSE
    FinalLetterGrade = 'F'
  END
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AttendanceSheets PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                           PROJECT(ENR:StudentNumber)
                           JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                             PROJECT(STU:FirstName)
                             PROJECT(STU:LastName)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1010,1417,6500,7438),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(990,1000,6500,406)
                         STRING('Attendance'),AT(2448,52),USE(?String17),FONT(,18,COLOR:Black,FONT:italic)
                       END
DescriptionBREAK       BREAK(COU:Description)
ClassBREAK               BREAK(CLA:ClassNumber)
                           HEADER,AT(0,0,,1594),FONT('Arial',10,COLOR:Black,FONT:bold)
                             STRING(@s40),AT(10,10),USE(COU:Description),FONT(,18,COLOR:Black)
                             GROUP('Class'),AT(1010,333,1594,760),BOXED
                               STRING(@n4),AT(1844,563),USE(CLA:RoomNumber)
                               STRING('Room'),AT(1156,563),USE(?String25)
                               STRING(@s20),AT(1156,802,1094),USE(CLA:ScheduledTime)
                             END
                             GROUP('Teacher'),AT(4458,333,1594,760),BOXED
                               STRING(@s20),AT(4521,802),USE(TEA:LastName)
                               STRING(@s20),AT(4521,563),USE(TEA:FirstName)
                             END
                             LINE,AT(1021,1479,5500,0)
                             STRING('Student Number'),AT(3531,1198)
                             STRING('Last Name'),AT(1063,1208),USE(?String15)
                             STRING('First Name'),AT(2188,1208),USE(?String16)
                           END
Detail1                    DETAIL,AT(,,,250),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                             STRING(@P###-##-####P),AT(3656,21),USE(ENR:StudentNumber)
                             STRING(@S20),AT(2094,10),USE(STU:FirstName)
                             STRING(@s20),AT(1042,10,948),USE(STU:LastName)
                           END
                           FOOTER,AT(0,0,,240),PAGEAFTER(-1),WITHPRIOR(1)
                           END
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Courses:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AttendanceSheets')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, 3000)
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Student Schedules
!!! </summary>
ClassSchedules1 PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Enrollment)
                       PROJECT(ENR:ClassNumber)
                       PROJECT(ENR:StudentNumber)
                       JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:CourseNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                         END
                         JOIN(COU:KeyNumber,CLA:CourseNumber)
                           PROJECT(COU:Description)
                         END
                       END
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:FirstName)
                         PROJECT(STU:LastName)
                         PROJECT(STU:Number)
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

report               REPORT,AT(1000,1552,6000,7448),PRE(RPT),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6000,552)
                         STRING('Student Schedule'),AT(0,281,6000,220),FONT(,,COLOR:Black,FONT:bold),CENTER
                       END
ENR:StudentNumberBreak BREAK(ENR:StudentNumber)
                         HEADER,AT(0,0,5990,677)
                           STRING(@P###-##-####P),AT(3406,63),USE(STU:Number),FONT(,,COLOR:Black,FONT:bold),RIGHT(1)
                           STRING('Room'),AT(2875,292),USE(?String9),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Time'),AT(3688,292),USE(?String10),FONT(,,COLOR:Black,FONT:bold)
                           LINE,AT(50,560,5900,0),USE(?Line2),COLOR(COLOR:Black)
                           STRING('Class'),AT(479,292),USE(?String11),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(83,63),USE(STU:FirstName),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(1760,63),USE(STU:LastName),FONT(,,COLOR:Black,FONT:bold)
                         END
detail                   DETAIL,AT(-10,10,6000,302),USE(?detail)
                           STRING(@S30),AT(250,42),USE(COU:Description)
                           STRING(@n4),AT(2875,42),USE(CLA:RoomNumber)
                           STRING(@s20),AT(3688,42),USE(CLA:ScheduledTime)
                         END
                         FOOTER,AT(0,0,,323),PAGEAFTER(-1)
                         END
                       END
                       FOOTER,AT(1000,9000,6000,219)
                         STRING(@pPage <<<#p),AT(5250,30,700,135),USE(?PageCount),FONT('Arial',8,COLOR:Black,FONT:regular), |
  PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepLongClass                         ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Enrollment:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('ClassSchedules1')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open()                                 ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowNumeric,)
  ThisReport.Init(Process:View, Relate:Enrollment, ?Progress:PctText, Progress:Thermometer, ProgressMgr, ENR:StudentNumber)
  ThisReport.AddSortOrder(ENR:StuSeq)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Enrollment.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Enrollment.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
EnrollSummary PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,1104,6500,7729),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,490,6500,615)
                         STRING('Enrollment Summary Report'),AT(1760,250),USE(?String11),FONT(,18,COLOR:Black,FONT:italic)
                       END
DescriptionBREAK       BREAK(COU:Description)
                         HEADER,AT(0,0,,354),FONT('Arial',10,COLOR:Black,FONT:bold)
                           STRING(@s40),AT(0,0),USE(COU:Description),FONT(,18,COLOR:Black)
                         END
ClassBREAK               BREAK(CLA:ClassNumber)
                           HEADER,AT(0,0,,792),FONT('Arial',10,COLOR:Black,FONT:bold)
                             GROUP('Class'),AT(115,94,2146,552),BOXED
                               STRING(@n4),AT(604,313),USE(CLA:RoomNumber)
                               STRING('Room'),AT(156,313),USE(?String25)
                               STRING(@s20),AT(1031,313,1094),USE(CLA:ScheduledTime)
                             END
                             GROUP('Teacher'),AT(2510,125,3240,510),BOXED
                               STRING(@s20),AT(4125,313),USE(TEA:LastName)
                               STRING(@s20),AT(2573,313),USE(TEA:FirstName)
                             END
                           END
Detail1                    DETAIL,AT(,,,0),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                           END
                           FOOTER,AT(0,0,,458),WITHPRIOR(1)
                             STRING(@n5),AT(156,125),USE(Cnt_StudentNumber_2),CNT,RESET(ClassBREAK)
                             STRING('Students Enrolled in this Class'),AT(708,125)
                           END
                         END
                         FOOTER,AT(0,0,,448),PAGEAFTER(-1),WITHPRIOR(1)
                           LINE,AT(63,63,3156,0),USE(?Line1),COLOR(COLOR:Black)
                           STRING(@n5),AT(156,104),USE(Cnt_StudentNumber),CNT,RESET(DescriptionBREAK)
                           STRING('Students Enrolled in this Course'),AT(708,104)
                         END
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Courses:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('EnrollSummary')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, ProgressMgr, COU:Description)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
FinalGrades PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
FinalLetterGrade     STRING(1)                             ! 
FullName             STRING(35)                            ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Students)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Number)
                       JOIN(ENR:StuSeq,STU:Number)
                         PROJECT(ENR:FinalExam)
                         PROJECT(ENR:MidtermExam)
                         PROJECT(ENR:StudentNumber)
                         PROJECT(ENR:TermPaper)
                         PROJECT(ENR:ClassNumber)
                         JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                           PROJECT(CLA:TeacherNumber)
                           PROJECT(CLA:CourseNumber)
                           JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                             PROJECT(TEA:LastName)
                           END
                           JOIN(COU:KeyNumber,CLA:CourseNumber)
                             PROJECT(COU:Description)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2010,6500,6823),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6500,1000)
                         STRING('TopSpeed University'),AT(2010,156),USE(?String19),FONT(,18,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Final Grade Report'),AT(2344,604),USE(?String20),FONT(,14,COLOR:Black,FONT:bold+FONT:underline)
                       END
STU:NumberBreak        BREAK(STU:Number)
                         HEADER,AT(0,0,,802)
                           STRING(@s35),AT(1146,94),USE(FullName),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Student:'),AT(479,94),USE(?String23),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@P###-##-####P),AT(1146,281,823,208),USE(ENR:StudentNumber),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Term Paper'),AT(4667,583),USE(?String13),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final Grade'),AT(5625,583),USE(?String16),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Class'),AT(83,583),USE(?String17),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Instructor'),AT(2010,573),USE(?String18),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Mid-Term'),AT(3521,583),USE(?String15),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final'),AT(4208,583),USE(?String14),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                         END
Detail1                  DETAIL,AT(10,,6490,260),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                           STRING(@n-6),AT(3667,42,365,177),USE(ENR:MidtermExam)
                           STRING(@n-6),AT(4188,42,438,177),USE(ENR:FinalExam)
                           STRING(@n-6),AT(4990,42,365,177),USE(ENR:TermPaper)
                           STRING(@n5.2),AT(5677,42,313,177),USE(FinalGrade),RIGHT
                           STRING(@s1),AT(6115,42),USE(FinalLetterGrade)
                           STRING(@S30),AT(73,42),USE(COU:Description)
                           STRING(@S20),AT(2021,42),USE(TEA:LastName)
                         END
                         FOOTER,AT(0,0,,448),FONT('Arial',8,COLOR:Black),PAGEAFTER(-1)
                           LINE,AT(5510,31,1042,0),USE(?Line1),COLOR(COLOR:Black)
                           STRING(@n5.2),AT(5667,135),USE(FinalGrade,,?FinalGrade:2),RIGHT,AVE,RESET(STU:NumberBreak)
                           STRING('Overall GPA:'),AT(4656,135),USE(?String22),FONT(,,COLOR:Black,FONT:bold)
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Students:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('FinalGrades')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Students.SetOpenRelated()
  Relate:Students.Open()                                   ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Students, ?Progress:PctText, Progress:Thermometer, ProgressMgr, STU:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(STU:KeyLastName)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Students.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FinalGrade = (ENR:MidtermExam + (ENR:FinalExam * 2) + ENR:TermPaper) / 4
  CASE (INT(FinalGrade / 10))
  OF 9
    FinalLetterGrade = 'A'
  OF 8
    FinalLetterGrade = 'B'
  OF 7
    FinalLetterGrade = 'C'
  OF 6
    FinalLetterGrade = 'D'
  ELSE
    FinalLetterGrade = 'F'
  END
  FullName = CLIP(STU:FirstName) & ' ' & STU:LastName
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
StudentIDs PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
FilesOpened          BYTE                                  ! 
FullName             STRING(35)                            ! 
CSZ                  STRING(35)                            ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Students)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Number)
                       PROJECT(STU:Photograph)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(0,0,8500,11000),PRE(RPT),FONT('Arial',10,COLOR:Black,FONT:regular),THOUS
detail                 DETAIL,AT(,,4250,2198),USE(?detail)
                         BOX,AT(219,229,3729,1719),USE(?Box1),COLOR(COLOR:Black),LINEWIDTH(30),ROUND
                         STRING('TopSpeed University'),AT(448,333),USE(?String1),FONT(,12,COLOR:Black,FONT:bold+FONT:italic)
                         IMAGE,AT(2719,385),USE(?Image1)
                         STRING(@S20),AT(448,1219),USE(STU:Address)
                         STRING(@s20),AT(448,1385),USE(STU:Address2)
                         STRING('Student Identification'),AT(448,531),USE(?String2),FONT(,,COLOR:Black,FONT:bold)
                         STRING(@s35),AT(448,792),USE(FullName),TRN
                         STRING(@P###-##-####P),AT(448,969),USE(STU:Number),LEFT(1)
                         STRING(@s35),AT(448,1563),USE(CSZ),TRN
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Students:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('StudentIDs')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Students.SetOpenRelated()
  Relate:Students.Open()                                   ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Students, ?Progress:PctText, Progress:Thermometer, ProgressMgr, STU:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(STU:KeyLastName)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Students.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FullName = CLIP(STU:FirstName) & ' ' & STU:LastName
  CSZ = CLIP(STU:City) & ', ' & STU:State & ' ' & STU:Zip
  !Assign BLOB to IMAGE control
  Report$?Image1{PROP:NoWidth} = TRUE
  Report$?Image1{PROP:NoHeight} = TRUE
  Report$?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
  IF Report$?Image1{PROP:Height} > 1000
    AspectRatio$ = Report$?Image1{PROP:Width}/Report$?Image1{PROP:Height}
    Report$?Image1{PROP:Height} = 1000
    Report$?Image1{PROP:Width} = 1000 * AspectRatio$
  END
  IF Report$?Image1{PROP:Width} > 1000
    AspectRatio$ = Report$?Image1{PROP:Height}/Report$?Image1{PROP:Width}
    Report$?Image1{PROP:Width} = 1000
    Report$?Image1{PROP:Height} = 1000 * AspectRatio$
  END
   
  
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Teacher Schedules
!!! </summary>
ClassSchedules2 PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Teachers)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:Number)
                       JOIN(CLA:KeyTeacherNumber,TEA:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:CourseNumber)
                         JOIN(COU:KeyNumber,CLA:CourseNumber)
                           PROJECT(COU:Description)
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

report               REPORT,AT(1000,1552,6000,7448),PRE(RPT),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6000,552)
                         STRING('Teacher''s Schedule'),AT(0,281,6000,220),FONT(,,COLOR:Black,FONT:bold),CENTER
                       END
Tea:NumberBreak        BREAK(TEA:Number)
                         HEADER,AT(0,0,,823)
                           STRING(@S20),AT(125,115),USE(TEA:FirstName)
                           STRING(@S20),AT(1802,135),USE(TEA:LastName)
                           STRING(@P###-##-####P),AT(3583,156),USE(TEA:Number),RIGHT(1)
                           STRING('Class'),AT(385,490),USE(?String9),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Time'),AT(3708,490),USE(?String11),FONT(,,COLOR:Black,FONT:bold)
                           LINE,AT(167,708,5563,0),COLOR(COLOR:Black)
                           STRING('Room'),AT(2802,490),USE(?String10),FONT(,,COLOR:Black,FONT:bold)
                         END
detail                   DETAIL,AT(-10,10,6000,302),USE(?detail)
                           STRING(@S30),AT(250,42),USE(COU:Description)
                           STRING(@n4),AT(2875,42),USE(CLA:RoomNumber)
                           STRING(@s20),AT(3688,42),USE(CLA:ScheduledTime)
                         END
                         FOOTER,AT(0,0,,208),PAGEAFTER(-1)
                         END
                       END
                       FOOTER,AT(1000,9000,6000,219)
                         STRING(@pPage <<<#p),AT(5250,30,700,135),USE(?PageCount),FONT('Arial',8,COLOR:Black,FONT:regular), |
  PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
? DEBUGHOOK(Teachers:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('ClassSchedules2')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Teachers.SetOpenRelated()
  Relate:Teachers.Open()                                   ! File Teachers used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Teachers, ?Progress:PctText, Progress:Thermometer, ProgressMgr, TEA:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(TEA:KeyLastName)
  ThisReport.SetFilter('CLA:TeacherNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Teachers.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Teachers.Close()
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SplashIt PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
window               WINDOW,AT(,,209,109),FONT('MS Sans Serif',8,COLOR:Black,FONT:regular),NOFRAME,CENTER,GRAY, |
  MDI,PALETTE(256)
                       PANEL,AT(0,0,209,108),BEVEL(6)
                       PANEL,AT(7,6,195,95),BEVEL(-2,1)
                       STRING('SoftVelocity University'),AT(11,18,188,20),USE(?String2),FONT('Arial',18,COLOR:Red, |
  FONT:bold+FONT:italic),CENTER
                       PANEL,AT(13,41,182,12),BEVEL(-1,1,9)
                       STRING('SoftVelocity, Inc. '),AT(11,72,188,18),USE(?String3),FONT('Arial',14,,FONT:bold),CENTER
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('SplashIt')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?String2
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
  TARGET{Prop:Timer} = 1000                                ! Close window on timer event, so configure timer
  TARGET{Prop:Alrt,255} = MouseLeft                        ! Alert mouse clicks that will close window
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)                            ! Splash window will close on mouse click
      END
    OF EVENT:LoseFocus
        POST(Event:CloseWindow)                            ! Splash window will close when focus is lost
    OF Event:Timer
      POST(Event:CloseWindow)                              ! Splash window will close on event timer
    OF Event:AlertKey
      CASE KEYCODE()                                       ! Splash window will close on mouse click
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

