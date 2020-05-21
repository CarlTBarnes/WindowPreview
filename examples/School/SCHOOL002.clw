

   MEMBER('SCHOOL.clw')                                    ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Courses File
!!! </summary>
BrowseCourses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Courses)
                       PROJECT(COU:Number)
                       PROJECT(COU:Description)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
COU:Number             LIKE(COU:Number)               !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse Courses - No HLP()'),AT(,,159,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,SYSTEM
                       LIST,AT(8,20,143,144),USE(?Browse:1),HVSCROLL,FORMAT('32R(1)|M~Number~L(2)@n4@80L(2)|M~' & |
  'Description~@S30@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(8,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(57,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(106,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,151,162),USE(?CurrentTab)
                         TAB('by Course Description')
                         END
                       END
                       BUTTON('Close'),AT(61,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(110,170,45,14),USE(?Help),MSG('HLP from Window ~BrowseCourses'),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


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
  GlobalErrors.SetProcedureName('BrowseCourses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Courses,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Alpha) ! Moveable thumb based upon COU:Description for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,COU:KeyDescription) ! Add the sort order for COU:KeyDescription for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,COU:Description,1,BRW1)        ! Initialize the browse locator using  using key: COU:KeyDescription , COU:Description
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateCourses
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
    Relate:Courses.Close()
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
    UpdateCourses
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Enrollment File
!!! </summary>
BrowseEnrollment PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:StudentNumber)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
                       PROJECT(ENR:ClassNumber)
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:LastName)
                         PROJECT(STU:FirstName)
                         PROJECT(STU:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !List box control field - type derived from field
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !Browse key field - type derived from field
STU:Number             LIKE(STU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Enrollment File'),AT(,,260,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,HLP('~BrowseEnrollment'),SYSTEM
                       LIST,AT(8,20,244,142),USE(?Browse:1),HVSCROLL,FORMAT('[60L(2)|M~Student Number~@p###-##' & |
  '-####p@80L(2)|M~Last Name~@S20@80L(2)|M~First Name~@S20@/60C(2)M~Midterm Exam~C(0)@n' & |
  '3@79C(2)M~Final Exam~C(0)@n3@44C(2)|M~Term Paper~C(0)@n3@]|M'),FROM(Queue:Browse:1),IMM, |
  MSG('Browsing Records')
                       BUTTON('&Insert'),AT(109,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(158,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(207,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,252,162),USE(?CurrentTab)
                         TAB('by Student Number')
                         END
                         TAB('by Class Number')
                         END
                       END
                       BUTTON('Close'),AT(162,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(211,170,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Enrollment:Record)
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
  GlobalErrors.SetProcedureName('BrowseEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  Access:Students.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,ENR:SeqStu)      ! Add the sort order for ENR:SeqStu for sort order 1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:StudentNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,ENR:StuSeq)      ! Add the sort order for ENR:StuSeq for sort order 2
  BRW1.AddField(ENR:StudentNumber,BRW1.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW1.AddField(STU:LastName,BRW1.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW1.AddField(STU:FirstName,BRW1.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(ENR:MidtermExam,BRW1.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:FinalExam,BRW1.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:TermPaper,BRW1.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW1.AddField(ENR:ClassNumber,BRW1.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateEnrollment
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
    Relate:Classes.Close()
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
    UpdateEnrollment
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Teachers File
!!! </summary>
UpdateTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectMajorsThread   LONG                                  ! 
BRW2::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:TeacherNumber)
                       PROJECT(CLA:CourseNumber)
                       JOIN(COU:KeyNumber,CLA:CourseNumber)
                         PROJECT(COU:Description)
                         PROJECT(COU:Number)
                       END
                     END
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
CLA:RoomNumber         LIKE(CLA:RoomNumber)           !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
CLA:TeacherNumber      LIKE(CLA:TeacherNumber)        !Browse key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::TEA:Record  LIKE(TEA:RECORD),THREAD
QuickWindow          WINDOW('Update the Teachers File'),AT(,,159,177),FONT('MS Sans Serif',8,COLOR:Black),GRAY, |
  IMM,MDI,HLP('~UpdateTeachers'),SYSTEM
                       SHEET,AT(4,6,151,151),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&Number:'),AT(8,26),USE(?Tea:Number:Prompt)
                           ENTRY(@P###-##-####P),AT(53,26,,10),USE(TEA:Number),RIGHT(1)
                           PROMPT('&First Name:'),AT(8,39),USE(?Tea:FirstName:Prompt)
                           ENTRY(@S20),AT(53,39,84,10),USE(TEA:FirstName)
                           PROMPT('&Last Name:'),AT(8,52),USE(?Tea:LastName:Prompt)
                           ENTRY(@S20),AT(53,52,84,10),USE(TEA:LastName)
                           PROMPT('&Address:'),AT(8,66),USE(?Tea:Address:Prompt)
                           ENTRY(@S20),AT(53,66,84,10),USE(TEA:Address)
                           PROMPT('&City:'),AT(8,81),USE(?Tea:City:Prompt)
                           ENTRY(@S20),AT(53,81,84,10),USE(TEA:City)
                           PROMPT('&State:'),AT(8,95),USE(?Tea:State:Prompt)
                           ENTRY(@S2),AT(53,95,40,10),USE(TEA:State)
                           PROMPT('&Zip:'),AT(8,108),USE(?Tea:Zip:Prompt)
                           ENTRY(@n05),AT(53,108,40,10),USE(TEA:Zip)
                           PROMPT('&Telephone:'),AT(8,122),USE(?Tea:Telephone:Prompt)
                           ENTRY(@s12),AT(53,122,52,10),USE(TEA:Telephone)
                           PROMPT('Department:'),AT(8,137),USE(?Tea:Department:Prompt)
                           ENTRY(@n4),AT(53,137,21,10),USE(TEA:Department),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(77,137,12,12),USE(?Button7)
                           STRING(@S15),AT(90,137),USE(MAJ:Description)
                         END
                         TAB('Classes'),USE(?ClassesTab)
                           LIST,AT(8,26,143,124),USE(?Browse:2),HVSCROLL,FORMAT('52L(2)|M~Class Number~@P##-#####P' & |
  '@120L(2)|M~Description~@S30@48R(2)|M~Room Number~C(0)@n4@80L(2)|M~Scheduled Time~@s20@'), |
  FROM(Queue:Browse:2),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,119,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,119,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(106,119,45,14),USE(?Delete:3),HIDE
                         END
                       END
                       BUTTON('OK'),AT(12,159,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,159,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,159,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Majors:Record)
? DEBUGHOOK(Teachers:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Teachers Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Teachers Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateTeachers')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Tea:Number:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(TEA:Record,History::TEA:Record)
  SELF.AddHistoryField(?TEA:Number,1)
  SELF.AddHistoryField(?TEA:FirstName,2)
  SELF.AddHistoryField(?TEA:LastName,3)
  SELF.AddHistoryField(?TEA:Address,4)
  SELF.AddHistoryField(?TEA:City,5)
  SELF.AddHistoryField(?TEA:State,6)
  SELF.AddHistoryField(?TEA:Zip,7)
  SELF.AddHistoryField(?TEA:Telephone,8)
  SELF.AddHistoryField(?TEA:Department,9)
  SELF.AddUpdateFile(Access:Teachers)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Teachers
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:2,Queue:Browse:2,'Queue:Browse:2') !Tpl CBWndPrvListFromQ
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 1
  BRW2.AddRange(CLA:TeacherNumber,Relate:Classes,Relate:Teachers) ! Add file relationship range limit for sort order 1
  BRW2.AddField(CLA:ClassNumber,BRW2.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Description,BRW2.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW2.AddField(CLA:RoomNumber,BRW2.Q.CLA:RoomNumber)      ! Field CLA:RoomNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ScheduledTime,BRW2.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW2.AddField(CLA:TeacherNumber,BRW2.Q.CLA:TeacherNumber) ! Field CLA:TeacherNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Number,BRW2.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW2.AskProcedure = 1                                    ! Will call: UpdateClasses
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  MAJ:Number = TEA:Department                              ! Assign linking field value
  Access:Majors.Fetch(MAJ:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateClasses
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?Button7
      !Call drag toolbox
      POST(EVENT:AlertKey,?Tea:Department)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?TEA:Department
      IF Access:Teachers.TryValidateField(9)               ! Attempt to validate TEA:Department in Teachers
        SELECT(?TEA:Department)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?TEA:Department
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?TEA:Department{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?TEA:Department
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectMajorsThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?Tea:Department
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    OF EVENT:Drop
      !Receive dropped data and close drag toolbox
      Tea:Department = DROPID()
      DISPLAY
      POST(EVENT:CloseWindow,,SelectMajorsThread)
      SelectMajorsThread = 0
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?TEA:Department
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT Tea:Department
        GLO:DropThread = THREAD()
        GLO:DropControl = ?Tea:Department
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Majors Record
!!! </summary>
SelectMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
BRW1::View:Browse    VIEW(Majors)
                       PROJECT(MAJ:Description)
                       PROJECT(MAJ:Number)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Major'),AT(,,159,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,HLP('~SelectMajors'), |
  SYSTEM,TOOLBOX
                       LIST,AT(8,23,142,148),USE(?Browse:1),HVSCROLL,DRAGID('Majors'),FORMAT('80L(2)|M~Descrip' & |
  'tion~L(2)@S20@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,172),USE(?CurrentTab)
                         TAB('by Major Number'),USE(?TAB1)
                         END
                         TAB('by Major Description'),USE(?TAB2)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2

  CODE
? DEBUGHOOK(Majors:Record)
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
  GlobalErrors.SetProcedureName('SelectMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open()                                     ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Majors,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon MAJ:Description for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,MAJ:KeyDescription) ! Add the sort order for MAJ:KeyDescription for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,MAJ:Description,1,BRW1)        ! Initialize the browse locator using  using key: MAJ:KeyDescription , MAJ:Description
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon MAJ:Number for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,MAJ:KeyNumber)   ! Add the sort order for MAJ:KeyNumber for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,MAJ:Number,1,BRW1)             ! Initialize the browse locator using  using key: MAJ:KeyNumber , MAJ:Number
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
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
    Relate:Majors.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(MAJ:Number)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(MAJ:Number)
    POST(EVENT:Drop,GLO:DropControl,GLO:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Majors File
!!! </summary>
UpdateMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
BRW2::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:Address)
                       PROJECT(TEA:City)
                       PROJECT(TEA:State)
                       PROJECT(TEA:Zip)
                       PROJECT(TEA:Telephone)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                     END
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:Address            LIKE(TEA:Address)              !List box control field - type derived from field
TEA:City               LIKE(TEA:City)                 !List box control field - type derived from field
TEA:State              LIKE(TEA:State)                !List box control field - type derived from field
TEA:Zip                LIKE(TEA:Zip)                  !List box control field - type derived from field
TEA:Telephone          LIKE(TEA:Telephone)            !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BRW4::View:Browse    VIEW(Students)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                       PROJECT(STU:Telephone)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Number)
                       PROJECT(STU:Major)
                     END
Queue:Browse:4       QUEUE                            !Queue declaration for browse/combo box using ?Browse:4
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:Address            LIKE(STU:Address)              !List box control field - type derived from field
STU:Address2           LIKE(STU:Address2)             !List box control field - type derived from field
STU:City               LIKE(STU:City)                 !List box control field - type derived from field
STU:State              LIKE(STU:State)                !List box control field - type derived from field
STU:Zip                LIKE(STU:Zip)                  !List box control field - type derived from field
STU:Telephone          LIKE(STU:Telephone)            !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !Primary key field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::MAJ:Record  LIKE(MAJ:RECORD),THREAD
QuickWindow          WINDOW('Update the Majors File'),AT(,,159,138),FONT('MS Sans Serif',8,COLOR:Black),GRAY,IMM, |
  MDI,HLP('~UpdateMajors'),SYSTEM
                       SHEET,AT(4,4,151,112),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&Description:'),AT(8,20),USE(?MAJ:Description:Prompt)
                           ENTRY(@S20),AT(64,20,84,10),USE(MAJ:Description)
                         END
                         TAB('Teachers'),USE(?TeachersTab)
                           LIST,AT(8,20,143,94),USE(?Browse:2),HVSCROLL,FORMAT('80L(2)|M~First Name~L(2)@S20@80L(2' & |
  ')|M~Last Name~L(2)@S20@80L(2)|M~Address~L(2)@S20@80L(2)|M~City~L(2)@S20@24L(2)|M~Sta' & |
  'te~L(2)@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~L(2)@s12@'),FROM(Queue:Browse:2),IMM, |
  MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,98,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,98,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(106,98,45,14),USE(?Delete:3),HIDE
                         END
                         TAB('Students'),USE(?StudentsTab)
                           LIST,AT(8,20,143,94),USE(?Browse:4),HVSCROLL,FORMAT('80L(2)|M~First Name~L(2)@S20@80L(2' & |
  ')|M~Last Name~L(2)@S20@80L(2)|M~Address~L(2)@S20@80L(2)|M~Address 2~L(2)@s20@80L(2)|' & |
  'M~City~L(2)@S20@24L(2)|M~State~L(2)@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~L(2)' & |
  '@s12@40R(2)|M~Grad Year~C(0)@n4@'),FROM(Queue:Browse:4),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,98,45,14),USE(?Insert:5),HIDE
                           BUTTON('&Change'),AT(57,98,45,14),USE(?Change:5),HIDE
                           BUTTON('&Delete'),AT(106,98,45,14),USE(?Delete:5),HIDE
                         END
                       END
                       BUTTON('OK'),AT(12,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,120,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW4                 CLASS(BrowseClass)                    ! Browse using ?Browse:4
Q                      &Queue:Browse:4                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW4::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW4::Sort0:StepClass StepStringClass                      ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Majors:Record)
? DEBUGHOOK(Students:Record)
? DEBUGHOOK(Teachers:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Majors Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Majors Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MAJ:Description:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(MAJ:Record,History::MAJ:Record)
  SELF.AddHistoryField(?MAJ:Description,2)
  SELF.AddUpdateFile(Access:Majors)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open()                                     ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Majors
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Teachers,SELF) ! Initialize the browse manager
  BRW4.Init(?Browse:4,Queue:Browse:4.ViewPosition,BRW4::View:Browse,Queue:Browse:4,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:2,Queue:Browse:2,'Queue:Browse:2') !Tpl CBWndPrvListFromQ
   CBListPropFromQ(?Browse:4,Queue:Browse:4,'Queue:Browse:4') !Tpl CBWndPrvListFromQ
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW2.AddRange(TEA:Department,Relate:Teachers,Relate:Majors) ! Add file relationship range limit for sort order 1
  BRW2.AddField(TEA:FirstName,BRW2.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW2.AddField(TEA:LastName,BRW2.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Address,BRW2.Q.TEA:Address)            ! Field TEA:Address is a hot field or requires assignment from browse
  BRW2.AddField(TEA:City,BRW2.Q.TEA:City)                  ! Field TEA:City is a hot field or requires assignment from browse
  BRW2.AddField(TEA:State,BRW2.Q.TEA:State)                ! Field TEA:State is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Zip,BRW2.Q.TEA:Zip)                    ! Field TEA:Zip is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Telephone,BRW2.Q.TEA:Telephone)        ! Field TEA:Telephone is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Number,BRW2.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Department,BRW2.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
  BRW4.Q &= Queue:Browse:4
  BRW4::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon STU:LastName for sort order 1
  BRW4.AddSortOrder(BRW4::Sort0:StepClass,STU:MajorKey)    ! Add the sort order for STU:MajorKey for sort order 1
  BRW4.AddRange(STU:Major,Relate:Students,Relate:Majors)   ! Add file relationship range limit for sort order 1
  BRW4.AddLocator(BRW4::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW4::Sort0:Locator.Init(,STU:LastName,1,BRW4)           ! Initialize the browse locator using  using key: STU:MajorKey , STU:LastName
  BRW4.AddField(STU:FirstName,BRW4.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW4.AddField(STU:LastName,BRW4.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW4.AddField(STU:Address,BRW4.Q.STU:Address)            ! Field STU:Address is a hot field or requires assignment from browse
  BRW4.AddField(STU:Address2,BRW4.Q.STU:Address2)          ! Field STU:Address2 is a hot field or requires assignment from browse
  BRW4.AddField(STU:City,BRW4.Q.STU:City)                  ! Field STU:City is a hot field or requires assignment from browse
  BRW4.AddField(STU:State,BRW4.Q.STU:State)                ! Field STU:State is a hot field or requires assignment from browse
  BRW4.AddField(STU:Zip,BRW4.Q.STU:Zip)                    ! Field STU:Zip is a hot field or requires assignment from browse
  BRW4.AddField(STU:Telephone,BRW4.Q.STU:Telephone)        ! Field STU:Telephone is a hot field or requires assignment from browse
  BRW4.AddField(STU:GradYear,BRW4.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW4.AddField(STU:Number,BRW4.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW4.AddField(STU:Major,BRW4.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
  BRW2.AskProcedure = 1                                    ! Will call: UpdateTeachers
  BRW4.AskProcedure = 2                                    ! Will call: UpdateStudents
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
  BRW4.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW4.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Majors.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      UpdateTeachers
      UpdateStudents
    END
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END


BRW4.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:5
    SELF.ChangeControl=?Change:5
    SELF.DeleteControl=?Delete:5
  END

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Students File
!!! </summary>
UpdateStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
DOSDialogHeader      CSTRING(40)                           ! 
DOSExtParameter      CSTRING(250)                          ! 
DOSTargetVariable    CSTRING(80)                           ! 
SelectMajorsThread   LONG                                  ! 
PhotoChanged         BYTE                                  ! 
BRW2::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:ClassNumber)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
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
                     END
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !Browse key field - type derived from field
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::STU:Record  LIKE(STU:RECORD),THREAD
QuickWindow          WINDOW('Update the Students File'),AT(,,160,204),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~UpdateStudents'),PALETTE(256),SYSTEM
                       SHEET,AT(3,4,151,173),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&First Name:'),AT(7,20),USE(?STU:FirstName:Prompt)
                           ENTRY(@S20),AT(47,20,84,10),USE(STU:FirstName)
                           PROMPT('&Last Name:'),AT(7,34),USE(?STU:LastName:Prompt)
                           ENTRY(@S20),AT(47,34,84,10),USE(STU:LastName)
                           PROMPT('&Address:'),AT(7,48),USE(?STU:Address:Prompt)
                           ENTRY(@S20),AT(47,48,84,10),USE(STU:Address)
                           PROMPT('Address 2:'),AT(7,62),USE(?STU:Address2:Prompt)
                           ENTRY(@s20),AT(47,62,84,10),USE(STU:Address2)
                           PROMPT('&City:'),AT(7,76),USE(?STU:City:Prompt)
                           ENTRY(@S20),AT(47,76,84,10),USE(STU:City)
                           PROMPT('&State:'),AT(7,90),USE(?STU:State:Prompt)
                           ENTRY(@S2),AT(47,90,40,10),USE(STU:State)
                           PROMPT('&Zip:'),AT(7,104),USE(?STU:Zip:Prompt)
                           ENTRY(@n05),AT(47,104,40,10),USE(STU:Zip)
                           PROMPT('&Telephone:'),AT(7,118),USE(?STU:Telephone:Prompt)
                           ENTRY(@s12),AT(47,118,52,10),USE(STU:Telephone)
                           PROMPT('Grad Year:'),AT(7,132),USE(?STU:GradYear:Prompt)
                           ENTRY(@n4),AT(47,132,40,10),USE(STU:GradYear)
                           PROMPT('&Number:'),AT(7,146),USE(?STU:Number:Prompt)
                           ENTRY(@P###-##-####P),AT(47,146,,10),USE(STU:Number),RIGHT(1),REQ
                           STRING(@s20),AT(74,160,77,10),USE(MAJ:Description)
                           PROMPT('Major:'),AT(7,160),USE(?STU:Major:Prompt)
                           ENTRY(@n4),AT(47,160,13,10),USE(STU:Major),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(61,159,12,12),USE(?Button8)
                         END
                         TAB('Enrollment'),USE(?EnrollmentTab)
                           LIST,AT(7,22,143,148),USE(?Browse:2),HVSCROLL,FORMAT('[52L(2)|M~Class Number~@p##-#####' & |
  'p@80L(2)|M~Scheduled Time~@s20@/120L(2)|M~Description~@S30@]|M~Class~[52R(2)|M~Midte' & |
  'rm Exam~C(0)@n3@44R(2)|M~Final Exam~C(0)@n3@44R(2)|M~Term Paper~C(0)@n3@]|M~Grades~'),FROM(Queue:Browse:2), |
  IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(7,128,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,128,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(105,128,45,14),USE(?Delete:3),HIDE
                         END
                         TAB('Photo'),USE(?Tab3)
                           IMAGE,AT(8,24),USE(?Image1)
                           ENTRY(@s64),AT(7,158,129,12),USE(GLO:FileName)
                           PROMPT('Image File Name:'),AT(7,147),USE(?GLO:FileName:Prompt)
                           BUTTON('...'),AT(137,158,12,12),USE(?LookupFile)
                         END
                       END
                       BUTTON('OK'),AT(12,185,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,185,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,185,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
FileLookup6          SelectFileClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Enrollment:Record)
? DEBUGHOOK(Majors:Record)
? DEBUGHOOK(Students:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
!Display photo maintaining aspect ratio
DisplayImage  ROUTINE
  IF ?Image1{PROP:Height} > 100
    AspectRatio$ = ?Image1{PROP:Width}/?Image1{PROP:Height}
    ?Image1{PROP:Height} = 100
    ?Image1{PROP:Width} = 100 * AspectRatio$
  END
  IF ?Image1{PROP:Width} > 100
    AspectRatio$ = ?Image1{PROP:Height}/?Image1{PROP:Width}
    ?Image1{PROP:Width} = 100
    ?Image1{PROP:Height} = 100 * AspectRatio$
  END

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Students Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Students Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateStudents')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?STU:FirstName:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(STU:Record,History::STU:Record)
  SELF.AddHistoryField(?STU:FirstName,2)
  SELF.AddHistoryField(?STU:LastName,3)
  SELF.AddHistoryField(?STU:Address,4)
  SELF.AddHistoryField(?STU:Address2,5)
  SELF.AddHistoryField(?STU:City,6)
  SELF.AddHistoryField(?STU:State,7)
  SELF.AddHistoryField(?STU:Zip,8)
  SELF.AddHistoryField(?STU:Telephone,9)
  SELF.AddHistoryField(?STU:GradYear,11)
  SELF.AddHistoryField(?STU:Number,1)
  SELF.AddHistoryField(?STU:Major,10)
  SELF.AddUpdateFile(Access:Students)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open()                                 ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Students
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:2,Queue:Browse:2,'Queue:Browse:2') !Tpl CBWndPrvListFromQ
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,ENR:StuSeq)      ! Add the sort order for ENR:StuSeq for sort order 1
  BRW2.AddRange(ENR:StudentNumber,Relate:Enrollment,Relate:Students) ! Add file relationship range limit for sort order 1
  BRW2.AddField(ENR:ClassNumber,BRW2.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ScheduledTime,BRW2.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW2.AddField(COU:Description,BRW2.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW2.AddField(ENR:MidtermExam,BRW2.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW2.AddField(ENR:FinalExam,BRW2.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW2.AddField(ENR:TermPaper,BRW2.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW2.AddField(ENR:StudentNumber,BRW2.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ClassNumber,BRW2.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Number,BRW2.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  QuickWindow{PROP:MinWidth} = 160                         ! Restrict the minimum window width
  QuickWindow{PROP:MinHeight} = 204                        ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  !Display photo from stored BLOB
  ?Image1{PROP:NoWidth} = TRUE
  ?Image1{PROP:NoHeight} = TRUE
  IF SELF.OriginalRequest <> InsertRecord
    ?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
  ELSE
    ?Image1{PROP:Text} = 'NoPhoto.BMP'
    PhotoChanged = TRUE
  END
  GLO:FileName = ''
  DO DisplayImage
  
  BRW2.AskProcedure = 1                                    ! Will call: UpdateEnrollment
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  FileLookup6.Init
  FileLookup6.ClearOnCancel = True
  FileLookup6.SetMask('Image Files','*.GIF;*.BMP;*.JPG;*.PCX') ! Set the file mask
  FileLookup6.AddMask('All Files','*.*')                   ! Add additional masks
  FileLookup6.WindowTitle='Choose a File'
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
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


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  MAJ:Number = STU:Major                                   ! Assign linking field value
  Access:Majors.Fetch(MAJ:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateEnrollment
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?Button8
      !Call drag toolbox
      POST(EVENT:AlertKey,?STU:Major)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?STU:Major
      IF Access:Students.TryValidateField(10)              ! Attempt to validate STU:Major in Students
        SELECT(?STU:Major)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?STU:Major
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?STU:Major{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?GLO:FileName
      !Display photo from selected disk file
      IF NOT TARGET{PROP:AcceptAll}
        ?Image1{PROP:NoWidth} = TRUE
        ?Image1{PROP:NoHeight} = TRUE
        ?Image1{PROP:Text} = GLO:FileName
        DO DisplayImage
        PhotoChanged = TRUE
      END
      
    OF ?LookupFile
      ThisWindow.Update()
      GLO:FileName = FileLookup6.Ask(1)
      DISPLAY
      !Display photo from selected disk file
      IF NOT TARGET{PROP:AcceptAll} AND GLO:FileName <> ''
        ?Image1{PROP:NoWidth} = TRUE
        ?Image1{PROP:NoHeight} = TRUE
        ?Image1{PROP:Text} = GLO:FileName
        DO DisplayImage
        PhotoChanged = TRUE
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCompleted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  !Update Photo BLOB on disk
  IF PhotoChanged = TRUE
    STU:Photograph{PROP:size} = 0
    STU:Photograph{PROP:Handle} = ?Image1{PROP:ImageBlob}
  END
  
  ReturnValue = PARENT.TakeCompleted()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?STU:Major
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectMajorsThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?STU:Major
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    OF EVENT:Drop
      !Receive dropped data and close drag toolbox
      STU:Major = DROPID()
      DISPLAY
      POST(EVENT:CloseWindow,,SelectMajorsThread)
      SelectMajorsThread = 0
      SELF.Reset
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?STU:Major
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT STU:Major
        GLO:DropThread = THREAD()
        GLO:DropControl = ?STU:Major
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Majors File
!!! </summary>
BrowseMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Majors)
                       PROJECT(MAJ:Description)
                       PROJECT(MAJ:Number)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Majors File'),AT(,,200,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,HLP('~BrowseMajors'),SYSTEM
                       LIST,AT(8,20,184,143),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Description~L(2)@S20@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(49,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(98,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(147,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,192,162),USE(?CurrentTab)
                         TAB('by Major Description')
                         END
                       END
                       BUTTON('Close'),AT(102,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(151,170,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Majors:Record)
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
  GlobalErrors.SetProcedureName('BrowseMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open()                                     ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Majors,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon MAJ:Description for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,MAJ:KeyDescription) ! Add the sort order for MAJ:KeyDescription for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,MAJ:Description,1,BRW1)        ! Initialize the browse locator using  using key: MAJ:KeyDescription , MAJ:Description
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon MAJ:Number for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,MAJ:KeyNumber)   ! Add the sort order for MAJ:KeyNumber for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,MAJ:Number,1,BRW1)             ! Initialize the browse locator using  using key: MAJ:KeyNumber , MAJ:Number
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateMajors
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
    Relate:Majors.Close()
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
    UpdateMajors
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Students File
!!! </summary>
BrowseStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
Junk                 STRING(20)                            ! 
Junk1                STRING(20),OVER(Junk)                 ! 
Junk2                STRING(20),OVER(Junk)                 ! 
BRW1::View:Browse    VIEW(Students)
                       PROJECT(STU:LastName)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                       PROJECT(STU:Telephone)
                       PROJECT(STU:Number)
                       PROJECT(STU:Major)
                       JOIN(MAJ:KeyNumber,STU:Major)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
Junk                   LIKE(Junk)                     !List box control field - type derived from local data
STU:Address            LIKE(STU:Address)              !List box control field - type derived from field
STU:Address2           LIKE(STU:Address2)             !List box control field - type derived from field
STU:City               LIKE(STU:City)                 !List box control field - type derived from field
STU:State              LIKE(STU:State)                !List box control field - type derived from field
STU:Zip                LIKE(STU:Zip)                  !List box control field - type derived from field
STU:Telephone          LIKE(STU:Telephone)            !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !Primary key field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Students File'),AT(,,358,201),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,HLP('~BrowseStudents'),SYSTEM
                       LIST,AT(8,20,342,143),USE(?Browse:1),HVSCROLL,FORMAT('[80L(2)M~Last Name~@S20@80L(2)|M~' & |
  'First Name~@S20@/80C(2)M~Major~C(0)@S20@40C(2)|M~Grad Year~C(0)@n4@/80C(2)|_M@s20@]|' & |
  'M[123L(2)|M@S20@/80L(2)|M@s20@/83L(2)_M@S20@16L(2)_M@S2@24R(2)|_M@n05@]|M~Address~[5' & |
  '2L(2)~Telephone~@s12@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(207,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(256,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(305,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,350,162),USE(?CurrentTab)
                         TAB('by Major'),USE(?TAB1)
                         END
                         TAB('by Last Name'),USE(?TAB2)
                         END
                         TAB('by Grad Year'),USE(?TAB3)
                         END
                       END
                       BUTTON('Close'),AT(260,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(309,170,45,14),USE(?Help),STD(STD:Help)
                       PROMPT('Press Ctrl+Shift+F1 to open CB wnd Preview. On the Controls list press the LIST' & |
  ' button, then press the From(Q) button and you can see the QUEUE that feeds this lis' & |
  't. Press the "View From(Q)" to see all the data.'),AT(7,170,249,27),USE(?PROMPT1)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


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
  GlobalErrors.SetProcedureName('BrowseStudents')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Junk',Junk)                                        ! Added by: BrowseBox(ABC)
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Students.SetOpenRelated()
  Relate:Students.Open()                                   ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon STU:LastName for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,STU:KeyLastName) ! Add the sort order for STU:KeyLastName for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,STU:LastName,1,BRW1)           ! Initialize the browse locator using  using key: STU:KeyLastName , STU:LastName
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha+ScrollSort:Descending) ! Moveable thumb based upon STU:GradYear for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,STU:KeyGradYear) ! Add the sort order for STU:KeyGradYear for sort order 2
  BRW1.AddLocator(BRW1::Sort2:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort2:Locator.Init(,STU:GradYear,1,BRW1)           ! Initialize the browse locator using  using key: STU:KeyGradYear , STU:GradYear
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon STU:Major for sort order 3
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,STU:MajorKey)    ! Add the sort order for STU:MajorKey for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(,STU:Major,1,BRW1)              ! Initialize the browse locator using  using key: STU:MajorKey , STU:Major
  BRW1.AddField(STU:LastName,BRW1.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW1.AddField(STU:FirstName,BRW1.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(STU:GradYear,BRW1.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW1.AddField(Junk,BRW1.Q.Junk)                          ! Field Junk is a hot field or requires assignment from browse
  BRW1.AddField(STU:Address,BRW1.Q.STU:Address)            ! Field STU:Address is a hot field or requires assignment from browse
  BRW1.AddField(STU:Address2,BRW1.Q.STU:Address2)          ! Field STU:Address2 is a hot field or requires assignment from browse
  BRW1.AddField(STU:City,BRW1.Q.STU:City)                  ! Field STU:City is a hot field or requires assignment from browse
  BRW1.AddField(STU:State,BRW1.Q.STU:State)                ! Field STU:State is a hot field or requires assignment from browse
  BRW1.AddField(STU:Zip,BRW1.Q.STU:Zip)                    ! Field STU:Zip is a hot field or requires assignment from browse
  BRW1.AddField(STU:Telephone,BRW1.Q.STU:Telephone)        ! Field STU:Telephone is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddField(STU:Major,BRW1.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateStudents
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
    Relate:Students.Close()
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
    UpdateStudents
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSIF CHOICE(?CurrentTab) = 3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
StudentTree PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
DisplayString        STRING(255)                           ! 
REL1::Toolbar        CLASS(ToolbarReltreeClass)
TakeEvent            PROCEDURE(<*LONG VCR>,WindowManager WM),VIRTUAL
  END
REL1::SaveLevel      BYTE,AUTO
REL1::Action         LONG,AUTO
Queue:RelTree        QUEUE,PRE()                           ! Browsing Queue
REL1::Display        STRING(200)                           ! Queue display string
REL1::NormalFG       LONG
REL1::NormalBG       LONG
REL1::SelectedFG     LONG
REL1::SelectedBG     LONG
REL1::Icon           SHORT
REL1::Level          LONG                                  ! Record level in the tree
REL1::Loaded         SHORT                                 ! Inferior level is loaded
REL1::Position       STRING(1024)                          ! Record POSITION in VIEW
                END
REL1::LoadedQueue    QUEUE,PRE()                           ! Status Queue
REL1::LoadedLevel    LONG                                  ! Record level
REL1::LoadedPosition STRING(1024)                          ! Record POSITION in VIEW
               END
REL1::CurrentLevel   LONG                                  ! Current loaded level
REL1::CurrentChoice  LONG                                  ! Current record
REL1::NewItemLevel   LONG                                  ! Level for a new item
REL1::NewItemPosition STRING(1024)                         ! POSITION of a new record
REL1::LoadAll        LONG
window               WINDOW('Students and Classes'),AT(,,162,183),RESIZE,CENTER,GRAY,IMM,MDI,HLP('~StudentTree'), |
  SYSTEM
                       LIST,AT(3,5,154,160),USE(?RelTree),VSCROLL,FORMAT('800L*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Insert'),AT(8,109,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(58,109,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(108,109,45,14),USE(?Delete),HIDE
                       BUTTON('&Expand All'),AT(8,168,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,168,45,14),USE(?Contract)
                       BUTTON('Close'),AT(108,168,45,14),USE(?Close)
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Courses:Record)
? DEBUGHOOK(Students:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
REL1::NextParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::NextSavedLevel
  END

REL1::PreviousParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::PreviousSavedLevel
  END

REL1::NextLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::NextSavedLevel

REL1::NextSavedLevel ROUTINE
  DATA
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)+1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    GET(Queue:RelTree,SavePointer)
  END

REL1::PreviousSavedLevel ROUTINE
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)-1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) <> SaveRecords
      SavePointer += 1 + RECORDS(Queue:RelTree) - SaveRecords
    END
    GET(Queue:RelTree,SavePointer)
  END

REL1::PreviousLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::PreviousSavedLevel

REL1::NextRecord ROUTINE
  DO REL1::LoadLevel
  IF CHOICE(?RelTree) < RECORDS(Queue:RelTree)
    SELECT(?RelTree,CHOICE(?RelTree)+1)
  END

REL1::PreviousRecord ROUTINE
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  SavePointer = CHOICE(?RelTree)-1
  LOOP
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) = SaveRecords
      BREAK
    END
    SavePointer += RECORDS(Queue:RelTree) - SaveRecords
  END
  SELECT(?RelTree,SavePointer)

REL1::AssignButtons ROUTINE
  REL1::Toolbar.DeleteButton = ?Delete
  REL1::Toolbar.InsertButton = ?Insert
  REL1::Toolbar.ChangeButton = ?Change
  REL1::Toolbar.HelpButton = ?Help
  Toolbar.SetTarget(?RelTree)

!---------------------------------------------------------------------------
REL1::Load:Students ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| First, the Title line is added.
!|
!| Next, each record of the file Students is read. If the record is not filtered,
!| then the following happens:
!|
!|   First, the queue REL1::LoadedQueue is searched, to see if the tree branch
!|   corresponding to the record is "loaded", that is, if the branch is currently opened.
!|
!|   If the branch is open, then the records for that branch are read from the file
!|   Enrollment. This is done in the routine REL1::Load:Enrollment.
!|
!|   If the branch is not open, then the RelationTree looks for a single record from
!|   Enrollment, to see if any child records are available. If they are, the
!|   branch can be expanded, so REL1::Level gets a -1. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Students record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Students.
!|
  REL1::Display = 'Students and Enrollment'
  REL1::Loaded = 0
  REL1::Position = ''
  REL1::Level = 0
  REL1::Icon = 3
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
  ADD(Queue:RelTree)
  Access:Students.UseFile
  SET(Students)
  LOOP
    IF Access:Students.Next() NOT= Level:Benign
      IF Access:Students.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    REL1::Loaded = 0
    REL1::Position = POSITION(Students)
    REL1::Level = 1
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      ENR:StudentNumber = STU:Number
      CLEAR(ENR:ClassNumber,0)
      Access:Enrollment.UseFile
      SET(ENR:StuSeq,ENR:StuSeq)
      LOOP
        IF Access:Enrollment.Next()
          IF Access:Enrollment.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
            EXIT
          END
        END
        IF UPPER(ENR:StudentNumber) <> UPPER(STU:Number) THEN BREAK.
        REL1::Level = -1
        BREAK
      END
      DO REL1::Format:Students
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 1
      REL1::Loaded = True
      DO REL1::Format:Students
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Enrollment
    END
  END

!---------------------------------------------------------------------------
REL1::Format:Students ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Students.
!|
!| First, the variable DisplayString is assigned the formatted value.
!|
!| Next, the queue variable REL1::Display is assigned the value in
!| DisplayString. It is possible for the display string to be reformatted in
!| the EMBED point "Relation Tree, Before Setting Display on Primary File".
!|
!| Next, any coloring done to the line is performed.
!|
!| Next, any icon assigments are made.
!|
  DisplayString = CLIP(STU:LastName) & ', ' & STU:FirstName
  REL1::Display = DisplayString
  REL1::NormalFG = 16711680
  REL1::NormalBG = -1
  REL1::SelectedFG = 16711680
  REL1::SelectedBG = -1
  IF REL1::Level > 0
    REL1::Icon = 2
  ELSIF REL1::Level < 0
    REL1::Icon = 1
  ELSE
    REL1::Icon = 0
  END

!---------------------------------------------------------------------------
REL1::LoadLevel ROUTINE
!|
!| This routine is used to load a single level of the RelationTree.
!|
!| First, we see where the load comes from. Since the alert-key handling sets
!| ?RelTree{PropList:MouseDownRow} to CHOICE, we can rely on this property
!| containing the correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| load row. If the requested load row is already loaded, we don't have to do
!| anything. If the requested row is not loaded...
!|
!|   First, we set REL1::Level to a positive value for the selected
!|   row and put that record back into the queue Queue:RelTree. The presence of
!|   records with a greater Level below this record in the queue tells the
!|   listbox that the level is opened.
!|
!|   Next, we add a record the the queue REL1::LoadedQueue. This queue
!|   is used to rebuild the display when necessary.
!|
!|   Next, we retrieve the file record that corresponds to the requested load row.
!|
!|   Finally, we reformat the Queue:RelTree entry. This allows for any changes in icon
!|   and colors based on conditional usage.
!|
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  IF NOT REL1::Loaded
    REL1::Level = ABS(REL1::Level)
    PUT(Queue:RelTree)
    REL1::Loaded = True
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    EXECUTE(ABS(REL1::Level))
      BEGIN
        REGET(Students,REL1::Position)
        DO REL1::Format:Students
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    EXECUTE(ABS(REL1::Level))
      DO REL1::Load:Enrollment
    END
  END
!---------------------------------------------------------------------------
REL1::UnloadLevel ROUTINE
!|
!| This routine is used to unload a level of the RelationTree.
!|
!| First, we see where the unload comes from. Since the alert-key handling sets
!| ?RelTree{PropList:MouseDownRow} to CHOICE, we can rely on this property
!| containing the correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| unload row. If the requested load row isn't loaded, we don't have to do
!| anything. If the requested row is loaded...
!|
!|   First, we set REL1::Level to a negative value for the selected
!|   row and put that record back into the queue Queue:RelTree. Since there
!|   won't be any records at lower levels, we use the negative value to signal
!|   the listbox that the level is closed, but children exist.
!|
!|   Next, we retrieve the record the the queue REL1::LoadedQueue that
!|   corresponds to the unloaded level. This queue record is then deleted.
!|
!|   Next, we retrieve the file record that corresponds to the requested load row.
!|
!|   Next, we reformat the Queue:RelTree entry. This allows for any changes in icon
!|   and colors based on conditional usage.
!|
!|   Finally, we run through all of the Queue:RelTree entries for branches below the
!|   unloaded level, and delete these entries.
!|
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  IF REL1::Loaded
    REL1::Level = -ABS(REL1::Level)
    PUT(Queue:RelTree)
    REL1::Loaded = False
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF NOT ERRORCODE()
      DELETE(REL1::LoadedQueue)
    END
    EXECUTE(ABS(REL1::Level))
      BEGIN
        REGET(Students,REL1::Position)
        DO REL1::Format:Students
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    REL1::CurrentLevel = ABS(REL1::Level)
    REL1::CurrentChoice += 1
    LOOP
      GET(Queue:RelTree,REL1::CurrentChoice)
      IF ERRORCODE() THEN BREAK.
      IF ABS(REL1::Level) <= REL1::CurrentLevel THEN BREAK.
      DELETE(Queue:RelTree)
    END
  END
!---------------------------------------------------------------------------
REL1::Load:Enrollment ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| Next, each record of the file Enrollment is read. If the record is not filtered,
!| the queue record that corresponds to this record is formatted and added to the queue
!| Queue:RelTree. This is done in the routine REL1::Format:Enrollment.
!|
  ENR:StudentNumber = STU:Number
  CLEAR(ENR:ClassNumber)
  Access:Enrollment.UseFile
  SET(ENR:StuSeq,ENR:StuSeq)
  LOOP
    IF Access:Enrollment.Next()
      IF Access:Enrollment.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF ENR:StudentNumber <> STU:Number THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Enrollment)
    REL1::Level = 2
    DO REL1::Format:Enrollment
    ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
  END

!-------------------------------------------------------
REL1::Format:Enrollment ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Enrollment.
!|
!| First, the variable DisplayString is assigned the formatted value.
!|
!| Next, the queue variable REL1::Display is assigned the value in
!| DisplayString. It is possible for the display string to be reformatted in
!| the EMBED point "Relation Tree, Before Setting Display on Primary File".
!|
!| Next, any coloring done to the line is performed.
!|
!| Next, any icon assigments are made.
!|
   CLA:ClassNumber = ENR:ClassNumber                       ! Move value for lookup
   Access:Classes.Fetch(CLA:KeyClassNumber)                ! Get value from file
   COU:Number = CLA:CourseNumber                           ! Move value for lookup
   Access:Courses.Fetch(COU:KeyNumber)                     ! Get value from file
  DisplayString = CLIP(COU:Description) & ' ' & CLA:ScheduledTime
  REL1::Display = DisplayString
  IF ENR:MidTermExam < 70 OR ENR:FinalExam < 70 OR ENR:TermPaper < 70
    REL1::NormalFG = 255
    REL1::NormalBG = -1
    REL1::SelectedFG = 255
    REL1::SelectedBG = -1
  ELSE
    REL1::NormalFG = -1
    REL1::NormalBG = -1
    REL1::SelectedFG = -1
    REL1::SelectedBG = -1
  END
  REL1::Icon = 1

REL1::AddEntry ROUTINE
  REL1::Action = InsertRecord
  DO REL1::UpdateLoop

REL1::EditEntry ROUTINE
  REL1::Action = ChangeRecord
  DO REL1::UpdateLoop

REL1::RemoveEntry ROUTINE
  REL1::Action = DeleteRecord
  DO REL1::UpdateLoop

REL1::UpdateLoop ROUTINE
  LOOP
    VCRRequest = VCR:None
    ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
    CASE REL1::Action
      OF InsertRecord
        DO REL1::AddEntryServer
      OF DeleteRecord
        DO REL1::RemoveEntryServer
      OF ChangeRecord
        DO REL1::EditEntryServer
    END
    CASE VCRRequest
      OF VCR:Forward
        DO REL1::NextRecord
      OF VCR:Backward
        DO REL1::PreviousRecord
      OF VCR:PageForward
        DO REL1::NextLevel
      OF VCR:PageBackward
        DO REL1::PreviousLevel
      OF VCR:First
        DO REL1::PreviousParent
      OF VCR:Last
        DO REL1::NextParent
      OF VCR:Insert
        DO REL1::PreviousParent
        REL1::Action = InsertRecord
      OF VCR:None
        BREAK
    END
  END
!---------------------------------------------------------------------------
REL1::AddEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to insert a new record.
!|
!| First, we see where the insert request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| insert row. The new record will be added to the RelationTree level BELOW
!| the requested insert row. To add a first-level record, the RelationTree
!| header must be selected for the insert.
!|
!| Next, the record is cleared, and any related values are primed.
!|
!| Next, GlobalRequest is set to InsertRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the insert is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the newly inserted record highlighted.
!|
  IF ?Insert{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 0
    Access:Students.PrimeRecord
    GlobalRequest = InsertRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  OF 1
  OROF 2
    LOOP WHILE ABS(REL1::Level) = 2
      REL1::CurrentChoice -= 1
      GET(Queue:RelTree,REL1::CurrentChoice)
    UNTIL ERRORCODE()
    REGET(Students,REL1::Position)
    GET(Enrollment,0)
    CLEAR(Enrollment)
    ENR:StudentNumber = STU:Number
    Access:Enrollment.PrimeRecord(1)
    GlobalRequest = InsertRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 2
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::EditEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to change a record.
!|
!| First, we see where the change request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| change row. and retrieve the appropriate record from disk.
!|
!| Next, GlobalRequest is set to ChangeRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the change is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the newly changed record highlighted.
!|
  IF ?Change{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    WATCH(Students)
    REGET(Students,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Students)
      DO REL1::RefreshTree
    END
  OF 2
    WATCH(Enrollment)
    REGET(Enrollment,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::RemoveEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to delete a record.
!|
!| First, we see where the delete request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| delete row. and retrieve the appropriate record from disk.
!|
!| Next, GlobalRequest is set to DeleteRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the change is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the record below the deleted record is highlighted.
!|
  IF ?Delete{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    REGET(Students,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 2
    REGET(Enrollment,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::RefreshTree ROUTINE
!|
!| This routine is used to refresh the RelationTree.
!|
!| First, the queue Queue:RelTree is FREEd. The display is always completely rebuilt.
!|
!| Next, the routine REL1::Load:Students is called. This routine will
!| call any other routines necessary to rebuild the display.
!|
!| Finally, if a new item has been added (via REL1::AddEntry), then the
!| queue is searched for that entry, and the record is highlighted.
!|
  FREE(Queue:RelTree)
  DO REL1::Load:Students
  IF REL1::NewItemLevel
    REL1::CurrentChoice = 0
    LOOP
      REL1::CurrentChoice += 1
      GET(Queue:RelTree,REL1::CurrentChoice)
      IF ERRORCODE() THEN BREAK.
      IF ABS(REL1::Level) <> ABS(REL1::NewItemLevel) THEN CYCLE.
      IF REL1::Position <> REL1::NewItemPosition THEN CYCLE.
      SELECT(?RelTree,REL1::CurrentChoice)
      BREAK
    END
  END
!---------------------------------------------------------------------------
REL1::ContractAll ROUTINE
!|
!| This routine re-initializes the RelationTree.
!|
!| The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd, and the routine REL1::Load:Students is called, which loads
!| the first level of the RelationTree.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  DO REL1::Load:Students
!---------------------------------------------------------------------------
REL1::ExpandAll ROUTINE
!|
!| This routine expands every branch of the RelationTree.
!|
!| First, The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd.
!|
!| Next, the variable REL1::LoadAll is set to true, and the routine REL1::Load:Students
!| is called. Since REL1::LoadAll is True, all branches are completely loaded.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  REL1::LoadAll = True
  DO REL1::Load:Students
  REL1::LoadAll = False

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('StudentTree')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?RelTree
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  Access:Courses.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  DO REL1::ContractAll
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?RelTree,Queue:RelTree,'Queue:RelTree') !Tpl CBWndPrvListFromQ
  window{PROP:MinWidth} = 162                              ! Restrict the minimum window width
  window{PROP:MinHeight} = 183                             ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  Toolbar.AddTarget(REL1::Toolbar, ?RelTree)
  DO REL1::AssignButtons
  ?RelTree{PROP:IconList,1} = '~Closed.ico'
  ?RelTree{PROP:IconList,2} = '~Open.ico'
  ?RelTree{PROP:IconList,3} = '~~open.ico'
  ?RelTree{Prop:Selected} = 1
  ?RelTree{PROP:Alrt,255} = CtrlRight
  ?RelTree{PROP:Alrt,254} = CtrlLeft
  ?RelTree{PROP:Alrt,253} = MouseLeft2
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?Expand
      SETCURSOR(CURSOR:Wait)
      
    OF ?Contract
      SETCURSOR(CURSOR:Wait)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Insert
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::AddEntry
    OF ?Change
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::EditEntry
    OF ?Delete
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::RemoveEntry
    OF ?Expand
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ExpandAll
      SETCURSOR
      
    OF ?Contract
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ContractAll
      SETCURSOR
      
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    ELSE
      CASE EVENT()
      OF EVENT:AlertKey
        CASE KEYCODE()
        OF CtrlRight
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Expanded,?RelTree)
        OF CtrlLeft
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Contracted,?RelTree)
        OF MouseLeft2
          DO REL1::EditEntry
        END
      END
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    OF EVENT:Expanded
      DO REL1::LoadLevel
    OF EVENT:Contracted
      DO REL1::UnloadLevel
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?RelTree
      CASE KEYCODE()
      OF MouseRight
      OROF AppsKey
        EXECUTE(POPUP('&Insert|&Change|&Delete'))
          DO REL1::AddEntry
          DO REL1::EditEntry
          DO REL1::RemoveEntry
        END
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
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
    OF EVENT:GainFocus
      REL1::CurrentChoice = CHOICE(?RelTree)
      GET(Queue:RelTree,REL1::CurrentChoice)
      REL1::NewItemLevel = REL1::Level
      REL1::NewItemPosition = REL1::Position
      DO REL1::RefreshTree
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

REL1::Toolbar.TakeEvent PROCEDURE(<*LONG VCR>,WindowManager WM)
  CODE
  CASE ACCEPTED()
  OF Toolbar:Bottom TO Toolbar:Up
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Bottom+1)
      DO REL1::NextParent
      DO REL1::PreviousParent
      DO REL1::NextLevel
      DO REL1::PreviousLevel
      DO REL1::NextRecord
      DO REL1::PreviousRecord
    END
  OF Toolbar:Insert TO Toolbar:Delete
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Insert+1)
      DO REL1::AddEntry
      DO REL1::EditEntry
      DO REL1::RemoveEntry
    END
  ELSE
    PARENT.TakeEvent(VCR,ThisWindow)
  END

Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Teachers File
!!! </summary>
BrowseTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:Address)
                       PROJECT(TEA:City)
                       PROJECT(TEA:State)
                       PROJECT(TEA:Zip)
                       PROJECT(TEA:Telephone)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                       JOIN(MAJ:KeyNumber,TEA:Department)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
TEA:Address            LIKE(TEA:Address)              !List box control field - type derived from field
TEA:City               LIKE(TEA:City)                 !List box control field - type derived from field
TEA:State              LIKE(TEA:State)                !List box control field - type derived from field
TEA:Zip                LIKE(TEA:Zip)                  !List box control field - type derived from field
TEA:Telephone          LIKE(TEA:Telephone)            !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Teachers File'),AT(,,358,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,HLP('~BrowseTeachers'),SYSTEM
                       LIST,AT(8,20,342,143),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Department~@S20@80L(2)|M' & |
  '~Last Name~@S20@80L(2)|M~First Name~@S20@80L(2)|M~Address~@S20@80L(2)|M~City~@S20@24' & |
  'L(2)|M~State~@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~@s12@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(207,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(256,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(305,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,350,162),USE(?CurrentTab)
                         TAB('by Last Name')
                         END
                         TAB('by Department')
                         END
                       END
                       BUTTON('Close'),AT(260,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(309,170,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Majors:Record)
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
  GlobalErrors.SetProcedureName('BrowseTeachers')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open()                                     ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Teachers,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon TEA:LastName for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,TEA:KeyLastName) ! Add the sort order for TEA:KeyLastName for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,TEA:LastName,1,BRW1)           ! Initialize the browse locator using  using key: TEA:KeyLastName , TEA:LastName
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:FirstName,BRW1.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Address,BRW1.Q.TEA:Address)            ! Field TEA:Address is a hot field or requires assignment from browse
  BRW1.AddField(TEA:City,BRW1.Q.TEA:City)                  ! Field TEA:City is a hot field or requires assignment from browse
  BRW1.AddField(TEA:State,BRW1.Q.TEA:State)                ! Field TEA:State is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Zip,BRW1.Q.TEA:Zip)                    ! Field TEA:Zip is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Telephone,BRW1.Q.TEA:Telephone)        ! Field TEA:Telephone is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Department,BRW1.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateTeachers
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
    Relate:Majors.Close()
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
    UpdateTeachers
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

