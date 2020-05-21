

   MEMBER('SCHOOL.clw')                                    ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Classes Record
!!! </summary>
SelectClasses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
BRW1::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:CourseNumber)
                       PROJECT(CLA:TeacherNumber)
                       JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                         PROJECT(TEA:LastName)
                         PROJECT(TEA:Number)
                       END
                       JOIN(COU:KeyNumber,CLA:CourseNumber)
                         PROJECT(COU:Description)
                         PROJECT(COU:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
CLA:RoomNumber         LIKE(CLA:RoomNumber)           !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
CLA:CourseNumber       LIKE(CLA:CourseNumber)         !Browse key field - type derived from field
CLA:TeacherNumber      LIKE(CLA:TeacherNumber)        !Browse key field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Class to Enroll in'),AT(,,189,191),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,HLP('~SelectClasses'),SYSTEM,TOOLBOX
                       LIST,AT(8,30,172,142),USE(?Browse:1),HVSCROLL,DRAGID('Classes'),FORMAT('[52L(2)|M~Class' & |
  ' Number~@P##-#####P@120L(2)|M~Description~@S30@/52C(2)|M~Room~C(0)@n4@80L(2)|M~Sched' & |
  'uled Time~@s20@80L(2)|M~Instructor~@S20@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,180,172),USE(?CurrentTab)
                         TAB('by Class Number'),USE(?TAB1)
                         END
                         TAB('by Course Number'),USE(?TAB2)
                         END
                         TAB('by Teacher Number'),USE(?TAB3)
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
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3

  CODE
? DEBUGHOOK(Classes:Record)
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
  GlobalErrors.SetProcedureName('SelectClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processes
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:CourseNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,CLA:KeyCourseNumber) ! Add the sort order for CLA:KeyCourseNumber for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,CLA:CourseNumber,1,BRW1)       ! Initialize the browse locator using  using key: CLA:KeyCourseNumber , CLA:CourseNumber
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 2
  BRW1.AddLocator(BRW1::Sort2:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort2:Locator.Init(,CLA:TeacherNumber,1,BRW1)      ! Initialize the browse locator using  using key: CLA:KeyTeacherNumber , CLA:TeacherNumber
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:ClassNumber for sort order 3
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,CLA:KeyClassNumber) ! Add the sort order for CLA:KeyClassNumber for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(,CLA:ClassNumber,1,BRW1)        ! Initialize the browse locator using  using key: CLA:KeyClassNumber , CLA:ClassNumber
  BRW1.AddField(CLA:ClassNumber,BRW1.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(CLA:RoomNumber,BRW1.Q.CLA:RoomNumber)      ! Field CLA:RoomNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ScheduledTime,BRW1.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(CLA:CourseNumber,BRW1.Q.CLA:CourseNumber)  ! Field CLA:CourseNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:TeacherNumber,BRW1.Q.CLA:TeacherNumber) ! Field CLA:TeacherNumber is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
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
      SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
      
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
  ELSIF CHOICE(?CurrentTab) = 3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Enrollment File
!!! </summary>
UpdateEnrollment PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectStudentsThread LONG                                  ! 
SelectClassesThread  LONG                                  ! 
History::ENR:Record  LIKE(ENR:RECORD),THREAD
QuickWindow          WINDOW('Update the Enrollment File'),AT(,,263,102),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateEnrollment'),SYSTEM
                       PROMPT('&Student Number:'),AT(8,10),USE(?ENR:StudentNumber:Prompt)
                       ENTRY(@p###-##-####p),AT(64,10,52,10),USE(ENR:StudentNumber),RIGHT(1),ALRT(F10Key),ALRT(MouseRight), |
  DROPID('Students'),MSG('Press F10 to drag a Student'),TIP('Press F10 to drag a Student')
                       BUTTON('...'),AT(118,9,12,12),USE(?Button4)
                       STRING(@S20),AT(133,10),USE(STU:LastName)
                       PROMPT('&Class Number'),AT(8,31),USE(?ENR:ClassNumber:Prompt)
                       ENTRY(@p##-#####p),AT(64,31,40,10),USE(ENR:ClassNumber),RIGHT(1),ALRT(F10Key),ALRT(MouseRight), |
  DROPID('Classes'),MSG('Press F10 to drag a Class'),TIP('Press F10 to drag a Class')
                       BUTTON('...'),AT(107,30,12,12),USE(?Button5)
                       STRING(@S30),AT(133,31),USE(COU:Description)
                       STRING(@s20),AT(133,42),USE(CLA:ScheduledTime)
                       PROMPT('Midterm:'),AT(11,63),USE(?ENR:MidtermExam:Prompt)
                       ENTRY(@n3),AT(41,63,40,10),USE(ENR:MidtermExam)
                       PROMPT('Final:'),AT(96,63),USE(?ENR:FinalExam:Prompt)
                       ENTRY(@n3),AT(115,63,40,10),USE(ENR:FinalExam)
                       PROMPT('Term Paper:'),AT(170,63),USE(?ENR:TermPaper:Prompt)
                       ENTRY(@n3),AT(212,63,40,10),USE(ENR:TermPaper)
                       BUTTON('OK'),AT(4,84,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(53,84,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(102,84,45,14),USE(?Help),STD(STD:Help)
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
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Enrollment Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Enrollment Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?ENR:StudentNumber:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(ENR:Record,History::ENR:Record)
  SELF.AddHistoryField(?ENR:StudentNumber,1)
  SELF.AddHistoryField(?ENR:ClassNumber,2)
  SELF.AddHistoryField(?ENR:MidtermExam,3)
  SELF.AddHistoryField(?ENR:FinalExam,4)
  SELF.AddHistoryField(?ENR:TermPaper,5)
  SELF.AddUpdateFile(Access:Enrollment)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Enrollment
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
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  Access:Students.Fetch(STU:KeyStudentNumber)
  CLA:ClassNumber = ENR:ClassNumber                        ! Assign linking field value
  Access:Classes.Fetch(CLA:KeyClassNumber)
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  Access:Courses.Fetch(COU:KeyNumber)
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
    SelectClasses
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
    OF ?Button4
      !Call drag toolbox
      POST(EVENT:AlertKey,?ENR:StudentNumber)
      
    OF ?Button5
      !Call drag toolbox
      POST(EVENT:AlertKey,?ENR:ClassNumber)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?ENR:StudentNumber
      IF Access:Enrollment.TryValidateField(1)             ! Attempt to validate ENR:StudentNumber in Enrollment
        SELECT(?ENR:StudentNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:StudentNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:StudentNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:ClassNumber
      IF NOT QuickWindow{PROP:AcceptAll}
        CLA:ClassNumber = ENR:ClassNumber
        IF Access:Classes.TryFetch(CLA:KeyClassNumber)
          IF SELF.Run(1,SelectRecord) = RequestCompleted
            ENR:ClassNumber = CLA:ClassNumber
          ELSE
            SELECT(?ENR:ClassNumber)
            CYCLE
          END
        END
      END
      ThisWindow.Reset(0)
      IF Access:Enrollment.TryValidateField(2)             ! Attempt to validate ENR:ClassNumber in Enrollment
        SELECT(?ENR:ClassNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:ClassNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:ClassNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:MidtermExam
      IF Access:Enrollment.TryValidateField(3)             ! Attempt to validate ENR:MidtermExam in Enrollment
        SELECT(?ENR:MidtermExam)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:MidtermExam
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:MidtermExam{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:FinalExam
      IF Access:Enrollment.TryValidateField(4)             ! Attempt to validate ENR:FinalExam in Enrollment
        SELECT(?ENR:FinalExam)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:FinalExam
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:FinalExam{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:TermPaper
      IF Access:Enrollment.TryValidateField(5)             ! Attempt to validate ENR:TermPaper in Enrollment
        SELECT(?ENR:TermPaper)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:TermPaper
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:TermPaper{PROP:FontColor} = FieldColorQueue.OldColor
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
  OF ?ENR:StudentNumber
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectStudentsThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:StudentNumber
        SelectStudentsThread = START(SelectStudents)
        GLO:ThreadRef &= SelectStudentsThread
      END
      
    OF EVENT:Drop
      !Receive dropped data and close drag toolbox
      ENR:StudentNumber = DEFORMAT(DROPID(),@P###-##-####P)
      DISPLAY
      POST(EVENT:CloseWindow,,SelectStudentsThread)
      SelectStudentsThread = 0
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  OF ?ENR:ClassNumber
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectClassesThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:ClassNumber
        SelectClassesThread = START(SelectClasses)
        GLO:ThreadRef &= SelectClassesThread
      END
      
    OF EVENT:Drop
      !Receive dropped data and close drag toolbox
      ENR:ClassNumber = DEFORMAT(DROPID(),@P##-#####P)
      DISPLAY
      POST(EVENT:CloseWindow,,SelectClassesThread)
      SelectClassesThread = 0
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
    OF ?ENR:StudentNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT ENR:StudentNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:StudentNumber
        SelectStudentsThread = START(SelectStudents)
        GLO:ThreadRef &= SelectStudentsThread
      END
      
    OF ?ENR:ClassNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT ENR:ClassNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:ClassNumber
        SelectClassesThread = START(SelectClasses)
        GLO:ThreadRef &= SelectClassesThread
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
      IF SelectStudentsThread
        POST(EVENT:CloseWindow,,SelectStudentsThread)
      END
      IF SelectClassesThread
        POST(EVENT:CloseWindow,,SelectClassesThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Students Record
!!! </summary>
SelectStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
BRW1::View:Browse    VIEW(Students)
                       PROJECT(STU:LastName)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:Number)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Major)
                       JOIN(MAJ:KeyNumber,STU:Major)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Student to Enroll'),AT(,,257,187),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,HLP('~SelectStudents'),SYSTEM,TOOLBOX
                       LIST,AT(8,20,237,141),USE(?Browse:1),HVSCROLL,DRAGID('Students'),FORMAT('[80L(2)|M~Last' & |
  ' Name~@S20@80L(2)|M~First Name~@S20@/80C(1)|M~Number~C(0)@P###-##-####P@93C(3)|M~Maj' & |
  'or~C(0)@S20@40C(2)|M~Grad Year~@n4@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,247,162),USE(?CurrentTab)
                         TAB('by Major')
                         END
                         TAB('by Last Name')
                         END
                         TAB('by Grad Year')
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
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3

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
  GlobalErrors.SetProcedureName('SelectStudents')
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
  Relate:Students.SetOpenRelated()
  Relate:Students.Open()                                   ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon STU:LastName for sort order 1
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
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(STU:GradYear,BRW1.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW1.AddField(STU:Major,BRW1.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
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
    Relate:Students.Close()
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
      SETDROPID(FORMAT(STU:Number,@P###-##-####P))
      
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
  ELSIF CHOICE(?CurrentTab) = 3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(FORMAT(STU:Number,@P###-##-####P))
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Classes File
!!! </summary>
UpdateClasses PROCEDURE 

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
SelectCoursesThread  LONG                                  ! 
SelectTeachersThread LONG                                  ! 
History::CLA:Record  LIKE(CLA:RECORD),THREAD
QuickWindow          WINDOW('Update the Classes File'),AT(,,164,138),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateClasses'),SYSTEM
                       SHEET,AT(2,3,158,114),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&Class Number:'),AT(7,21),USE(?CLA:ClassNumber:Prompt)
                           ENTRY(@P##-#####P),AT(71,21,40,10),USE(CLA:ClassNumber),RIGHT(1)
                           PROMPT('&Course Number:'),AT(7,34),USE(?CLA:CourseNumber:Prompt)
                           ENTRY(@n4),AT(71,34,24,10),USE(CLA:CourseNumber),RIGHT(1),ALRT(F10Key),DROPID('Courses')
                           BUTTON('...'),AT(98,33,12,12),USE(?Button7)
                           STRING(@S20),AT(71,47),USE(COU:Description)
                           ENTRY(@p###-##-####p),AT(71,60,,10),USE(CLA:TeacherNumber),RIGHT(1),ALRT(F10Key),DROPID('Teachers')
                           BUTTON('...'),AT(128,60,12,12),USE(?Button7:2)
                           PROMPT('&Teacher Number:'),AT(7,60),USE(?CLA:TeacherNumber:Prompt)
                           STRING(@S20),AT(71,73),USE(TEA:LastName)
                           PROMPT('Room Number:'),AT(7,90),USE(?CLA:RoomNumber:Prompt)
                           ENTRY(@n4),AT(71,90,40,10),USE(CLA:RoomNumber)
                           PROMPT('Scheduled Time:'),AT(7,103),USE(?CLA:ScheduledTime:Prompt)
                           ENTRY(@s20),AT(71,103,84,10),USE(CLA:ScheduledTime)
                         END
                       END
                       BUTTON('OK'),AT(17,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(66,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(115,120,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Courses:Record)
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
    ActionMessage = 'Adding a Classes Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Classes Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?CLA:ClassNumber:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(CLA:Record,History::CLA:Record)
  SELF.AddHistoryField(?CLA:ClassNumber,1)
  SELF.AddHistoryField(?CLA:CourseNumber,2)
  SELF.AddHistoryField(?CLA:TeacherNumber,3)
  SELF.AddHistoryField(?CLA:RoomNumber,4)
  SELF.AddHistoryField(?CLA:ScheduledTime,5)
  SELF.AddUpdateFile(Access:Classes)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Classes
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
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  Access:Teachers.Fetch(TEA:KeyTeacherNumber)
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  Access:Courses.Fetch(COU:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
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
    OF ?CLA:CourseNumber
      !Close drag toolbox
      IF SelectCoursesThread
        POST(EVENT:CloseWindow,,SelectCoursesThread)
      END
      
    OF ?Button7
      !Call drag toolbox
      POST(EVENT:AlertKey,?CLA:CourseNumber)
      
    OF ?CLA:TeacherNumber
      !Close drag toolbox
      IF SelectTeachersThread
        POST(EVENT:CloseWindow,,SelectTeachersThread)
        SelectTeachersThread = 0
      END
      
    OF ?Button7:2
      !Call drag toolbox
      POST(EVENT:AlertKey,?CLA:TeacherNumber)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?CLA:CourseNumber
      IF Access:Classes.TryValidateField(2)                ! Attempt to validate CLA:CourseNumber in Classes
        SELECT(?CLA:CourseNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?CLA:CourseNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?CLA:CourseNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?CLA:TeacherNumber
      IF Access:Classes.TryValidateField(3)                ! Attempt to validate CLA:TeacherNumber in Classes
        SELECT(?CLA:TeacherNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?CLA:TeacherNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?CLA:TeacherNumber{PROP:FontColor} = FieldColorQueue.OldColor
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
  OF ?CLA:CourseNumber
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectCoursesThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:CourseNumber
        SelectCoursesThread = START(SelectCourses)
        GLO:ThreadRef &= SelectCoursesThread
      END
      
    OF EVENT:Drop
      !Receive dropped data and close drag toolbox
      CLA:CourseNumber = DROPID()
      DISPLAY
      POST(EVENT:CloseWindow,,SelectCoursesThread)
      SelectCoursesThread = 0
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  OF ?CLA:TeacherNumber
    CASE EVENT()
    OF EVENT:AlertKey
      !Call drag toolbox
      IF NOT SelectTeachersThread
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:TeacherNumber
        SelectTeachersThread = START(SelectTeachers)
        GLO:ThreadRef &= SelectTeachersThread
      END
      
    OF EVENT:Drop
      CLA:TeacherNumber = DROPID()
      DISPLAY
      POST(EVENT:CloseWindow,,SelectTeachersThread)
      SelectTeachersThread = 0
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
    OF ?CLA:CourseNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT CLA:CourseNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:CourseNumber
        SelectCoursesThread = START(SelectCourses)
        GLO:ThreadRef &= SelectCoursesThread
      END
      
    OF ?CLA:TeacherNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT CLA:TeacherNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:TeacherNumber
        SelectTeachersThread = START(SelectTeachers)
        GLO:ThreadRef &= SelectTeachersThread
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
      !Close drag thread
      IF SelectCoursesThread
        POST(EVENT:CloseWindow,,SelectCoursesThread)
      END
      !Close drag thread
      IF SelectTeachersThread
        POST(EVENT:CloseWindow,,SelectTeachersThread)
        SelectTeachersThread = 0
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Courses Record
!!! </summary>
SelectCourses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
BRW1::View:Browse    VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
COU:Number             LIKE(COU:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Course'),AT(,,158,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,HLP('~SelectCourses'), |
  SYSTEM,TOOLBOX
                       LIST,AT(8,20,142,154),USE(?Browse:1),HVSCROLL,DRAGID('Courses'),FORMAT('80L(2)|M~Descri' & |
  'ption~L(2)@S30@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,177),USE(?CurrentTab)
                         TAB('by Course Description')
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
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager

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
  GlobalErrors.SetProcedureName('SelectCourses')
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
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
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
      SETDROPID(COU:Number)
      
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


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(COU:Number)
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Teachers Record
!!! </summary>
SelectTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
BRW1::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                       JOIN(MAJ:KeyNumber,TEA:Department)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag a Teacher'),AT(,,251,171),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM, |
  HLP('~SelectTeachers'),SYSTEM,TOOLBOX
                       LIST,AT(8,20,230,124),USE(?Browse:1),HVSCROLL,DRAGID('Teachers'),FORMAT('80L(2)|M~Last ' & |
  'Name~@S20@80L(2)|M~First Name~@S20@80L(2)|M~Department~@S20@'),FROM(Queue:Browse:1),IMM, |
  MSG('Browsing Records')
                       SHEET,AT(4,4,242,162),USE(?CurrentTab)
                         TAB('by Last Name')
                         END
                         TAB('by Department')
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
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2

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
  GlobalErrors.SetProcedureName('SelectTeachers')
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
  Relate:Teachers.SetOpenRelated()
  Relate:Teachers.Open()                                   ! File Teachers used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Teachers,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,TEA:Department,1,BRW1)         ! Initialize the browse locator using  using key: TEA:KeyDepartment , TEA:Department
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon TEA:LastName for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,TEA:KeyLastName) ! Add the sort order for TEA:KeyLastName for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,TEA:LastName,1,BRW1)           ! Initialize the browse locator using  using key: TEA:KeyLastName , TEA:LastName
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:FirstName,BRW1.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Department,BRW1.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
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
    Relate:Teachers.Close()
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
      SETDROPID(Tea:Number)
      
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
    SETDROPID(Tea:Number)
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Courses File
!!! </summary>
UpdateCourses PROCEDURE 

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
History::COU:Record  LIKE(COU:RECORD),THREAD
QuickWindow          WINDOW('Update the Courses File'),AT(,,232,150),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateCourses'),SYSTEM
                       SHEET,AT(3,2,226,125),USE(?CurrentTab),WIZARD
                         TAB('General'),USE(?TAB1)
                           PROMPT('&Description:'),AT(9,15,46,10),USE(?COU:Description:Prompt)
                           ENTRY(@S30),AT(69,15,148,10),USE(COU:Description)
                           PROMPT('Complete Description:'),AT(9,33),USE(?COU:CompleteDescription:Prompt)
                         END
                       END
                       BUTTON('OK'),AT(81,130,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(130,130,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(179,130,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Courses Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Courses Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateCourses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?COU:Description:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(COU:Record,History::COU:Record)
  SELF.AddHistoryField(?COU:Description,2)
  SELF.AddUpdateFile(Access:Courses)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Courses
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
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
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

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
ClassTree PROCEDURE 

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
window               WINDOW('Tree List of Classes and Enrollments'),AT(,,162,174),RESIZE,CENTER,GRAY,IMM,MDI,HLP('~ClassTree'), |
  SYSTEM
                       LIST,AT(4,5,150,147),USE(?RelTree),VSCROLL,FORMAT('800L|M*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Expand All'),AT(8,155,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,155,45,14),USE(?Contract)
                       BUTTON('&Insert'),AT(7,103,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(57,103,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(107,103,45,14),USE(?Delete),HIDE
                       BUTTON('Close'),AT(108,155,45,14),USE(?Close)
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
REL1::Load:Courses ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| First, the Title line is added.
!|
!| Next, each record of the file Courses is read. If the record is not filtered,
!| then the following happens:
!|
!|   First, the queue REL1::LoadedQueue is searched, to see if the tree branch
!|   corresponding to the record is "loaded", that is, if the branch is currently opened.
!|
!|   If the branch is open, then the records for that branch are read from the file
!|   Classes. This is done in the routine REL1::Load:Classes.
!|
!|   If the branch is not open, then the RelationTree looks for a single record from
!|   Classes, to see if any child records are available. If they are, the
!|   branch can be expanded, so REL1::Level gets a -1. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Courses record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Courses.
!|
  REL1::Display = 'Classes and Enrollments'
  REL1::Loaded = 0
  REL1::Position = ''
  REL1::Level = 0
  REL1::Icon = 3
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
  ADD(Queue:RelTree)
  Access:Courses.UseFile
  SET(Courses)
  LOOP
    IF Access:Courses.Next() NOT= Level:Benign
      IF Access:Courses.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    REL1::Loaded = 0
    REL1::Position = POSITION(Courses)
    REL1::Level = 1
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      CLA:CourseNumber = COU:Number
      CLEAR(CLA:ClassNumber,0)
      Access:Classes.UseFile
      SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
      LOOP
        IF Access:Classes.Next()
          IF Access:Classes.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
            EXIT
          END
        END
        IF UPPER(CLA:CourseNumber) <> UPPER(COU:Number) THEN BREAK.
        REL1::Level = -1
        BREAK
      END
      DO REL1::Format:Courses
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 1
      REL1::Loaded = True
      DO REL1::Format:Courses
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Classes
    END
  END

!---------------------------------------------------------------------------
REL1::Format:Courses ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Courses.
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
  DisplayString = COU:Description
  REL1::Display = DisplayString
  REL1::NormalFG = 255
  REL1::NormalBG = -1
  REL1::SelectedFG = 255
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
        REGET(Courses,REL1::Position)
        DO REL1::Format:Courses
      END
      BEGIN
        REGET(Classes,REL1::Position)
        DO REL1::Format:Classes
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    EXECUTE(ABS(REL1::Level))
      DO REL1::Load:Classes
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
        REGET(Courses,REL1::Position)
        DO REL1::Format:Courses
      END
      BEGIN
        REGET(Classes,REL1::Position)
        DO REL1::Format:Classes
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
REL1::Load:Classes ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| For each record of the file Classes is read. If the record is not filtered,
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
!|   branch can be expanded, so REL1::Level gets a -2. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Classes record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Classes.
!|
  CLA:CourseNumber = COU:Number
  CLEAR(CLA:ClassNumber)
  Access:Classes.UseFile
  SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
  LOOP
    IF Access:Classes.Next()
      IF Access:Classes.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF CLA:CourseNumber <> COU:Number THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Classes)
    REL1::Level = 2
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      ENR:ClassNumber = CLA:ClassNumber
      CLEAR(ENR:StudentNumber,0)
      Access:Enrollment.UseFile
      SET(ENR:SeqStu,ENR:SeqStu)
      LOOP
        IF Access:Enrollment.Next()
          IF Access:Enrollment.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
            EXIT
          END
        END
        IF UPPER(ENR:ClassNumber) <> UPPER(CLA:ClassNumber) THEN BREAK.
        REL1::Level = -2
        BREAK
      END
      DO REL1::Format:Classes
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 2
      REL1::Loaded = True
      DO REL1::Format:Classes
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Enrollment
    END
  END

!-------------------------------------------------------
REL1::Format:Classes ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Classes.
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
  DisplayString = CLA:ScheduledTime
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
REL1::Load:Enrollment ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| Next, each record of the file Enrollment is read. If the record is not filtered,
!| the queue record that corresponds to this record is formatted and added to the queue
!| Queue:RelTree. This is done in the routine REL1::Format:Enrollment.
!|
  ENR:ClassNumber = CLA:ClassNumber
  CLEAR(ENR:StudentNumber)
  Access:Enrollment.UseFile
  SET(ENR:SeqStu,ENR:SeqStu)
  LOOP
    IF Access:Enrollment.Next()
      IF Access:Enrollment.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF ENR:ClassNumber <> CLA:ClassNumber THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Enrollment)
    REL1::Level = 3
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
   STU:Number = ENR:StudentNumber                          ! Move value for lookup
   Access:Students.Fetch(STU:KeyStudentNumber)             ! Get value from file
  DisplayString = CLIP(STU:LastName) & ', ' & STU:FirstName
  REL1::Display = DisplayString
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
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
    Access:Courses.PrimeRecord
    GlobalRequest = InsertRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  OF 1
    REGET(Courses,REL1::Position)
    GET(Classes,0)
    CLEAR(Classes)
    CLA:CourseNumber = COU:Number
    Access:Classes.PrimeRecord(1)
    GlobalRequest = InsertRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 2
      REL1::NewItemPosition = POSITION(Classes)
      DO REL1::RefreshTree
    END
  OF 2
  OROF 3
    LOOP WHILE ABS(REL1::Level) = 3
      REL1::CurrentChoice -= 1
      GET(Queue:RelTree,REL1::CurrentChoice)
    UNTIL ERRORCODE()
    REGET(Classes,REL1::Position)
    GET(Enrollment,0)
    CLEAR(Enrollment)
    ENR:ClassNumber = CLA:ClassNumber
    Access:Enrollment.PrimeRecord(1)
    GlobalRequest = InsertRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 3
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
    WATCH(Courses)
    REGET(Courses,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Courses)
      DO REL1::RefreshTree
    END
  OF 2
    WATCH(Classes)
    REGET(Classes,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Classes)
      DO REL1::RefreshTree
    END
  OF 3
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
    REGET(Courses,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 2
    REGET(Classes,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 3
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
!| Next, the routine REL1::Load:Courses is called. This routine will
!| call any other routines necessary to rebuild the display.
!|
!| Finally, if a new item has been added (via REL1::AddEntry), then the
!| queue is searched for that entry, and the record is highlighted.
!|
  FREE(Queue:RelTree)
  DO REL1::Load:Courses
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
!| are FREEd, and the routine REL1::Load:Courses is called, which loads
!| the first level of the RelationTree.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  DO REL1::Load:Courses
!---------------------------------------------------------------------------
REL1::ExpandAll ROUTINE
!|
!| This routine expands every branch of the RelationTree.
!|
!| First, The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd.
!|
!| Next, the variable REL1::LoadAll is set to true, and the routine REL1::Load:Courses
!| is called. Since REL1::LoadAll is True, all branches are completely loaded.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  REL1::LoadAll = True
  DO REL1::Load:Courses
  REL1::LoadAll = False

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('ClassTree')
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
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open()                                    ! File Courses used by this procedure, so make sure it's RelationManager is open
  Access:Students.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  DO REL1::ContractAll
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?RelTree,Queue:RelTree,'Queue:RelTree') !Tpl CBWndPrvListFromQ
  window{PROP:MinWidth} = 162                              ! Restrict the minimum window width
  window{PROP:MinHeight} = 174                             ! Restrict the minimum window height
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
    Relate:Courses.Close()
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
        EXECUTE(POPUP('&Insert|&Change|&Delete|-|&Expand All|Co&ntract All'))
          DO REL1::AddEntry
          DO REL1::EditEntry
          DO REL1::RemoveEntry
          DO REL1::ExpandAll
          DO REL1::ContractAll
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
!!! Generated from procedure template - Frame
!!! Clarion for Windows Wizard Application
!!! </summary>
Main PROCEDURE 

OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
CurrentTab           STRING(80)                            ! 
SplashProcedureThread LONG
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('SV SQLite University + Carl''s WndPreview Class hooked via Ctrl+Shift+F1'),AT(, |
  ,600,370),FONT('MS Sans Serif',8,COLOR:Black),RESIZE,ALRT(MouseLeft2),CENTER,ICON('_SoftVUn.ico'), |
  IMM,MAX,HLP('~TopSpeedUniversity'),STATUS(-1,80,120,45),SYSTEM
                       MENUBAR,USE(?MENUBAR1)
                         MENU('*DevCon2019*'),USE(?DevCon2019)
                           ITEM('Enable F1 Hook to show CB Window Preview '),USE(?CbHlpHookEnableItem)
                           ITEM('(Hide the above item for non-developer users'),USE(?CbHlpHookEnableItem2)
                         END
                         MENU('&File'),USE(?MENU1)
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR2),SEPARATOR
                           ITEM('Create SQLite DB'),USE(?CreateDB)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit'),USE(?MENU2)
                           ITEM('Cu&t'),USE(?Cut),KEY(CtrlX),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),KEY(CtrlC),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),KEY(CtrlV),MSG('Paste contents of Windows Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Trees'),USE(?Trees)
                           ITEM('&Class Tree'),USE(?BrowseClassTree)
                           ITEM('&Students Tree'),USE(?BrowseStudentsTree)
                         END
                         MENU('&Browse'),USE(?MENU3)
                           ITEM('Students '),USE(?BrowseStudents),MSG('Browse Students')
                           ITEM('Teachers '),USE(?BrowseTeachers),MSG('Browse Teachers')
                           ITEM('Classes '),USE(?BrowseClasses),MSG('Browse Classes')
                           ITEM('Enrollment '),USE(?BrowseEnrollment),MSG('Browse Enrollment')
                           ITEM('Courses (no HLP())'),USE(?BrowseCourses),MSG('Browse Courses')
                           ITEM('Majors '),USE(?BrowseMajors),MSG('Browse Majors')
                           ITEM('Update Grades'),USE(?BrowseUpdateGrades)
                         END
                         MENU('&Reports'),USE(?ReportMenu),MSG('Report data')
                           ITEM('Student Class Schedules'),USE(?PrintENR:StuSeq),MSG('Print ordered by by Student Number')
                           ITEM('Teacher Class Schedules'),USE(?ReportsTeacherClassSchedules)
                           ITEM('Attendance Sheets'),USE(?PrintENR:SeqStu),MSG('Print ordered by by Class Number')
                           ITEM('Course Enrollment'),USE(?PrintCOU:KeyDescription),MSG('Print ordered by by Course' & |
  ' Description')
                           ITEM('Course Enrollment Summary'),USE(?ReportsCourseEnrollmentSummary)
                           ITEM('Final Grades'),USE(?ReportsFinalGrades)
                           ITEM('Student IDs'),USE(?ReportsStudentIDs)
                         END
                         MENU('&Window'),USE(?MENU4),MSG('Create and Arrange windows'),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),USE(?MENU5),MSG('Windows Help')
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'),STD(STD:HelpOnHelp)
                         END
                       END
                       TOOLBAR,AT(0,0,600,16),USE(?TOOLBAR1)
                         BUTTON,AT(2,2,16,14),USE(?Toolbar:Top, Toolbar:Top),ICON('VCRFIRST.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'First Page')
                         BUTTON,AT(18,2,16,14),USE(?Toolbar:PageUp, Toolbar:PageUp),ICON('VCRPRIOR.ICO'),DISABLE,FLAT, |
  TIP('Go to the Prior Page')
                         BUTTON,AT(34,2,16,14),USE(?Toolbar:Up, Toolbar:Up),ICON('VCRUP.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'Prior Record')
                         BUTTON,AT(50,2,16,14),USE(?Toolbar:Locate, Toolbar:Locate),ICON('FIND.ICO'),DISABLE,FLAT,TIP('Locate record')
                         BUTTON,AT(66,2,16,14),USE(?Toolbar:Down, Toolbar:Down),ICON('VCRDOWN.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'Next Record')
                         BUTTON,AT(82,2,16,14),USE(?Toolbar:PageDown, Toolbar:PageDown),ICON('VCRNEXT.ICO'),DISABLE, |
  FLAT,TIP('Go to the Next Page')
                         BUTTON,AT(98,2,16,14),USE(?Toolbar:Bottom, Toolbar:Bottom),ICON('VCRLAST.ICO'),DISABLE,FLAT, |
  TIP('Go to the Last Page')
                         BUTTON,AT(118,2,16,14),USE(?Toolbar:Select, Toolbar:Select),ICON('MARK.ICO'),DISABLE,FLAT, |
  TIP('Select This Record')
                         BUTTON,AT(134,2,16,14),USE(?Toolbar:Insert, Toolbar:Insert),ICON('INSERT.ICO'),DISABLE,FLAT, |
  TIP('Insert a New Record')
                         BUTTON,AT(150,2,16,14),USE(?Toolbar:Change, Toolbar:Change),ICON('EDIT.ICO'),DISABLE,FLAT, |
  TIP('Edit This Record')
                         BUTTON,AT(166,2,16,14),USE(?Toolbar:Delete, Toolbar:Delete),ICON('DELETE.ICO'),DISABLE,FLAT, |
  TIP('Delete This Record')
                         BUTTON,AT(186,2,16,14),USE(?Toolbar:History, Toolbar:History),ICON('DITTO.ICO'),DISABLE,FLAT, |
  TIP('Previous value')
                         BUTTON,AT(202,2,16,14),USE(?Toolbar:Help, Toolbar:Help),ICON('HELP.ICO'),DISABLE,FLAT,TIP('Get Help')
                       END
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
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
Menu::MENUBAR1 ROUTINE                                     ! Code for menu items on ?MENUBAR1
Menu::DevCon2019 ROUTINE                                   ! Code for menu items on ?DevCon2019
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
            '||Note: Any windows open now must be closed to work opening the Window Prv Class.'& |
            '||This APP has the CBWndPrvListFromQ Template Global Extension.' & |
             '||On any window with a LIST presss Ctrl+Shift+F1 to open CB Window Preview. ' & |
             '|On the CB wInspect Controls window press the LIST button' & |
             '|then press the From(Q) button and you can see the QUEUE that feeds the list.' & |
             '|Press the "View From(Q)" to see all the data.')
    
    !Was declared as Ref so no chance affects live APP until it is NEW() on the hot key
    !Help2WndPreviewCls  &CbWndPrvHelpHookClass  
  END
Menu::MENU1 ROUTINE                                        ! Code for menu items on ?MENU1
  CASE ACCEPTED()
  OF ?CreateDB
    CreateDB()
  END
Menu::MENU2 ROUTINE                                        ! Code for menu items on ?MENU2
Menu::Trees ROUTINE                                        ! Code for menu items on ?Trees
  CASE ACCEPTED()
  OF ?BrowseClassTree
    START(ClassTree, 25000)
  OF ?BrowseStudentsTree
    START(StudentTree, 25000)
  END
Menu::MENU3 ROUTINE                                        ! Code for menu items on ?MENU3
  CASE ACCEPTED()
  OF ?BrowseStudents
    START(BrowseStudents, 050000)
  OF ?BrowseTeachers
    START(BrowseTeachers, 050000)
  OF ?BrowseClasses
    START(BrowseClasses, 050000)
  OF ?BrowseEnrollment
    START(BrowseEnrollment, 050000)
  OF ?BrowseCourses
    START(BrowseCourses, 050000)
  OF ?BrowseMajors
    START(BrowseMajors, 050000)
  OF ?BrowseUpdateGrades
    START(UpdateGrades, 25000)
  END
Menu::ReportMenu ROUTINE                                   ! Code for menu items on ?ReportMenu
  CASE ACCEPTED()
  OF ?PrintENR:StuSeq
    START(ClassSchedules1, 050000)
  OF ?ReportsTeacherClassSchedules
    START(ClassSchedules2, 25000)
  OF ?PrintENR:SeqStu
    START(AttendanceSheets, 050000)
  OF ?PrintCOU:KeyDescription
    START(CourseEnrollment, 050000)
  OF ?ReportsCourseEnrollmentSummary
    START(EnrollSummary, 25000)
  OF ?ReportsFinalGrades
    START(FinalGrades, 25000)
  OF ?ReportsStudentIDs
    START(StudentIDs, 25000)
  END
Menu::MENU4 ROUTINE                                        ! Code for menu items on ?MENU4
Menu::MENU5 ROUTINE                                        ! Code for menu items on ?MENU5

ThisWindow.Ask PROCEDURE

  CODE
  SYSTEM{PROP:Icon} = '~_TpSpdU.ICO'
  IF NOT INRANGE(AppFrame{PROP:Timer},1,100)
    AppFrame{PROP:Timer} = 100
  END
    AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
    AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T3)
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
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
    OF ?Toolbar:Top
    OROF ?Toolbar:PageUp
    OROF ?Toolbar:Up
    OROF ?Toolbar:Locate
    OROF ?Toolbar:Down
    OROF ?Toolbar:PageDown
    OROF ?Toolbar:Bottom
    OROF ?Toolbar:Select
    OROF ?Toolbar:Insert
    OROF ?Toolbar:Change
    OROF ?Toolbar:Delete
    OROF ?Toolbar:History
    OROF ?Toolbar:Help
      IF SYSTEM{PROP:Active} <> THREAD()
        POST(EVENT:Accepted,ACCEPTED(),SYSTEM{Prop:Active} )
        CYCLE
      END
    ELSE
      DO Menu::MENUBAR1                                    ! Process menu items on ?MENUBAR1 menu
      DO Menu::DevCon2019                                  ! Process menu items on ?DevCon2019 menu
      DO Menu::MENU1                                       ! Process menu items on ?MENU1 menu
      DO Menu::MENU2                                       ! Process menu items on ?MENU2 menu
      DO Menu::Trees                                       ! Process menu items on ?Trees menu
      DO Menu::MENU3                                       ! Process menu items on ?MENU3 menu
      DO Menu::ReportMenu                                  ! Process menu items on ?ReportMenu menu
      DO Menu::MENU4                                       ! Process menu items on ?MENU4 menu
      DO Menu::MENU5                                       ! Process menu items on ?MENU5 menu
    END
  ReturnValue = PARENT.TakeAccepted()
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
    OF EVENT:OpenWindow
      SplashProcedureThread = START(SplashIt)              ! Run the splash window procedure
    OF EVENT:Timer
      AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
      AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T3)
    ELSE
      IF SplashProcedureThread
        IF EVENT() = Event:Accepted
          POST(Event:CloseWindow,,SplashProcedureThread)   ! Close the splash window
          SplashPRocedureThread = 0
        END
     END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Classes File
!!! </summary>
BrowseClasses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:CourseNumber)
                       PROJECT(CLA:TeacherNumber)
                       JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                         PROJECT(TEA:LastName)
                         PROJECT(TEA:Number)
                       END
                       JOIN(COU:KeyNumber,CLA:CourseNumber)
                         PROJECT(COU:Description)
                         PROJECT(COU:Number)
                       END
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
CLA:RoomNumber         LIKE(CLA:RoomNumber)           !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
CLA:CourseNumber       LIKE(CLA:CourseNumber)         !Browse key field - type derived from field
CLA:TeacherNumber      LIKE(CLA:TeacherNumber)        !Browse key field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Browse the Classes File'),AT(,,255,198),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  CENTER,GRAY,IMM,MDI,HLP('~BrowseClasses'),SYSTEM
                       LIST,AT(8,30,239,143),USE(?Browse:1),HVSCROLL,FORMAT('[49L(1)|M~Class Number~R@P##-####' & |
  '#P@120L|M~Course~@S30@/49R(1)|M~Room~@n4@80R(1)|M~Scheduled Time~R(0)@s20@]|M[80L|M~' & |
  'Instructor~L(2)@S20@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(57,158,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(106,158,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(155,158,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,247,172),USE(?CurrentTab)
                         TAB('by Class Number')
                         END
                         TAB('by Course Number')
                         END
                       END
                       BUTTON('Close'),AT(155,180,45,14),USE(?Close)
                       BUTTON('Help'),AT(205,180,45,14),USE(?Help),STD(STD:Help)
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
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Courses:Record)
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
  GlobalErrors.SetProcedureName('BrowseClasses')
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
  Access:Courses.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Teachers.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
   CBListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1') !Tpl CBWndPrvListFromQ
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:CourseNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,CLA:KeyCourseNumber) ! Add the sort order for CLA:KeyCourseNumber for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,CLA:CourseNumber,1,BRW1)       ! Initialize the browse locator using  using key: CLA:KeyCourseNumber , CLA:CourseNumber
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 2
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:ClassNumber for sort order 3
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,CLA:KeyClassNumber) ! Add the sort order for CLA:KeyClassNumber for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(,CLA:ClassNumber,1,BRW1)        ! Initialize the browse locator using  using key: CLA:KeyClassNumber , CLA:ClassNumber
  BRW1.AddField(CLA:ClassNumber,BRW1.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(CLA:RoomNumber,BRW1.Q.CLA:RoomNumber)      ! Field CLA:RoomNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ScheduledTime,BRW1.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(CLA:CourseNumber,BRW1.Q.CLA:CourseNumber)  ! Field CLA:CourseNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:TeacherNumber,BRW1.Q.CLA:TeacherNumber) ! Field CLA:TeacherNumber is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateClasses
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
    UpdateClasses
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

