

   MEMBER('SCHOOL.clw')                                    ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE

!!! <summary>
!!! Procedure not yet defined
!!! </summary>
UGrades PROCEDURE !Procedure not yet defined
  CODE
  GlobalErrors.ThrowMessage(Msg:ProcedureToDo,'UGrades')   ! This procedure acts as a place holder for a procedure yet to be defined
  SETKEYCODE(0)
  GlobalResponse = RequestCancelled                        ! Request cancelled is the implied action
!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
CreateDB             PROCEDURE                             ! Declare Procedure
FilesOpened     BYTE(0)

  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Classes_TPS:Record)
? DEBUGHOOK(Courses:Record)
? DEBUGHOOK(Courses_TPS:Record)
? DEBUGHOOK(Enrollment:Record)
? DEBUGHOOK(Enrollment_TPS:Record)
? DEBUGHOOK(Majors:Record)
? DEBUGHOOK(Majors_TPS:Record)
? DEBUGHOOK(Students:Record)
? DEBUGHOOK(Students_TPS:Record)
? DEBUGHOOK(Teachers:Record)
? DEBUGHOOK(Teachers_TPS:Record)
  REMOVE(GLO:SQLiteTableName)
  CREATE(Teachers)
  CREATE(Students)
  CREATE(Majors)
  CREATE(Enrollment)
  CREATE(Courses)
  CREATE(Classes)
  DO OpenFiles
  CopyClasses()
  CopyCourses()
  CopyEnrollments()
  CopyMajors()
  CopyStudents()
  CopyTeachers()
  DO CloseFiles
!--------------------------------------
OpenFiles  ROUTINE
  Access:Teachers.Open()                                   ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Teachers.UseFile()                                ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Students.Open()                                   ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Students.UseFile()                                ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Majors.Open()                                     ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Majors.UseFile()                                  ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Enrollment.Open()                                 ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Enrollment.UseFile()                              ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Courses.Open()                                    ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Courses.UseFile()                                 ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Classes.Open()                                    ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Classes.UseFile()                                 ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Teachers_TPS.Open()                               ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Teachers_TPS.UseFile()                            ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Students_TPS.Open()                               ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Students_TPS.UseFile()                            ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Majors_TPS.Open()                                 ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Majors_TPS.UseFile()                              ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Enrollment_TPS.Open()                             ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Enrollment_TPS.UseFile()                          ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Courses_TPS.Open()                                ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Courses_TPS.UseFile()                             ! Use File referenced in 'Other Files' so need to inform it's FileManager
  Access:Classes_TPS.Open()                                ! Open File referenced in 'Other Files' so need to inform it's FileManager
  Access:Classes_TPS.UseFile()                             ! Use File referenced in 'Other Files' so need to inform it's FileManager
  FilesOpened = True
!--------------------------------------
CloseFiles ROUTINE
  IF FilesOpened THEN
     Access:Teachers.Close()
     Access:Students.Close()
     Access:Majors.Close()
     Access:Enrollment.Close()
     Access:Courses.Close()
     Access:Classes.Close()
     Access:Teachers_TPS.Close()
     Access:Students_TPS.Close()
     Access:Majors_TPS.Close()
     Access:Enrollment_TPS.Close()
     Access:Courses_TPS.Close()
     Access:Classes_TPS.Close()
     FilesOpened = False
  END
!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Students_TPS File
!!! </summary>
CopyStudents PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Students_TPS)
                     END
ProgressWindow       WINDOW('Process Students_TPS'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Students:Record)
? DEBUGHOOK(Students_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyStudents')
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
  Relate:Students_TPS.Open()                               ! File Students_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Students_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Students_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close()
    Relate:Students_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  STU:Record = STUTPS:Record
  ADD(Students)
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Enrollment File
!!! </summary>
CopyEnrollments PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Enrollment_TPS)
                     END
ProgressWindow       WINDOW('Process Enrollment'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Enrollment:Record)
? DEBUGHOOK(Enrollment_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyEnrollments')
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
  Relate:Enrollment_TPS.Open()                             ! File Enrollment_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Enrollment_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Enrollment_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Enrollment.Close()
    Relate:Enrollment_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ENR:Record = ENRTPS:Record
  ADD(Enrollment)
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Majors_TPS File
!!! </summary>
CopyMajors PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Majors_TPS)
                     END
ProgressWindow       WINDOW('Process Majors_TPS'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Majors:Record)
? DEBUGHOOK(Majors_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open()                                     ! File Majors used by this procedure, so make sure it's RelationManager is open
  Relate:Majors_TPS.Open()                                 ! File Majors_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Majors_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Majors_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Majors.Close()
    Relate:Majors_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MAJ:Record = MAJTPS:Record
  ADD(Majors)
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Classes_TPS File
!!! </summary>
CopyClasses PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Classes_TPS)
                     END
ProgressWindow       WINDOW('Copying Classes_TPS'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Classes:Record)
? DEBUGHOOK(Classes_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open()                                    ! File Classes used by this procedure, so make sure it's RelationManager is open
  Relate:Classes_TPS.Open()                                ! File Classes_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Classes_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Classes_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close()
    Relate:Classes_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF CLATPS:ClassNumber <> 0
    CLA:Record = CLATPS:Record
    ADD(Classes)
  END
  
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Courses_TPS File
!!! </summary>
CopyCourses PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses_TPS)
                     END
ProgressWindow       WINDOW('Process Courses_TPS'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Courses:Record)
? DEBUGHOOK(Courses_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyCourses')
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
  Relate:Courses_TPS.Open()                                ! File Courses_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Courses_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Courses_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close()
    Relate:Courses_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  COU:Record = COUTPS:Record
  ADD(Courses)
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Process
!!! Process the Teachers_TPS File
!!! </summary>
CopyTeachers PROCEDURE 

Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Teachers_TPS)
                     END
ProgressWindow       WINDOW('Process Teachers_TPS'),AT(,,142,59),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT), |
  DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(46,42,49,15),USE(?Progress:Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Process'), |
  TIP('Cancel Process')
                     END

ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisProcess          CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepClass                             ! Progress Manager

  CODE
? DEBUGHOOK(Teachers:Record)
? DEBUGHOOK(Teachers_TPS:Record)
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
  GlobalErrors.SetProcedureName('CopyTeachers')
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
  Relate:Teachers_TPS.Open()                               ! File Teachers_TPS used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressWindow{Prop:Timer} = 10                          ! Assign timer interval
  ThisProcess.Init(Process:View, Relate:Teachers_TPS, ?Progress:PctText, Progress:Thermometer)
  ThisProcess.AddSortOrder()
  ProgressWindow{Prop:Text} = 'Processing Records'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  SELF.Init(ThisProcess)
  ?Progress:UserString{Prop:Text}=''
  SELF.AddItem(?Progress:Cancel, RequestCancelled)
  SEND(Teachers_TPS,'QUICKSCAN=on')
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Teachers.Close()
    Relate:Teachers_TPS.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisProcess.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  TEA:Record = TEATPS:Record
  ADD(Teachers)
  ReturnValue = PARENT.TakeRecord()
  RETURN ReturnValue

