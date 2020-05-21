   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
  INCLUDE('prnprop.clw')

   MAP
     MODULE('SCHOOL_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
     MODULE('SCHOOL001.CLW')
SelectClasses          PROCEDURE   !Select a Classes Record
UpdateEnrollment       PROCEDURE   !Update the Enrollment File
SelectStudents         PROCEDURE   !Select a Students Record
UpdateClasses          PROCEDURE   !Update the Classes File
SelectCourses          PROCEDURE   !Select a Courses Record
SelectTeachers         PROCEDURE   !Select a Teachers Record
UpdateCourses          PROCEDURE   !Update the Courses File
ClassTree              PROCEDURE   !
Main                   PROCEDURE   !Clarion for Windows Wizard Application
BrowseClasses          PROCEDURE   !Browse the Classes File
     END
     MODULE('SCHOOL002.CLW')
BrowseCourses          PROCEDURE   !Browse the Courses File
BrowseEnrollment       PROCEDURE   !Browse the Enrollment File
UpdateTeachers         PROCEDURE   !Update the Teachers File
SelectMajors           PROCEDURE   !Select a Majors Record
UpdateMajors           PROCEDURE   !Update the Majors File
UpdateStudents         PROCEDURE   !Update the Students File
BrowseMajors           PROCEDURE   !Browse the Majors File
BrowseStudents         PROCEDURE   !Browse the Students File
StudentTree            PROCEDURE   !
BrowseTeachers         PROCEDURE   !Browse the Teachers File
     END
     MODULE('SCHOOL003.CLW')
UpdateGrades           PROCEDURE   !
CourseEnrollment       PROCEDURE   !
AttendanceSheets       PROCEDURE   !
ClassSchedules1        PROCEDURE   !Student Schedules
EnrollSummary          PROCEDURE   !
FinalGrades            PROCEDURE   !
StudentIDs             PROCEDURE   !
ClassSchedules2        PROCEDURE   !Teacher Schedules
SplashIt               PROCEDURE   !
     END
     MODULE('SCHOOL004.CLW')
UGrades                PROCEDURE   !
CreateDB               PROCEDURE   !
CopyStudents           PROCEDURE   !Process the Students_TPS File
CopyEnrollments        PROCEDURE   !Process the Enrollment File
CopyMajors             PROCEDURE   !Process the Majors_TPS File
CopyClasses            PROCEDURE   !Process the Classes_TPS File
CopyCourses            PROCEDURE   !Process the Courses_TPS File
CopyTeachers           PROCEDURE   !Process the Teachers_TPS File
     END
   END

GLO:SQLiteTableName  STRING('school.sqlite {7}')
GLO:FileName         STRING(64)
AppFrameRef          &WINDOW
GLO:DropThread       LONG
GLO:DropControl      LONG
GLO:ThreadRef        &LONG
GLO:CameFrom         STRING(20)
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
Students             FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),PRE(STU),CREATE,BINDABLE,THREAD !                     
KeyStudentNumber         KEY(STU:Number),NOCASE,OPT,PRIMARY ! by Student Number   
MajorKey                 KEY(STU:Major,STU:LastName,STU:FirstName),DUP,NOCASE,OPT ! by Major            
KeyLastName              KEY(STU:LastName),DUP,NOCASE      ! by Last Name        
KeyGradYear              KEY(-STU:GradYear,STU:LastName,STU:FirstName),DUP,NOCASE,OPT ! by Grad Year        
DynoKey                  INDEX,NOCASE                      !                     
Photograph                  BLOB                           !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
Address2                    STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Major                       LONG                           !                     
GradYear                    LONG                           !                     
                         END
                     END                       

Teachers             FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),PRE(TEA),CREATE,BINDABLE,THREAD !                     
KeyTeacherNumber         KEY(TEA:Number),NOCASE,OPT,PRIMARY ! by Teacher Number   
KeyLastName              KEY(TEA:LastName),DUP,NOCASE      ! by Last Name        
KeyDepartment            KEY(TEA:Department),DUP,NOCASE,OPT ! by Department       
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Department                  LONG                           !                     
                         END
                     END                       

Classes              FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),PRE(CLA),CREATE,BINDABLE,THREAD !                     
KeyClassNumber           KEY(CLA:ClassNumber),NOCASE,OPT,PRIMARY ! by Class Number     
KeyCourseNumber          KEY(CLA:CourseNumber,CLA:ClassNumber),DUP,NOCASE ! by Course Number    
KeyTeacherNumber         KEY(CLA:TeacherNumber),DUP,NOCASE ! by Teacher Number   
Record                   RECORD,PRE()
ClassNumber                 LONG                           !                     
CourseNumber                LONG                           !                     
TeacherNumber               LONG                           !                     
RoomNumber                  LONG                           !                     
ScheduledTime               STRING(20)                     !                     
                         END
                     END                       

Enrollment           FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),PRE(ENR),CREATE,BINDABLE,THREAD !                     
StuSeq                   KEY(ENR:StudentNumber,ENR:ClassNumber),NOCASE,OPT ! by Student Number   
SeqStu                   KEY(ENR:ClassNumber,ENR:StudentNumber),NOCASE,OPT ! by Class Number     
Record                   RECORD,PRE()
StudentNumber               LONG                           !                     
ClassNumber                 LONG                           !                     
MidtermExam                 SHORT                          !                     
FinalExam                   SHORT                          !                     
TermPaper                   SHORT                          !                     
                         END
                     END                       

Courses              FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),PRE(COU),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(COU:Number),NOCASE,OPT,PRIMARY ! by Course Number    
KeyDescription           KEY(COU:Description),DUP,NOCASE   ! by Course Description
CompleteDescription         BLOB                           !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(40)                     !                     
                         END
                     END                       

Majors               FILE,DRIVER('SQLite'),OWNER(GLO:SQLiteTableName),NAME('majors'),PRE(MAJ),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(MAJ:Number),NOCASE,OPT,PRIMARY ! by Major Number     
KeyDescription           KEY(MAJ:Description),NOCASE,OPT   ! by Major Description
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(20)                     !                     
                         END
                     END                       

Students_TPS         FILE,DRIVER('TOPSPEED'),NAME('students'),PRE(STUTPS),CREATE,BINDABLE,THREAD !                     
KeyStudentNumber         KEY(STUTPS:Number),NOCASE,OPT,PRIMARY ! by Student Number   
MajorKey                 KEY(STUTPS:Major,STUTPS:LastName,STUTPS:FirstName),DUP,NOCASE,OPT ! by Major            
KeyLastName              KEY(STUTPS:LastName),DUP,NOCASE   ! by Last Name        
KeyGradYear              KEY(-STUTPS:GradYear,STUTPS:LastName,STUTPS:FirstName),DUP,NOCASE,OPT ! by Grad Year        
DynoKey                  INDEX,NOCASE                      !                     
Photograph                  BLOB                           !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
Address2                    STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Major                       LONG                           !                     
GradYear                    LONG                           !                     
                         END
                     END                       

Teachers_TPS         FILE,DRIVER('TOPSPEED'),NAME('teachers'),PRE(TEATPS),CREATE,BINDABLE,THREAD !                     
KeyTeacherNumber         KEY(TEATPS:Number),NOCASE,OPT,PRIMARY ! by Teacher Number   
KeyLastName              KEY(TEATPS:LastName),DUP,NOCASE   ! by Last Name        
KeyDepartment            KEY(TEATPS:Department),DUP,NOCASE,OPT ! by Department       
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Department                  LONG                           !                     
                         END
                     END                       

Classes_TPS          FILE,DRIVER('TOPSPEED'),NAME('classes'),PRE(CLATPS),CREATE,BINDABLE,THREAD !                     
KeyClassNumber           KEY(CLATPS:ClassNumber),NOCASE,OPT,PRIMARY ! by Class Number     
KeyCourseNumber          KEY(CLATPS:CourseNumber,CLATPS:ClassNumber),DUP,NOCASE ! by Course Number    
KeyTeacherNumber         KEY(CLATPS:TeacherNumber),DUP,NOCASE ! by Teacher Number   
Record                   RECORD,PRE()
ClassNumber                 LONG                           !                     
CourseNumber                LONG                           !                     
TeacherNumber               LONG                           !                     
RoomNumber                  LONG                           !                     
ScheduledTime               STRING(20)                     !                     
                         END
                     END                       

Enrollment_TPS       FILE,DRIVER('TOPSPEED'),NAME('enrollme'),PRE(ENRTPS),CREATE,BINDABLE,THREAD !                     
StuSeq                   KEY(ENRTPS:StudentNumber,ENRTPS:ClassNumber),NOCASE,OPT ! by Student Number   
SeqStu                   KEY(ENRTPS:ClassNumber,ENRTPS:StudentNumber),NOCASE,OPT ! by Class Number     
Record                   RECORD,PRE()
StudentNumber               LONG                           !                     
ClassNumber                 LONG                           !                     
MidtermExam                 SHORT                          !                     
FinalExam                   SHORT                          !                     
TermPaper                   SHORT                          !                     
                         END
                     END                       

Courses_TPS          FILE,DRIVER('TOPSPEED'),NAME('courses'),PRE(COUTPS),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(COUTPS:Number),NOCASE,OPT,PRIMARY ! by Course Number    
KeyDescription           KEY(COUTPS:Description),DUP,NOCASE ! by Course Description
CompleteDescription         MEMO(1000)                     !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(40)                     !                     
                         END
                     END                       

Majors_TPS           FILE,DRIVER('TOPSPEED'),NAME('majors.tps'),PRE(MAJTPS),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(MAJTPS:Number),NOCASE,OPT,PRIMARY ! by Major Number     
KeyDescription           KEY(MAJTPS:Description),NOCASE,OPT ! by Major Description
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(20)                     !                     
                         END
                     END                       

!endregion

Access:Students      &FileManager,THREAD                   ! FileManager for Students
Relate:Students      &RelationManager,THREAD               ! RelationManager for Students
Access:Teachers      &FileManager,THREAD                   ! FileManager for Teachers
Relate:Teachers      &RelationManager,THREAD               ! RelationManager for Teachers
Access:Classes       &FileManager,THREAD                   ! FileManager for Classes
Relate:Classes       &RelationManager,THREAD               ! RelationManager for Classes
Access:Enrollment    &FileManager,THREAD                   ! FileManager for Enrollment
Relate:Enrollment    &RelationManager,THREAD               ! RelationManager for Enrollment
Access:Courses       &FileManager,THREAD                   ! FileManager for Courses
Relate:Courses       &RelationManager,THREAD               ! RelationManager for Courses
Access:Majors        &FileManager,THREAD                   ! FileManager for Majors
Relate:Majors        &RelationManager,THREAD               ! RelationManager for Majors
Access:Students_TPS  &FileManager,THREAD                   ! FileManager for Students_TPS
Relate:Students_TPS  &RelationManager,THREAD               ! RelationManager for Students_TPS
Access:Teachers_TPS  &FileManager,THREAD                   ! FileManager for Teachers_TPS
Relate:Teachers_TPS  &RelationManager,THREAD               ! RelationManager for Teachers_TPS
Access:Classes_TPS   &FileManager,THREAD                   ! FileManager for Classes_TPS
Relate:Classes_TPS   &RelationManager,THREAD               ! RelationManager for Classes_TPS
Access:Enrollment_TPS &FileManager,THREAD                  ! FileManager for Enrollment_TPS
Relate:Enrollment_TPS &RelationManager,THREAD              ! RelationManager for Enrollment_TPS
Access:Courses_TPS   &FileManager,THREAD                   ! FileManager for Courses_TPS
Relate:Courses_TPS   &RelationManager,THREAD               ! RelationManager for Courses_TPS
Access:Majors_TPS    &FileManager,THREAD                   ! FileManager for Majors_TPS
Relate:Majors_TPS    &RelationManager,THREAD               ! RelationManager for Majors_TPS

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  HELP('SCHOOL.CHM')                                       ! Open the applications help file
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\SCHOOL.INI', NVD_INI)                     ! Configure INIManager to use INI file
  DctInit()
  Main
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

