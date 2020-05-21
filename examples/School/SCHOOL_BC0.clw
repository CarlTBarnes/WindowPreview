  MEMBER('SCHOOL.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
SCHOOL_BC0:DctInit    PROCEDURE()
SCHOOL_BC0:DctKill    PROCEDURE()
SCHOOL_BC0:FilesInit  PROCEDURE()
  END

Hide:Access:Students CLASS(FileManager),TYPE               ! FileManager for Students
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:Students CLASS(RelationManager),TYPE           ! RelationManager for Students
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Teachers CLASS(FileManager),TYPE               ! FileManager for Teachers
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:Teachers CLASS(RelationManager),TYPE           ! RelationManager for Teachers
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Classes  CLASS(FileManager),TYPE               ! FileManager for Classes
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:Classes  CLASS(RelationManager),TYPE           ! RelationManager for Classes
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Enrollment CLASS(FileManager),TYPE             ! FileManager for Enrollment
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:Enrollment CLASS(RelationManager),TYPE         ! RelationManager for Enrollment
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Courses  CLASS(FileManager),TYPE               ! FileManager for Courses
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Courses  CLASS(RelationManager),TYPE           ! RelationManager for Courses
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Majors   CLASS(FileManager),TYPE               ! FileManager for Majors
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Majors   CLASS(RelationManager),TYPE           ! RelationManager for Majors
Init                   PROCEDURE
DeferedAddRelations    PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Students_TPS CLASS(FileManager),TYPE           ! FileManager for Students_TPS
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Students_TPS CLASS(RelationManager),TYPE       ! RelationManager for Students_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Teachers_TPS CLASS(FileManager),TYPE           ! FileManager for Teachers_TPS
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Teachers_TPS CLASS(RelationManager),TYPE       ! RelationManager for Teachers_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Classes_TPS CLASS(FileManager),TYPE            ! FileManager for Classes_TPS
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Classes_TPS CLASS(RelationManager),TYPE        ! RelationManager for Classes_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Enrollment_TPS CLASS(FileManager),TYPE         ! FileManager for Enrollment_TPS
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
ValidateFieldServer    PROCEDURE(UNSIGNED Id,BYTE HandleErrors),BYTE,PROC,DERIVED
                     END


Hide:Relate:Enrollment_TPS CLASS(RelationManager),TYPE     ! RelationManager for Enrollment_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Courses_TPS CLASS(FileManager),TYPE            ! FileManager for Courses_TPS
BindFields             PROCEDURE(),DERIVED
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Courses_TPS CLASS(RelationManager),TYPE        ! RelationManager for Courses_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:Majors_TPS CLASS(FileManager),TYPE             ! FileManager for Majors_TPS
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:Majors_TPS CLASS(RelationManager),TYPE         ! RelationManager for Majors_TPS
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:Students &Hide:Access:Students,AUTO,THREAD
_Hide:Relate:Students &Hide:Relate:Students,AUTO,THREAD
_Hide:Access:Teachers &Hide:Access:Teachers,AUTO,THREAD
_Hide:Relate:Teachers &Hide:Relate:Teachers,AUTO,THREAD
_Hide:Access:Classes &Hide:Access:Classes,AUTO,THREAD
_Hide:Relate:Classes &Hide:Relate:Classes,AUTO,THREAD
_Hide:Access:Enrollment &Hide:Access:Enrollment,AUTO,THREAD
_Hide:Relate:Enrollment &Hide:Relate:Enrollment,AUTO,THREAD
_Hide:Access:Courses &Hide:Access:Courses,AUTO,THREAD
_Hide:Relate:Courses &Hide:Relate:Courses,AUTO,THREAD
_Hide:Access:Majors  &Hide:Access:Majors,AUTO,THREAD
_Hide:Relate:Majors  &Hide:Relate:Majors,AUTO,THREAD
_Hide:Access:Students_TPS &Hide:Access:Students_TPS,AUTO,THREAD
_Hide:Relate:Students_TPS &Hide:Relate:Students_TPS,AUTO,THREAD
_Hide:Access:Teachers_TPS &Hide:Access:Teachers_TPS,AUTO,THREAD
_Hide:Relate:Teachers_TPS &Hide:Relate:Teachers_TPS,AUTO,THREAD
_Hide:Access:Classes_TPS &Hide:Access:Classes_TPS,AUTO,THREAD
_Hide:Relate:Classes_TPS &Hide:Relate:Classes_TPS,AUTO,THREAD
_Hide:Access:Enrollment_TPS &Hide:Access:Enrollment_TPS,AUTO,THREAD
_Hide:Relate:Enrollment_TPS &Hide:Relate:Enrollment_TPS,AUTO,THREAD
_Hide:Access:Courses_TPS &Hide:Access:Courses_TPS,AUTO,THREAD
_Hide:Relate:Courses_TPS &Hide:Relate:Courses_TPS,AUTO,THREAD
_Hide:Access:Majors_TPS &Hide:Access:Majors_TPS,AUTO,THREAD
_Hide:Relate:Majors_TPS &Hide:Relate:Majors_TPS,AUTO,THREAD


SCHOOL_BC0:DctInit PROCEDURE()
  CODE
  _Hide:Access:Students &= NEW(Hide:Access:Students)
  _Hide:Relate:Students &= NEW(Hide:Relate:Students)
  _Hide:Access:Teachers &= NEW(Hide:Access:Teachers)
  _Hide:Relate:Teachers &= NEW(Hide:Relate:Teachers)
  _Hide:Access:Classes &= NEW(Hide:Access:Classes)
  _Hide:Relate:Classes &= NEW(Hide:Relate:Classes)
  _Hide:Access:Enrollment &= NEW(Hide:Access:Enrollment)
  _Hide:Relate:Enrollment &= NEW(Hide:Relate:Enrollment)
  _Hide:Access:Courses &= NEW(Hide:Access:Courses)
  _Hide:Relate:Courses &= NEW(Hide:Relate:Courses)
  _Hide:Access:Majors &= NEW(Hide:Access:Majors)
  _Hide:Relate:Majors &= NEW(Hide:Relate:Majors)
  _Hide:Access:Students_TPS &= NEW(Hide:Access:Students_TPS)
  _Hide:Relate:Students_TPS &= NEW(Hide:Relate:Students_TPS)
  _Hide:Access:Teachers_TPS &= NEW(Hide:Access:Teachers_TPS)
  _Hide:Relate:Teachers_TPS &= NEW(Hide:Relate:Teachers_TPS)
  _Hide:Access:Classes_TPS &= NEW(Hide:Access:Classes_TPS)
  _Hide:Relate:Classes_TPS &= NEW(Hide:Relate:Classes_TPS)
  _Hide:Access:Enrollment_TPS &= NEW(Hide:Access:Enrollment_TPS)
  _Hide:Relate:Enrollment_TPS &= NEW(Hide:Relate:Enrollment_TPS)
  _Hide:Access:Courses_TPS &= NEW(Hide:Access:Courses_TPS)
  _Hide:Relate:Courses_TPS &= NEW(Hide:Relate:Courses_TPS)
  _Hide:Access:Majors_TPS &= NEW(Hide:Access:Majors_TPS)
  _Hide:Relate:Majors_TPS &= NEW(Hide:Relate:Majors_TPS)
  Relate:Students &= _Hide:Relate:Students
  Relate:Teachers &= _Hide:Relate:Teachers
  Relate:Classes &= _Hide:Relate:Classes
  Relate:Enrollment &= _Hide:Relate:Enrollment
  Relate:Courses &= _Hide:Relate:Courses
  Relate:Majors &= _Hide:Relate:Majors
  Relate:Students_TPS &= _Hide:Relate:Students_TPS
  Relate:Teachers_TPS &= _Hide:Relate:Teachers_TPS
  Relate:Classes_TPS &= _Hide:Relate:Classes_TPS
  Relate:Enrollment_TPS &= _Hide:Relate:Enrollment_TPS
  Relate:Courses_TPS &= _Hide:Relate:Courses_TPS
  Relate:Majors_TPS &= _Hide:Relate:Majors_TPS


SCHOOL_BC0:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:Students.Init
  _Hide:Relate:Teachers.Init
  _Hide:Relate:Classes.Init
  _Hide:Relate:Enrollment.Init
  _Hide:Relate:Courses.Init
  _Hide:Relate:Majors.Init
  _Hide:Relate:Students_TPS.Init
  _Hide:Relate:Teachers_TPS.Init
  _Hide:Relate:Classes_TPS.Init
  _Hide:Relate:Enrollment_TPS.Init
  _Hide:Relate:Courses_TPS.Init
  _Hide:Relate:Majors_TPS.Init


SCHOOL_BC0:DctKill PROCEDURE()
  CODE
  _Hide:Relate:Students.Kill
  DISPOSE(_Hide:Relate:Students)
  _Hide:Relate:Teachers.Kill
  DISPOSE(_Hide:Relate:Teachers)
  _Hide:Relate:Classes.Kill
  DISPOSE(_Hide:Relate:Classes)
  _Hide:Relate:Enrollment.Kill
  DISPOSE(_Hide:Relate:Enrollment)
  _Hide:Relate:Courses.Kill
  DISPOSE(_Hide:Relate:Courses)
  _Hide:Relate:Majors.Kill
  DISPOSE(_Hide:Relate:Majors)
  _Hide:Relate:Students_TPS.Kill
  DISPOSE(_Hide:Relate:Students_TPS)
  _Hide:Relate:Teachers_TPS.Kill
  DISPOSE(_Hide:Relate:Teachers_TPS)
  _Hide:Relate:Classes_TPS.Kill
  DISPOSE(_Hide:Relate:Classes_TPS)
  _Hide:Relate:Enrollment_TPS.Kill
  DISPOSE(_Hide:Relate:Enrollment_TPS)
  _Hide:Relate:Courses_TPS.Kill
  DISPOSE(_Hide:Relate:Courses_TPS)
  _Hide:Relate:Majors_TPS.Kill
  DISPOSE(_Hide:Relate:Majors_TPS)


Hide:Relate:Students.Init PROCEDURE()
  CODE
  _Hide:Access:Students.Init
  SELF.Init(Access:Students,1)


Hide:Access:Students.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= STU:Record
  SELF.FileNameValue = 'Students'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Students
  PARENT.Init
  Access:Students &= SELF


Hide:Access:Students.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Students &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Students.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Students,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(STU:KeyStudentNumber,'by Student Number',0)
    SELF.AddKey(STU:MajorKey,'by Major',0)
    SELF.AddKey(STU:KeyLastName,'by Last Name',0)
    SELF.AddKey(STU:KeyGradYear,'by Grad Year',0)
    SELF.AddKey(STU:DynoKey,'STU:DynoKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:Students.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:Majors10      USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 10
    GlobalErrors.SetField('STU:Major')
    !LONG
    IF STU:Major <> 0
    Save:Majors10 = Access:Majors.SaveFile()
    MAJ:Number = STU:Major
    ReturnValue = Access:Majors.TryFetch(MAJ:KeyNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Majors')
      END
    END
    Access:Majors.RestoreFile(Save:Majors10)
    END! End IsNotNull
  END
  RETURN ReturnValue


Hide:Relate:Students.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Enrollment,RI:CASCADE,RI:RESTRICT,ENR:StuSeq)
  SELF.AddRelationLink(STU:Number,ENR:StudentNumber)

  SELF.AddRelation(Relate:Majors)


Hide:Relate:Students.Kill PROCEDURE

  CODE
  _Hide:Access:Students.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:Students &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Students)                           ! destroy the file manager


Hide:Relate:Teachers.Init PROCEDURE()
  CODE
  _Hide:Access:Teachers.Init
  SELF.Init(Access:Teachers,1)


Hide:Access:Teachers.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TEA:Record
  SELF.FileNameValue = 'Teachers'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Teachers
  PARENT.Init
  Access:Teachers &= SELF


Hide:Access:Teachers.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Teachers &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Teachers.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Teachers,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(TEA:KeyTeacherNumber,'by Teacher Number',0)
    SELF.AddKey(TEA:KeyLastName,'by Last Name',0)
    SELF.AddKey(TEA:KeyDepartment,'by Department',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:Teachers.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:Majors9       USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 9
    GlobalErrors.SetField('TEA:Department')
    !LONG
    IF TEA:Department <> 0
    Save:Majors9 = Access:Majors.SaveFile()
    MAJ:Number = TEA:Department
    ReturnValue = Access:Majors.TryFetch(MAJ:KeyNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Majors')
      END
    END
    Access:Majors.RestoreFile(Save:Majors9)
    END! End IsNotNull
  END
  RETURN ReturnValue


Hide:Relate:Teachers.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Majors)

  SELF.AddRelation(Relate:Classes,RI:CASCADE,RI:RESTRICT,CLA:KeyTeacherNumber)
  SELF.AddRelationLink(TEA:Number,CLA:TeacherNumber)


Hide:Relate:Teachers.Kill PROCEDURE

  CODE
  _Hide:Access:Teachers.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:Teachers &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Teachers)                           ! destroy the file manager


Hide:Relate:Classes.Init PROCEDURE()
  CODE
  _Hide:Access:Classes.Init
  SELF.Init(Access:Classes,1)


Hide:Access:Classes.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= CLA:Record
  SELF.FileNameValue = 'Classes'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Classes
  PARENT.Init
  Access:Classes &= SELF


Hide:Access:Classes.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Classes &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Classes.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Classes,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(CLA:KeyClassNumber,'by Class Number',0)
    SELF.AddKey(CLA:KeyCourseNumber,'by Course Number',0)
    SELF.AddKey(CLA:KeyTeacherNumber,'by Teacher Number',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:Classes.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:Courses2      USHORT,AUTO
Save:Teachers3     USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 2
    GlobalErrors.SetField('CLA:CourseNumber')
    !LONG
    IF CLA:CourseNumber <> 0
    Save:Courses2 = Access:Courses.SaveFile()
    COU:Number = CLA:CourseNumber
    ReturnValue = Access:Courses.TryFetch(COU:KeyNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Courses')
      END
    END
    Access:Courses.RestoreFile(Save:Courses2)
    END! End IsNotNull
  OF 3
    GlobalErrors.SetField('CLA:TeacherNumber')
    !LONG
    IF CLA:TeacherNumber <> 0
    Save:Teachers3 = Access:Teachers.SaveFile()
    TEA:Number = CLA:TeacherNumber
    ReturnValue = Access:Teachers.TryFetch(TEA:KeyTeacherNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Teachers')
      END
    END
    Access:Teachers.RestoreFile(Save:Teachers3)
    END! End IsNotNull
  END
  RETURN ReturnValue


Hide:Relate:Classes.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Courses)

  SELF.AddRelation(Relate:Enrollment,RI:CASCADE,RI:RESTRICT,ENR:SeqStu)
  SELF.AddRelationLink(CLA:ClassNumber,ENR:ClassNumber)

  SELF.AddRelation(Relate:Teachers)


Hide:Relate:Classes.Kill PROCEDURE

  CODE
  _Hide:Access:Classes.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:Classes &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Classes)                            ! destroy the file manager


Hide:Relate:Enrollment.Init PROCEDURE()
  CODE
  _Hide:Access:Enrollment.Init
  SELF.Init(Access:Enrollment,1)


Hide:Access:Enrollment.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= ENR:Record
  SELF.FileNameValue = 'Enrollment'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Enrollment
  PARENT.Init
  Access:Enrollment &= SELF


Hide:Access:Enrollment.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Enrollment &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Enrollment.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Enrollment,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(ENR:StuSeq,'by Student Number',0)
    SELF.AddKey(ENR:SeqStu,'by Class Number',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:Enrollment.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

Save:Students1     USHORT,AUTO
Save:Classes2      USHORT,AUTO
  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 1
    GlobalErrors.SetField('ENR:StudentNumber')
    !LONG
    IF ENR:StudentNumber <> 0
    Save:Students1 = Access:Students.SaveFile()
    STU:Number = ENR:StudentNumber
    ReturnValue = Access:Students.TryFetch(STU:KeyStudentNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Students')
      END
    END
    Access:Students.RestoreFile(Save:Students1)
    END! End IsNotNull
  OF 2
    GlobalErrors.SetField('ENR:ClassNumber')
    !LONG
    IF ENR:ClassNumber <> 0
    Save:Classes2 = Access:Classes.SaveFile()
    CLA:ClassNumber = ENR:ClassNumber
    ReturnValue = Access:Classes.TryFetch(CLA:KeyClassNumber)
    IF ReturnValue <> Level:Benign
      IF HandleErrors                                      ! Display message if handling errors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldNotInFile,'Classes')
      END
    END
    Access:Classes.RestoreFile(Save:Classes2)
    END! End IsNotNull
  OF 3
    GlobalErrors.SetField('ENR:MidtermExam')
    IF NOT INRANGE(ENR:MidtermExam,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  OF 4
    GlobalErrors.SetField('ENR:FinalExam')
    IF NOT INRANGE(ENR:FinalExam,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  OF 5
    GlobalErrors.SetField('ENR:TermPaper')
    IF NOT INRANGE(ENR:TermPaper,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  END
  RETURN ReturnValue


Hide:Relate:Enrollment.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Students)

  SELF.AddRelation(Relate:Classes)


Hide:Relate:Enrollment.Kill PROCEDURE

  CODE
  _Hide:Access:Enrollment.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:Enrollment &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Enrollment)                         ! destroy the file manager


Hide:Relate:Courses.Init PROCEDURE()
  CODE
  _Hide:Access:Courses.Init
  SELF.Init(Access:Courses,1)


Hide:Access:Courses.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= COU:Record
  SELF.FileNameValue = 'Courses'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Courses
  PARENT.Init
  Access:Courses &= SELF


Hide:Access:Courses.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Courses &= NULL                                   ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Courses.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Courses,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(COU:KeyNumber,'by Course Number',1)
    SELF.AddKey(COU:KeyDescription,'by Course Description',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Courses.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Classes,RI:CASCADE,RI:RESTRICT,CLA:KeyCourseNumber)
  SELF.AddRelationLink(COU:Number,CLA:CourseNumber)


Hide:Relate:Courses.Kill PROCEDURE

  CODE
  _Hide:Access:Courses.Kill                                ! Kill the file manager
  PARENT.Kill
  Relate:Courses &= NULL                                   ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Courses)                            ! destroy the file manager


Hide:Relate:Majors.Init PROCEDURE()
  CODE
  _Hide:Access:Majors.Init
  SELF.Init(Access:Majors,1)


Hide:Access:Majors.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= MAJ:Record
  SELF.FileNameValue = 'Majors'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Majors
  PARENT.Init
  Access:Majors &= SELF


Hide:Access:Majors.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Majors &= NULL                                    ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Majors.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Majors,GlobalErrors)
    SELF.LockRecover = 10
    SELF.AddKey(MAJ:KeyNumber,'by Major Number',0)
    SELF.AddKey(MAJ:KeyDescription,'by Major Description',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Majors.DeferedAddRelations PROCEDURE

  CODE
  DO AddRelations_1
  
  PARENT.DeferedAddRelations

AddRelations_1 ROUTINE
  SELF.AddRelation(Relate:Teachers,RI:CASCADE,RI:RESTRICT,TEA:KeyDepartment)
  SELF.AddRelationLink(MAJ:Number,TEA:Department)

  SELF.AddRelation(Relate:Students,RI:CASCADE,RI:RESTRICT,STU:MajorKey)
  SELF.AddRelationLink(MAJ:Number,STU:Major)


Hide:Relate:Majors.Kill PROCEDURE

  CODE
  _Hide:Access:Majors.Kill                                 ! Kill the file manager
  PARENT.Kill
  Relate:Majors &= NULL                                    ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Majors)                             ! destroy the file manager


Hide:Relate:Students_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Students_TPS.Init
  SELF.Init(Access:Students_TPS,1)


Hide:Access:Students_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= STUTPS:Record
  SELF.FileNameValue = 'Students_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Students_TPS
  PARENT.Init
  Access:Students_TPS &= SELF


Hide:Access:Students_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Students_TPS &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Students_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Students_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(STUTPS:KeyStudentNumber,'by Student Number',0)
    SELF.AddKey(STUTPS:MajorKey,'by Major',0)
    SELF.AddKey(STUTPS:KeyLastName,'by Last Name',0)
    SELF.AddKey(STUTPS:KeyGradYear,'by Grad Year',0)
    SELF.AddKey(STUTPS:DynoKey,'STUTPS:DynoKey',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Students_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Students_TPS.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:Students_TPS &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Students_TPS)                       ! destroy the file manager


Hide:Relate:Teachers_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Teachers_TPS.Init
  SELF.Init(Access:Teachers_TPS,1)


Hide:Access:Teachers_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= TEATPS:Record
  SELF.FileNameValue = 'Teachers_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Teachers_TPS
  PARENT.Init
  Access:Teachers_TPS &= SELF


Hide:Access:Teachers_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Teachers_TPS &= NULL                              ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Teachers_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Teachers_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(TEATPS:KeyTeacherNumber,'by Teacher Number',0)
    SELF.AddKey(TEATPS:KeyLastName,'by Last Name',0)
    SELF.AddKey(TEATPS:KeyDepartment,'by Department',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Teachers_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Teachers_TPS.Kill                           ! Kill the file manager
  PARENT.Kill
  Relate:Teachers_TPS &= NULL                              ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Teachers_TPS)                       ! destroy the file manager


Hide:Relate:Classes_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Classes_TPS.Init
  SELF.Init(Access:Classes_TPS,1)


Hide:Access:Classes_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= CLATPS:Record
  SELF.FileNameValue = 'Classes_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Classes_TPS
  PARENT.Init
  Access:Classes_TPS &= SELF


Hide:Access:Classes_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Classes_TPS &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Classes_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Classes_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(CLATPS:KeyClassNumber,'by Class Number',0)
    SELF.AddKey(CLATPS:KeyCourseNumber,'by Course Number',0)
    SELF.AddKey(CLATPS:KeyTeacherNumber,'by Teacher Number',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Classes_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Classes_TPS.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:Classes_TPS &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Classes_TPS)                        ! destroy the file manager


Hide:Relate:Enrollment_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Enrollment_TPS.Init
  SELF.Init(Access:Enrollment_TPS,1)


Hide:Access:Enrollment_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= ENRTPS:Record
  SELF.FileNameValue = 'Enrollment_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Enrollment_TPS
  PARENT.Init
  Access:Enrollment_TPS &= SELF


Hide:Access:Enrollment_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Enrollment_TPS &= NULL                            ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Enrollment_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Enrollment_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(ENRTPS:StuSeq,'by Student Number',0)
    SELF.AddKey(ENRTPS:SeqStu,'by Class Number',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Access:Enrollment_TPS.ValidateFieldServer PROCEDURE(UNSIGNED Id,BYTE HandleErrors)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.ValidateFieldServer(Id,HandleErrors)
  CASE Id
  OF 3
    GlobalErrors.SetField('ENRTPS:MidtermExam')
    IF NOT INRANGE(ENRTPS:MidtermExam,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  OF 4
    GlobalErrors.SetField('ENRTPS:FinalExam')
    IF NOT INRANGE(ENRTPS:FinalExam,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  OF 5
    GlobalErrors.SetField('ENRTPS:TermPaper')
    IF NOT INRANGE(ENRTPS:TermPaper,0,100)
      ReturnValue = Level:Notify
    END
    IF ReturnValue <> Level:Benign
      IF HandleErrors
        ReturnValue = GlobalErrors.ThrowMessage(Msg:FieldOutOfRange,'0 .. 100')
      END
    END
  END
  RETURN ReturnValue


Hide:Relate:Enrollment_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Enrollment_TPS.Kill                         ! Kill the file manager
  PARENT.Kill
  Relate:Enrollment_TPS &= NULL                            ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Enrollment_TPS)                     ! destroy the file manager


Hide:Relate:Courses_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Courses_TPS.Init
  SELF.Init(Access:Courses_TPS,1)


Hide:Access:Courses_TPS.BindFields PROCEDURE

  CODE
  PARENT.BindFields
  BIND('COUTPS:CompleteDescription',COUTPS:CompleteDescription) ! Bind memo field


Hide:Access:Courses_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= COUTPS:Record
  SELF.FileNameValue = 'Courses_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Courses_TPS
  PARENT.Init
  Access:Courses_TPS &= SELF


Hide:Access:Courses_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Courses_TPS &= NULL                               ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Courses_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Courses_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(COUTPS:KeyNumber,'by Course Number',1)
    SELF.AddKey(COUTPS:KeyDescription,'by Course Description',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Courses_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Courses_TPS.Kill                            ! Kill the file manager
  PARENT.Kill
  Relate:Courses_TPS &= NULL                               ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Courses_TPS)                        ! destroy the file manager


Hide:Relate:Majors_TPS.Init PROCEDURE()
  CODE
  _Hide:Access:Majors_TPS.Init
  SELF.Init(Access:Majors_TPS,1)


Hide:Access:Majors_TPS.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= MAJTPS:Record
  SELF.FileNameValue = 'Majors_TPS'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Majors_TPS
  PARENT.Init
  Access:Majors_TPS &= SELF


Hide:Access:Majors_TPS.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:Majors_TPS &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:Majors_TPS.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(Majors_TPS,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(MAJTPS:KeyNumber,'by Major Number',0)
    SELF.AddKey(MAJTPS:KeyDescription,'by Major Description',0)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:Majors_TPS.Kill PROCEDURE

  CODE
  _Hide:Access:Majors_TPS.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:Majors_TPS &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:Majors_TPS)                         ! destroy the file manager

