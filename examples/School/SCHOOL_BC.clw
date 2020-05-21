  MEMBER('SCHOOL.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('SCHOOL_BC0.CLW')
SCHOOL_BC0:DctInit             PROCEDURE()
SCHOOL_BC0:DctKill             PROCEDURE()
SCHOOL_BC0:FilesInit           PROCEDURE()
    END
  END

DctInit PROCEDURE()
  CODE
  SCHOOL_BC0:DctInit
  SCHOOL_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  SCHOOL_BC0:DctKill

