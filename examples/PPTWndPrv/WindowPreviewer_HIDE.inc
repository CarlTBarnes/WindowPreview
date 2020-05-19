
 SECTION('Includes')
FEQ LONG

!ShowYes EQUATE('1')  !,VALUE(ShowYes,ShowNo)
!ShowNo  EQUATE('0')

 SECTION('AfterOpen')
  0{PROP:System}=1  !Red Close X
  LOOP
    FEQ=0{PROP:NextField,FEQ}
    IF ~FEQ THEN BREAK.
    UNHIDE(FEQ) ; ENABLE(FEQ)
  END

  SECTION('InAccept')
  windowPreviewerCanExit__=FALSE !Button Click stays open
