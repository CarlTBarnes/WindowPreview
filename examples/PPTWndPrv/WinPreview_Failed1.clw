 !THis failed because of ,VALUE(ShowYes,ShowNo) are undefined equates
 PROGRAM
 INCLUDE('EQUATES.CLW'),ONCE
 INCLUDE('KEYCODES.CLW'),ONCE
 MAP END
windowPreviewerCanExit__ BOOL(TRUE)
 INCLUDE('WindowPreviewer.inc', 'Includes'),ONCE
v2 STRING(44)
v3 STRING(44)
Window WINDOW('Login'),AT(,,163,76),CENTER,GRAY,SYSTEM,FONT('Segoe UI',9)
 PROMPT('&User Name:'),AT(7,9),USE(?Name:Pmt)
 ENTRY(@s20),AT(47,9,50),USE(v2),PASSWORD
 PROMPT('&Password:'),AT(7,27),USE(?Pwd:Pmt)
 ENTRY(@s20),AT(47,27,50),USE(v3),PASSWORD
 CHECK('&Show Name'),AT(101,9),USE(?bShowName),SKIP,VALUE(ShowYes,ShowNo)
 BUTTON('&Login'),AT(43,50),USE(?LoginBtn),STD(STD:Close)
 BUTTON('Cancel'),AT(86,50),USE(?CancelBtn),STD(STD:Close)
 BUTTON('&Backdoor'),AT(101,28,38,11),USE(?BackDoorBtn),HIDE
 END
 CODE
 OPEN(Window)
 INCLUDE('WindowPreviewer.inc', 'AfterOpen'),ONCE
 v2 = 'The quick brown fox jumped over the lazy dog'
 v3 = 'The quick brown fox jumped over the lazy dog'
 ACCEPT
 INCLUDE('WindowPreviewer.inc', 'InAccept'),ONCE
  IF windowPreviewerCanExit__ AND EVENT() = 1 AND ACCEPTED(){PROP:Type} = 9 THEN BREAK END
  windowPreviewerCanExit__ = TRUE
 END
