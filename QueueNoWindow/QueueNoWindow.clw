!Roberto Artigas requested to be abe to view a Queue without a Window 
!This also has a Window procedure to confirm the results are the same
  PROGRAM

  INCLUDE('CBWndPreview.INC'),ONCE

  MAP
QueueNoWnd  PROCEDURE()  
QueueWindow PROCEDURE()
  END

FilesDirQ    QUEUE(FILE:Queue),PRE(FDirQ)
ExtraByte       BYTE
             END ! FDirQ:Name  FDirQ:ShortName(8.3?)  FDirQ:Date  FDirQ:Time  FDirQ:Size  FDirQ:Attrib
  CODE
  DIRECTORY(FilesDirQ,'C:\Windows\*.*',ff_:NORMAL+ff_:DIRECTORY)  
  CASE Message('What to do?||1. View Queue in new QueueReflection without a Window' & |
               '||2. Window with LIST From(Q) using WndPreview can view Queue',|
                'Queue No Window Example',ICON:Clarion,'1. Reflection Q|2. Window w/LIST') 
  OF 1 ; QueueNoWnd()
  OF 2 ; START(QueueWindow)
  END 
!===============================================================    
QueueNoWnd PROCEDURE()  
WndPrvCls     CBWndPreviewClass
    CODE
    !Message('Click OK and it will call WndPrvCls.QueueuReflection()')
    WndPrvCls.QueueReflection(FilesDirQ,'FilesDirQ')
!   ^^^^^^ View a Queue with this one line ^^^^^^^^

!===============================================================    
QueueWindow PROCEDURE()     !See the same Queue in a LIST
FilesWindow WINDOW('FilesDirQ QUEUE '),AT(,,343,150),GRAY,SYSTEM,FONT('Segoe UI',9,,FONT:regular),RESIZE,CENTER
        STRING('<<=== Click Secret Button, then "LIST" button, then "From(Q)" button, then "View From(Q)"'),AT(14,1),USE(?Secret)
        LIST,AT(1,10),FULL,USE(?ListFiles),VSCROLL,FROM(FilesDirQ),FORMAT('120L(1)|M~Name~@s255@52L(' & |
                '1)|M~Short Name~@s13@40R(1)|M~Date~C(0)@d1@40R(1)|M~Time~C(0)@T4@56R(1)|M~Size~C(0)' & |
                '@n13@16L(1)|M~Attr~@n3@')
    END
WndPrvCls     CBWndPreviewClass    
    CODE
    OPEN(FilesWindow)
    WndPrvCls.Init()        !Button in top right corner 
   
    !Below line is so on Window showing "PROPLIST and Format()"the From(Q) button will show this Queue
    !   not required in 11.13505

    WndPrvCls.InitList(?ListFiles, FilesDirQ, 'FilesDirQ')  

    !Steps to see From(Q) on a Window:
    !   1. CLick secret button
    !   2. CLick LIST button opens "PROPLIST and Format()" 
    !   3. Click "From(Q)" button shows  FromQ Tab
    !   4. Click "View From(Q)" button opens window to show Queue records

    ACCEPT
    END
    