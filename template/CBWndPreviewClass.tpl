#TEMPLATE(CBWndPreviewClass,'CB Window Preview Class Related Templates'),FAMILY('ABC','CW20')
#Extension(CBWndPrvListFromQ,'For all LIST boxes set User Prop with &Queue Reference - Version 1.01')
#DISPLAY(' ')
#DISPLAY('For every LIST and COMBO add call to CBListPropFromQ(?List,Queue)')
#DISPLAY('which will set User Property FromQ. ')
#DISPLAY(' ')
#DISPLAY('In root module add CBListPropFromQ() PROCEDURE() ')
#DISPLAY(' ')
#PROMPT('Do NOT generate this Template i.e. Disable',CHECK),%DoNotGenerate,AT(10)
#!------------------------------------------------------------------------------
#!-- Legacy Init ------------------------------------------------------------------
#ATSTART
#DECLARE(%FromQ)
#ENDAT
#AT(%AfterWindowOpening),WHERE(~%DoNotGenerate)
  #! CBWndPrvListFromQ.TPL
  #FOR(%CONTROL),WHERE(%ControlType='LIST' OR %ControlType='DROP' OR %ControlType='COMBO')
    #SET(%FromQ,EXTRACT(%ControlStatement,'FROM',1)) 
    #IF(%FromQ AND 0=INSTRING(CHR(39),%FromQ,1,1))
 CBListPropFromQ(%Control,%FromQ,'%FromQ') !Tpl CBWndPrvListFromQ
    #ELSE
 !NO:  CBListPropFromQ(%Control,%FromQ,'%FromQ') !Tpl CBWndPrvListFromQ    
    #ENDIF
  #ENDFOR   
#EndAT
#!-------------
#AT(%GlobalMap),WHERE(~%DoNotGenerate)
    CBListPropFromQ(LONG FEQ,*QUEUE FrmQ,<STRING NameQ>)
#EndAT
#!-------------
#AT(%ProgramProcedures),WHERE(~%DoNotGenerate)
CBListPropFromQ PROCEDURE(LONG FEQ,*QUEUE FrmQ,<STRING NameQ>)
Ref GROUP,AUTO
Q    &QUEUE
L    LONG,OVER(Q)
  END
  CODE
  Ref.Q &=FrmQ ; FEQ{'FromQ'}=Ref.L ; FEQ{'FromWho'}=CHOOSE(~OMITTED(NameQ),NameQ,'Queue' & Ref.L)
  RETURN
#EndAT
#!------------------------------------------------------------------------------  