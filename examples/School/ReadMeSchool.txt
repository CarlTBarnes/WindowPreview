This is the Clarion School Example

It shows how easy it is to add the CBWndPreview class to an entire project by just implementing CbWndPrvHelpHookClass in the frame. 

After running the below menu item ...

    MENUBAR,USE(?MENUBAR1)
       MENU('*DevCon2019*'),USE(?DevCon2019)
           ITEM('Enable F1 Hook to show CB Window Preview '),USE(?CbHlpHookEnableItem)

    Message('Press Ctrl+Shift+F1 on Open Windows to see CB Window Preview Reflection List.' & |
            '||Note: Any windows open now must be closed to work.')

As the message says in any window press Ctrl+Shift+F1 and the Window Preview Field List appears.

It implements CBWndPreview Class with CbWndPrvHelpHook.inc

Make thse changes

In the EXE APP Global embeds "Before Global Includes" add these lines:

   INCLUDE('CbWndPrvHelpHook.INC'),ONCE   !<><><><><><><> CB Wnd Preview Help Hook <><><><><><><>
   INCLUDE('CbWndPreview.INC'),ONCE       !<><><><><><><> CB Wnd Preview Class     <><><><><><><>

Help2WndPreviewCls  &CbWndPrvHelpHookClass  !Declare as Ref so no chance affects live APP

In the Frame Add this ITEM to the File Menu or any Menu. This would be hidden from users and show only for Developers hence the HIDE.

  ITEM('Enable F1 Hook to show CB Window Preview on Any Window '),USE(?CbHlpHookEnableItem)  !,HIDE 


In Event:Accepted for ?CbHlpHookEnableItem Item Add. You must change 'YourHelp.CHM' to the name of your CHM file if you have one.

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

    