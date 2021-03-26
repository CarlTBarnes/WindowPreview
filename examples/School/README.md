## Implementing Window preview Class Help Hook 

It shows how easy it is to add the CBWndPreview class to an entire project (EXE and all DLLs) by just implementing CbWndPrvHelpHookClass in the Frame. 
 You setup a menu item that is only available to developers to turn it on. I have this automatically run for me.

### Make thse changes

In the EXE APP Global embeds "Before Global Includes" add these lines:
```Clarion
   INCLUDE('CbWndPrvHelpHook.INC'),ONCE   !<><><><><><><> CB Wnd Preview Help Hook <><><><><><><>
   INCLUDE('CbWndPreview.INC'),ONCE       !<><><><><><><> CB Wnd Preview Class     <><><><><><><>

Help2WndPreviewCls  &CbWndPrvHelpHookClass  !Declare as Ref so no chance affects live APP
```

In the Frame Add this ITEM to the File Menu or any Menu. This would be hidden from users and show only for Developers hence the HIDE.

```Clarion
  ITEM('Enable F1 Hook to show CB Window Preview on Any Window '),USE(?CbHlpHookEnableItem)  !,HIDE 
```

In Event:Accepted for ?CbHlpHookEnableItem Item Add. You must change 'YourHelp.CHM' to the name of your CHM file if you have one.

```Clarion
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
```
    