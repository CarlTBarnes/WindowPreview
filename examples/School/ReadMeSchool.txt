This is the Clarion School Example

It shows how easy it is to add the CBWndPreview class to an entire project by just implementing CbWndPrvHelpHookClass in the frame. 

After running the below menu item ...

    MENUBAR,USE(?MENUBAR1)
       MENU('*DevCon2019*'),USE(?DevCon2019)
           ITEM('Enable F1 Hook to show CB Window Preview '),USE(?CbHlpHookEnableItem)

    Message('Press Ctrl+Shift+F1 on Open Windows to see CB Window Preview Reflection List.' & |
            '||Note: Any windows open now must be closed to work.')

As the message says in any window press Ctrl+Shift+F1 and Wnd Preview Field List appears.



It implements CBWndPreview Class with CbWndPrvHelpHook.inc

See use of CbWndPrvHelpHookClass in Global and in Main Fraem

  CASE ACCEPTED()
  OF ?CbHlpHookEnableItem
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
            '||Note: Any windows open now must be closed to work.')
    