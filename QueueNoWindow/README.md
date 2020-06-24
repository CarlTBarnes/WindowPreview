## View Queue without a Window

If you have a Queue but no Window witrh LIST you can view it using a call to QueueReflection like the below code and in the example in this folder.

```clarion
WndPrvCls     CBWndPreviewClass

    WndPrvCls.QueueReflection(FilesDirQ,'FilesDirQ')

```

It opens to a window showing the Queue Definition. Yu can coipy thast definition to the clipboard.

![qdef](readmeqr1.png)

Click View Queue Records to see the Queue records. Right-click for options like to copy the queue contents to the clipboard.

![qdef](readmeqr2.png)
