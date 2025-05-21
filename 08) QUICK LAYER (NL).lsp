(defun c:nl ( / *error* check_name acdoc ss fname op_file dcl name r i la col)
  (if (setq ss (ssget "_I")) (setq ss (ssget ":L")))

  (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))

  (if (= 8 (logand (getvar 'undoctl) 8)) (vla-endundomark acDoc))
  (vla-startundomark acDoc)

  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
      )
    (if fname (vl-catch-all-apply 'vl-file-delete (list fname)))
    (vla-endundomark acDoc)
    (princ)
  )

  (setq
    fname (vl-filename-mktemp "newlayer" (getvar 'dwgprefix) ".dcl")
    op_file (open fname "w")
  )

  (foreach x
    '("newlayer : dialog {"
      ": spacer { height = 1;}"
      ": text { alignment = left; label = \"Enter Layer Name\";}"
      ": edit_box {alignment = left; key = \"name\"; edit_width = 40;}"
      ": spacer { height = 1;}"
      "ok_cancel;"
      "errtile;}"
     )
    (write-line x op_file)
  )
  (close op_file)
  
  (defun check_name (str)
    (if
      (wcmatch str "*<*,*>*,*/*,*\\*,*:*,*;*,*`?*,*`**,*|*,*`,*,*=*,*``*")
      (progn
        (set_tile "error" "Invalid character. Do not use <>/\\\":;?*|,=`")
        (mode_tile "name" 2)
        )
      (set_tile "error" "")
      )
    )

  (if
    (and
      (> (setq dcl (load_dialog fname)) 0)
      (new_dialog "newlayer" dcl)
      )
    (progn
      (set_tile "name" (setq name (cond (ss (cdr (assoc 8 (entget (ssname ss 0))))) ("New Layer"))))
      (action_tile "name" "(setq name $value) (check_name name)")
      (setq r (start_dialog))
      (unload_dialog dcl)
      )
    )
  (if
    (and
      (= r 1)
      (/= name "")
      )
    (progn
      (if
        (not (tblsearch "layer" name))
        (progn
          (setq la (vla-add (vla-get-layers acdoc) name))
          (if
            (setq col (acad_colordlg 7))
            (vla-put-color la col)
            )
          T
          )
        (setq la (vla-item (vla-get-layers acdoc) name))
        )
      (if
        (/= name (getvar 'clayer))
        (progn
          (vla-put-freeze la :vlax-false)
          (setvar 'clayer name)
          )
        )
      (if ss
        (repeat (setq i (sslength ss))
          (vla-put-layer (vlax-ename->vla-object (ssname ss (setq i (1- i)))) name)
          )
        )     
      )
    )
  (*error* nil)
  (princ)
  )