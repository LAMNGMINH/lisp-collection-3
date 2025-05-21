;Union polylines
;Stefan M. 09.01.2014
(defun c:UNIP ( / *error* i lst ms r1 reg ss sysvar prop obj reg1 op not_reg)
  (or acDoc (setq acDoc (vla-get-activedocument (vlax-get-acad-object))))
  (setq ms (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace)))
  (vla-startundomark acDoc)
  (setq sysvar (mapcar 'getvar '(peditaccept draworderctl cmdecho)))
  
  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg))
      )
    (mapcar 'setvar '(peditaccept draworderctl cmdecho) sysvar)
    (vla-endundomark acDoc)
    (princ)
    )
  
  (if
    (setq ss (ssget ":L" '((0 . "*POLYLINE,CIRCLE,REGION"))))
    (progn
      (setq prop (mapcar '(lambda (p) (vlax-get (vlax-ename->vla-object (ssname ss 0)) p)) '(Layer LineType Color)))
      
      (repeat (setq i (sslength ss))
        (setq i (1- i)
              obj (vlax-ename->vla-object (ssname ss i))
        )
        (if
          (eq (vla-get-ObjectName obj) "AcDbRegion")
          (setq reg1 (cons obj reg1))
          (setq lst  (cons obj lst))
        )
      )
      
      (if lst
          (foreach x lst
            (if
              (and
                (not (eq (vla-get-objectname x) "AcDbCircle"))
                (eq (vla-get-closed x) :vlax-false)
                )
              (progn
                (setq op T)
                (vla-put-closed x :vlax-true)
                )
              (setq op nil)
              )
            (if
              (not (vl-catch-all-error-p (setq r1 (vl-catch-all-apply 'vlax-invoke (list ms 'AddRegion (list x))))))
              (progn
                (setq reg (cons (car r1) reg))
                (vla-delete x)
                )
              (progn
                (if op (vla-put-closed x :vlax-false))
                (setq not_reg (cons x not_reg))
            )
          )
        )
      )
      
      (setq reg (append reg reg1))
      (if
        (setq r1 (car reg))
         (progn
           (foreach x (cdr reg) (vlax-invoke r1 'boolean acunion x))
           (mapcar '(lambda (p v) (vlax-put r1 p v))
                   '(Layer LineType Color)
                   prop
           )
           (setq
             lst (apply
                   'append
                   (mapcar
                     '(lambda (a)
                        (if
                          (listp a)
                           (mapcar 'vlax-vla-object->ename a)
                           (list (vlax-vla-object->ename a))
                        )
                      )
                     (mapcar
                       '(lambda (e / p)
                          (if (eq (vla-get-objectname e) "AcDbRegion")
                            (progn
                              (setq p (vlax-invoke e 'explode))
                              (vla-delete e)
                              p
                            )
                            e
                          )
                        )
                       (vlax-invoke r1 'explode)
                     )
                   )
                 )
           )
           (vla-delete r1)
           (setq ss (ssadd))
           (foreach x lst (if (not (eq (cdr (assoc 1 (entget x))) "CIRCLE")) (ssadd x ss)))
           (if
             (< 1 (sslength ss))
             (progn
               (mapcar 'setvar '(peditaccept draworderctl cmdecho) '(1 0 0))
               (command "_pedit" "_m" ss "" "_j" "" "")
             )
           )
        )
      )
    )
  )
  (*error* nil)
  (princ)
)