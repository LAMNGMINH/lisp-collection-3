;;Counting rectangles.
;;Stefan M., 11.feb.2015
;;color counting 04.mar.2015
(defun rectangle_dims (e / l a b)
  (setq l (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) e)))
  (if
    (and
      (or
        (= 1 (logand (cdr (assoc 70 e)) 1))
        (equal (car l) (last l) 1e-8)
      )
      (equal (distance  (car   l) (caddr l)) (distance  (cadr   l) (cadddr l)) 1e-8)
      (equal (mapcar '- (cadr  l) (car   l)) (mapcar '- (caddr  l) (cadddr l)) 1e-8)
      (equal (mapcar '- (caddr l) (cadr  l)) (mapcar '- (cadddr l) (car    l)) 1e-8)
    )
    (vl-sort (list (distance (car l) (cadr l)) (distance (cadr l) (caddr l))) '<)
  )
)

(defun C:RECDIMS (/ *error* ss e old r p1 c)
  (vl-load-com)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-activedocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace))
  )
  (vla-startundomark acDoc)
  
  ;;;;;; Error function ;;;;;;;;;
  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
      )
    (vla-endundomark acDoc)
    (princ)
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (if
    (setq ss (ssget '((0 . "LWPOLYLINE") (-4 . "<OR") (90 . 4) (90 . 5) (-4 . "OR>"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))
              c (cond
                  ((cdr (assoc 62 (entget e))))
                  ((cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 (entget e)))))))
                )
              c (if (zerop c) 7 c)
              )
        (if
          (and
            (setq dims (rectangle_dims (entget e)))
            (setq dims (cons c dims))
            )
          (if
            (setq old (vl-some '(lambda (d) (if (equal (cdr d) dims 1e-8) d)) r))
            (setq r (subst (cons (1+ (car old)) dims) old r))
            (setq r (cons  (cons 1 dims) r))
          )
        )
      )
      (if
        (and r (setq p1 (getpoint "\nSpecify table insert point: ")))
        (insert_table
          (vl-sort
            (vl-sort
              (vl-sort
                (mapcar '(lambda (a) (list (cadr a) (caddr a) (cadddr a) (car a))) r)
                '(lambda (a b) (< (caddr a) (caddr b)))
                )
              '(lambda (a b) (< (cadr a) (cadr b)))
             )
            '(lambda (a b) (< (car a) (car b)))
          )
          p1
          )       
         )
       )
    )
  (princ)
)

;;The textheight in table depends on cannonscale
(defun insert_table (lst pct / tab row col ht i n acol)
  (setq ht  (/ 2.5 (getvar 'cannoscalevalue))
        pct (trans pct 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tab (setq tab (vla-addtable space (vlax-3d-point pct) (+ 2 (length lst)) 3 (* 1.5 ht) ht))
        acol (vla-getinterfaceobject acobj  (strcat "AutoCAD.AcCmColor." (substr (vla-get-version acobj) 1 2)))
        )
  (vlax-put tab 'direction n)
  
  (mapcar
    (function
      (lambda (rowType)
        (vla-SetTextStyle  tab rowType (getvar 'textstyle))
        (vla-SetTextHeight tab rowType ht)
      )
    )
   '(2 4 1)
  )
  
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  (setq lst (cons '(nil "Width" "Length" "Pcs.") lst))

  (setq i 0)
  (foreach col (apply 'mapcar (cons 'list (mapcar 'cdr lst)))
    (vla-SetColumnWidth tab i
      (apply
        'max
        (mapcar
          '(lambda (x)
             ((lambda (txb) (+ (abs (- (caadr txb) (caar txb))) (* 2.0 ht)))
              (textbox (list (cons 1 (vl-princ-to-string x)) (cons 7 (getvar 'textstyle)) (cons 40 ht)))
              )
             )
          col
          )
        )
      )
    (setq i (1+ i))
    )
  
  (setq lst (cons '(nil "RECTANGLES") lst))
  
  (setq row 0)
  (foreach r lst
    (setq col 0)
    (vla-SetRowHeight tab row (* 1.5 ht))
    (foreach c (cdr r)
      (vla-SetText tab row col (vl-princ-to-string c))
      (if
        (car r)
        (progn
          (if (/= (vla-get-colorindex acol) (car r)) (vla-put-colorindex acol (car r)))
          (vla-SetCellContentColor tab row col acol)
          )
        )
      (setq col (1+ col))
      )
    (setq row (1+ row))
    )
  )
  
(princ "\nType RECDIMS to start the command")