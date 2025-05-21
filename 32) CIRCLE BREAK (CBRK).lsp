;;-------------------------=={ Circle Break }==-------------------------;;
;;                                                                      ;;
;;  This program enables the user to break a circle or ellipse into     ;;
;;  two arcs, one of which will inherit properties set within the       ;;
;;  program source code.                                                ;;
;;                                                                      ;;
;;  The program was written to decrease the time taken to display       ;;
;;  hidden sections of circular or elliptical geometry. Previously,     ;;
;;  to construct a circle or ellipse displaying two different           ;;
;;  linetypes (perhaps indicative of hidden detail), users would need   ;;
;;  to break or trim the original circle or ellipse using intersecting  ;;
;;  objects or points, and then either construct the second arc         ;;
;;  manually, or create another identical circle or ellipse and         ;;
;;  repeat the breaking or trimming process; before finally moving one  ;;
;;  of the arcs to a separate layer or applying a set of properties.    ;;
;;                                                                      ;;
;;  To save time in this procedure, this program allows the user to     ;;
;;  select the circle or ellipse at the section to be 'hidden', and     ;;
;;  then pick two break points. The remaining operations are then       ;;
;;  completed instantaneously by the program.                           ;;
;;                                                                      ;;
;;  The properties (such as Layer, Linetype, Lineweight etc.) to be     ;;
;;  applied to the 'hidden' arc may be changed below where noted.       ;;
;;                                                                      ;;
;;  Finally, this program will operate successfully in all UCS & Views, ;;
;;  with circles and ellipses constructed in any UCS plane.             ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    30-05-2013                                      ;;
;;----------------------------------------------------------------------;;

(defun c:cbrk ( / *error* cen dxf ent enx fnc lst pa1 pa2 pt1 pt2 sel )

;;----------------------------------------------------------------------;;
;;                      Hidden Section Properties                       ;;
;;----------------------------------------------------------------------;;

    (setq dxf
       '(
            (006 . "BYLAYER") ;; Linetype (must be loaded)
            (008 . "HIDDEN")  ;; Layer
            (039 . 0.0)       ;; Thickness
            (048 . 1.0)       ;; Linetype Scale
            (062 . 256)       ;; Colour (0 = ByBlock, 256 = ByLayer)
            (370 . -1)        ;; Lineweight (-1 = ByLayer, -2 = ByBlock, -3 = Default, 0.3 = 30 etc.)
        )
    )

;;----------------------------------------------------------------------;;

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (while
        (progn (setvar 'errno 0) (setq sel (entsel "\nSelect Circle/Ellipse section to be hidden: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   sel
                    (if (not (wcmatch (cdr (assoc 0 (entget (car sel)))) "CIRCLE,ELLIPSE"))
                        (princ "\nSelected object is not a Circle or Ellipse.")
                    )
                )
            )
        )
    )
    (if
        (and sel (setq pt1 (getpoint "\nPick 1st break point: "))
            (progn
                (while (equal pt1 (setq pt2 (getpoint "\nPick 2nd break point: ")) 1e-8)
                    (princ "\nPoints must be distinct.")
                )
                pt2
            )
        )
        (progn
            (setq ent (car sel)
                  enx (entget ent)
                  pt1 (vlax-curve-getclosestpointto ent (trans pt1 1 0))
                  pt2 (vlax-curve-getclosestpointto ent (trans pt2 1 0))
                  pa1 (vlax-curve-getparamatpoint ent pt1)
                  pa2 (vlax-curve-getparamatpoint ent pt2)
            )
            (if (< pa2 pa1)
                (mapcar 'set '(pt1 pt2 pa1 pa2) (list pt2 pt1 pa2 pa1))
            )
            (if (< pa1 (vlax-curve-getparamatpoint ent (vlax-curve-getclosestpointto ent (trans (cadr sel) 1 0))) pa2)
                (setq dxf (list dxf (LM:defaultprops enx)))
                (setq dxf (list (LM:defaultprops enx) dxf))
            )
            (if (= "CIRCLE" (cdr (assoc 0 enx)))
                (setq cen  (cdr (assoc 10 enx))
                      lst  (list (angle cen (trans pt1 0 ent)) (angle cen (trans pt2 0 ent)))
                      enx  (vl-remove-if '(lambda ( x ) (member (car x) '(-1 0 5 6 8 39 48 62 100 102 330 370))) enx)
                      fnc '(( ) (append '((0 . "ARC")) (car dxf) (mapcar 'cons '(50 51) lst) enx))
                )
                (setq lst  (list (LM:point->param enx pt1) (LM:point->param enx pt2))
                      enx  (vl-remove-if '(lambda ( x ) (member (car x) '(-1 5 6 8 39 41 42 48 62 102 330 370))) enx)
                      fnc '(( ) (append enx (car dxf) (mapcar 'cons '(41 42) lst)))
                )
            )
            (LM:startundo (LM:acdoc))
            (repeat 2
                (entmake  (fnc))
                (setq dxf (reverse dxf)
                      lst (reverse lst)
                )
            )
            (entdel ent)
            (LM:endundo (LM:acdoc))
        )
    )
    (princ)
)

;; Point -> Ellipse Parameter  -  Lee Mac
;; Returns the ellipse parameter for the given point
;; dxf  -  Ellipse DXF data (DXF groups 10, 11, 40, 210)
;; pnt  -  WCS Point on Ellipse
;; Uses relationship: ratio*tan(param) = tan(angle)

(defun LM:point->param ( dxf pnt / ang ocs )
    (setq ocs (cdr (assoc 210 dxf))
          ang (- (angle (trans (cdr (assoc 10 dxf)) 0 ocs) (trans pnt 0 ocs))
                 (angle '(0.0 0.0) (trans (cdr (assoc 11 dxf)) 0 ocs))
              )
    )
    (atan (sin ang) (* (cdr (assoc 40 dxf)) (cos ang)))
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( elist )
    (mapcar
        (function
            (lambda ( pair )
                (cond ((assoc (car pair) elist)) ( pair ))
            )
        )
       '(
            (008 . "0")
            (006 . "BYLAYER")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: CircleBreak.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Type \"cbrk\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;