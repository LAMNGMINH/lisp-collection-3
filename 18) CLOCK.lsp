;;-------------------------=={ 2D Projection }==------------------------;;
;;                                                                      ;;
;;  This program provides the user with a means of projecting a         ;;
;;  selected set of planar objects from one reference frame to another. ;;
;;                                                                      ;;
;;  Upon issuing the command syntax '2dpro' at the AutoCAD              ;;
;;  command-line, the user is prompted to select a set of 2D planar     ;;
;;  objects to be projected. This selection is restricted to Arcs,      ;;
;;  Circles, Elipses, Lines, LWPolylines, 2D (Heavy) Polylines,         ;;
;;  2D Splines & Points.                                                ;;
;;                                                                      ;;
;;  The user is then prompted to select a source reference frame &      ;;
;;  a destination reference frame. For each of these prompts, the       ;;
;;  program requires the user to select a closed LWPolyline with four   ;;
;;  non-collinear vertices. Following each selection, the program       ;;
;;  will ensure the points are counter-clockwise oriented with the      ;;
;;  points ordered such that the lower-left vertex appears first.       ;;
;;                                                                      ;;
;;  Following valid user responses, the program will then convert the   ;;
;;  four 2D points defining each reference frame into homogeneous       ;;
;;  coordinates, and will calculate the transformation matrix to map    ;;
;;  from the source reference frame (or projective space) to the        ;;
;;  destination reference frame.                                        ;;
;;                                                                      ;;
;;  The program will then iterate over the set of selected objects      ;;
;;  and, for each object, will calculate a 2D point set describing or   ;;
;;  (in the case of curved objects) approximating the object.           ;;
;;                                                                      ;;
;;  Each point is then converted to homogeneous coordinates and mapped  ;;
;;  to the destination reference frame using the tranformation matrix,  ;;
;;  before being converted back to cartesian coordinates.               ;;
;;                                                                      ;;
;;  The program will then generate either a Point, Line or LWPolyline   ;;
;;  from the mapped point(s) with properties matching those of the      ;;
;;  original object.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-10-10                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:2dpro ( / *error* des ent enx idx lst mat ocs sel src typ )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (if
        (and
             (setq sel
                 (LM:ssget "\nSelect objects to project: "
                    '(   "_:L"
                         (
                             (-4 . "<OR")
                                 (-4 . "<AND")
                                     (0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,POINT")
                                     (-4 . "<NOT")
                                         (-4 . "<AND")
                                             (0 . "POLYLINE") (-4 . "&") (70 . 88)
                                         (-4 . "AND>")
                                     (-4 . "NOT>")
                                 (-4 . "AND>")
                                 (-4 . "<AND")
                                     (0 . "SPLINE") (-4 . "&=") (70 . 8)
                                 (-4 . "AND>")
                             (-4 . "OR>")
                         )
                     )
                 )
             )
             (setq src (2dprojection:getreferenceframe "\nSelect source reference frame: "))
             (setq des (2dprojection:getreferenceframe "\nSelect destination reference frame: "))
             (setq mat (2dprojection:getmatrix src des))
             (setq ocs (trans '(0.0 0.0 1.0) 1 0 t))
        )
        (repeat (setq idx (sslength sel))
            (setq ent (ssname sel (setq idx (1- idx)))
                  enx (entget ent)
                  typ (cdr (assoc 0 enx))
            )
            (cond
                (   (= "POINT" typ)
                    (entmake
                        (vl-list*
                           '(0 . "POINT")
                            (cons 10 (trans (2dprojection:mappoint mat (trans (cdr (assoc 10 enx)) 0 ocs)) ocs 0))
                            (LM:defaultprops enx)
                        )
                    )
                )
                (   (= "LINE" typ)
                    (entmake
                        (vl-list*
                           '(0 . "LINE")
                            (cons 10 (trans (2dprojection:mappoint mat (trans (cdr (assoc 10 enx)) 0 ocs)) ocs 0))
                            (cons 11 (trans (2dprojection:mappoint mat (trans (cdr (assoc 11 enx)) 0 ocs)) ocs 0))
                            (LM:defaultprops enx)
                        )
                    )
                )
                (   (setq lst (LM:Entity->PointList ent))
                    (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (length lst))
                                (if (vlax-curve-isclosed ent) '(70 . 1) '(70 . 0))
                            )
                            (LM:defaultprops enx)
                            (mapcar '(lambda ( p ) (cons 10 (2dprojection:mappoint mat (trans p 0 ent)))) lst)
                            (list (assoc 210 enx))
                        )
                    )
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; 2D Projection: Get Reference Frame  -  Lee Mac
;; Prompts the user to select a closed planar polyline with 4 vertices in
;; order to obtain 4 counter-clockwise oriented non-collinear points
;; defining a reference frame for the transformation.

(defun 2dprojection:getreferenceframe ( msg / ent enx lst tmp )
    (while
        (progn (setvar 'errno 0) (setq ent (car (entsel msg)))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent) nil)
                (   (or (/= "LWPOLYLINE" (cdr (assoc 0 (setq enx (entget ent)))))
                        (zerop (logand 1 (cdr (assoc 70 enx))))
                        (/= 4 (cdr (assoc 90 enx)))
                        (/= 4 (length (setq lst (2dprojection:uniquefuzz (mapcar 'cdr (vl-remove-if-not '(lambda ( x ) (= 10 (car x))) enx)) 1e-8))))
                        (2dprojection:checkcollinearity (cons (last lst) lst))
                    )
                    (setq lst nil)
                    (princ "\nPlease select a closed polyline with 4 non-collinear vertices.")
                )
            )
        )
    )
    (if lst
        (progn
            (if (2dprojection:clockwise-p lst)
                (setq lst (reverse lst))
            )
            (setq tmp (apply 'mapcar (cons 'min lst)))
            (repeat (car (vl-sort-i lst '(lambda ( a b ) (< (distance a tmp) (distance b tmp)))))
                (setq lst (append (cdr lst) (list (car lst))))
            )
            lst
        )
    )
)

;; 2D Projection: Unique with Fuzz  -  Lee Mac
;; Returns a list with all elements considered duplicate to a given tolerance removed.

(defun 2dprojection:uniquefuzz ( lst fuz )
    (if lst
        (cons (car lst)
            (2dprojection:uniquefuzz
                (vl-remove-if
                    (function (lambda ( x ) (equal x (car lst) fuz)))
                    (cdr lst)
                )
                fuz
            )
        )
    )
)

;; 2D Projection: Check Collinearity  -  Lee Mac
;; Returns T if any three points in a supplied list are collinear.

(defun 2dprojection:checkcollinearity ( lst )
    (and (caddr lst)
        (or (   (lambda ( a b c )
                    (or (equal (+ a b) c 1e-8)
                        (equal (+ b c) a 1e-8)
                        (equal (+ c a) b 1e-8)
                    )
                )
                (distance (car  lst) (cadr  lst))
                (distance (cadr lst) (caddr lst))
                (distance (car  lst) (caddr lst))
            )
            (2dprojection:checkcollinearity (cdr lst))
        )
    )
)

;; 2D Projection: Clockwise-p  -  Lee Mac
;; Returns T if the supplied point list is clockwise oriented.

(defun 2dprojection:clockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
    )
)

;; 2D Projection: Map Point  -  Lee Mac
;; Converts a supplied 2D point to homogeneous coordinates, applies a
;; matrix transformation & then converts the result back to cartesian coordinates.

(defun 2dprojection:mappoint ( mat pnt )
    (apply (function (lambda ( x y z ) (list (/ x z) (/ y z))))
        (mxv mat (list (car pnt) (cadr pnt) 1.0))
    )
)

;; 2D Projection: Get Matrix  -  Lee Mac
;; Calculates the transformation matrix for transforming
;; homogeneous 2D points from one reference frame to another.

(defun 2dprojection:getmatrix ( l1 l2 / f )
    (mxm
        (
            (setq f
                (lambda ( l / c m )
                    (setq c
                        (mxv
                            (invm
                                (setq m
                                    (trp
                                        (mapcar
                                            (function
                                                (lambda ( a b )
                                                    (list (car a) (cadr a) b)
                                                )
                                            )
                                            l '(1.0 1.0 1.0)
                                        )
                                    )
                                )
                            )
                            (list (car (last l)) (cadr (last l)) 1.0)
                        )
                    )
                    (mapcar '(lambda ( r ) (mapcar '* r c)) m)
                )
            )
            l2
        )
        (invm (f l1))
    )
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Entity to Point List  -  Lee Mac
;; Returns a list of points describing or approximating the supplied entity,
;; else nil if the entity is not supported.

(defun LM:entity->pointlist ( ent / acc der di1 di2 di3 enx fun inc lst par rad typ )
    (setq enx (entget ent)
          typ (cdr (assoc 0 enx))
          acc 35.0
    )
    (cond
        (   (= "POINT" typ)
            (list (cdr (assoc 10 enx)))
        )
        (   (= "LINE" typ)
            (list (cdr (assoc 10 enx)) (cdr (assoc 11 enx)))
        )
        (   (wcmatch typ "CIRCLE,ARC")
            (setq di1 0.0
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
                  inc (/ di2 (1+ (fix (* acc (/ di2 (cdr (assoc 40 enx)) (+ pi pi))))))
                  fun (if (vlax-curve-isclosed ent) < <=)
            )
            (while (fun di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      di1 (+ di1 inc)
                )
            )
            lst
        )
        (   (or (= typ "LWPOLYLINE")
                (and (= typ "POLYLINE") (zerop (logand (cdr (assoc 70 enx)) 80)))
            )
            (setq par 0)
            (repeat (fix (1+ (vlax-curve-getendparam ent)))
                (if (setq der (vlax-curve-getsecondderiv ent par))
                    (if (equal der '(0.0 0.0 0.0) 1e-8)
                        (setq lst (cons (vlax-curve-getpointatparam ent par) lst))
                        (if (setq rad (distance '(0.0 0.0) (vlax-curve-getfirstderiv ent par))
                                  di1 (vlax-curve-getdistatparam ent par)
                                  di2 (vlax-curve-getdistatparam ent (1+ par))
                            )
                            (progn
                                (setq inc (/ (- di2 di1) (1+ (fix (* acc (/ (- di2 di1) rad (+ pi pi)))))))
                                (while (< di1 di2)
                                    (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                                          di1 (+ di1 inc)
                                    )
                                )
                            )
                        )
                    )
                )
                (setq par (1+ par))
            )
            (if (or (vlax-curve-isclosed ent) (equal '(0.0 0.0 0.0) der 1e-8))
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (= "ELLIPSE" typ)
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  di3 (* di2 (/ (+ pi pi) (abs (- (vlax-curve-getendparam ent) (vlax-curve-getstartparam ent)))))
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1)))
                      di1 (+ di1 (/ di3 (1+ (fix (/ acc (/ di3 der (+ pi pi)))))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (= "SPLINE" typ)
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  inc (/ di2 25.0)
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (/ (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1))) inc)
                      di1 (+ di1 (if (equal 0.0 der 1e-10) inc (min inc (/ 1.0 der (* 10. inc)))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
    )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun invm ( m / c f p r )
    (defun f ( p m ) (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m))
    (setq  m (mapcar 'append m (imat (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun imat ( n / i j l m )
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l)
                  j (1- j)
            )
        )
        (setq m (cons l m)
              l nil
              i (1- i)
        )
    )
    m
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
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
        "\n:: 2DProjection.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"2dpro\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;


;;-----------------------------=={ Clock }==----------------------------;;
;;                                                                      ;;
;;  This novelty application will display a chronograph-style clock     ;;
;;  interface powered by a Visual LISP Command Reactor. The clock will  ;;
;;  display a traditional analog dial, including a 24-hour dial,        ;;
;;  day-of-the-week dial, the date, month & year and a digital display. ;;
;;                                                                      ;;
;;  The clock face may be enabled & disabled using the 'clock' command  ;;
;;  toggle. Upon issuing this command at the AutoCAD command-line, the  ;;
;;  user will be prompted to supply an insertion point for the clock.   ;;
;;                                                                      ;;
;;  The clock hands will then update automatically to reflect the       ;;
;;  current time after an AutoCAD command has been used.                ;;
;;                                                                      ;;
;;  The 'runclock' command will continuously update the clock display   ;;
;;  as the user moves the mouse until the user clicks or presses any    ;;
;;  key to exit. However, to enable a continuous animation, the         ;;
;;  'runclock' command will take precedence over all other actions      ;;
;;  whilst running.                                                     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    15-06-2013                                      ;;
;;----------------------------------------------------------------------;;

(setq
    clock:layer  "LMAC-Clock"
    clock:second "LMAC-Second"
    clock:minute "LMAC-Minute"
    clock:hour   "LMAC-Hour"
    clock:day    "LMAC-Day"
    clock:24hr   "LMAC-24hr"
    clock:05pi   (/ pi 2.0)
)

;;----------------------------------------------------------------------;;

(defun c:clock ( / *error* ins )

    (defun *error* ( msg )
        (clock:purgeclock)
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (= 'vlr-command-reactor (type clock:reactor))
        (clock:purgeclock)
        (if (setq ins (getpoint "\nPick Point for Clock: "))
            (progn
                (vlr-set-notification
                    (setq clock:reactor
                        (vlr-command-reactor (clock:makeclock ins) '((:vlr-commandended . clock:callback)))
                    )
                    'active-document-only
                )
                (command "_.redraw")
                (princ "\nClock running.")
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun c:runclock ( / *error* cmd grr )

    (defun *error* ( msg )
        (if (= 'int (type cmd))
            (setvar 'cmdecho cmd)
        )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (= 'vlr-command-reactor (type clock:reactor))
        (progn
            (setq cmd (getvar 'cmdecho))
            (setvar 'cmdecho 0)
            (princ "\nMove mouse to run clock, press any key to exit.")
            (while
                (and
                    (not (vl-catch-all-error-p (setq grr (vl-catch-all-apply 'grread '(t 15 1)))))
                    (= 5 (car grr))
                )
                (command "_.redraw" "_.delay" 1000)
            )
            (setvar 'cmdecho cmd)
        )
        (princ "\nClock doesn't exist, type \"Clock\" to create the clock.")
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun clock:callback ( obj arg / 24h )
    (if (not (wcmatch (strcase (car arg)) "U,UNDO"))
        (progn
            (mapcar
                (function
                    (lambda ( han rot )
                        (if (entget (handent han))
                            (vla-put-rotation (vlax-ename->vla-object (handent han)) (* rot -2 pi))
                            ;; entmod causes lock up with redraw command
                        )
                    )
                )
                (vlr-data obj)
                (list
                    (setq 24h (rem (getvar 'date) 1))
                    (* 2 24h)
                    (rem (* 24h 0024) 1)
                    (/ (fix (rem (* 24h 86400) 60)) 60.0)
                    (/ (fix (rem (getvar 'date) 7))  7.0)
                )
            )
            (mapcar
                (function
                    (lambda ( han str )
                        (if (entget (handent han))
                            (vla-put-textstring (vlax-ename->vla-object (handent han)) str)
                        )
                    )
                )
                (reverse (vlr-data obj))
                (list
                    (menucmd "m=$(edtime,$(getvar,date),DD)")
                    (menucmd "m=$(edtime,$(getvar,date),HH:MM)")
                    (menucmd "m=$(edtime,$(getvar,date),MONTH YYYY)")
                )
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun clock:makeclock ( ins / ang cen han inc ocs rad )

    (if (not (tblsearch "layer" clock:layer))
        (entmake
            (list
               '(000 . "LAYER")
               '(100 . "AcDbSymbolTableRecord")
               '(100 . "AcDbLayerTableRecord")
               '(070 . 0)
                (cons 002 clock:layer)
               '(062 . 80)
               '(290 . 0)
            )
        )
    )

    (setq ocs (trans '(0.0 0.0 1.0) 1 0 t)
          ins (trans ins 1 ocs)
    )
    (clock:lwpoly
        (list
            (list 10 (+ (car ins) 0.09) (cadr ins))
           '(42 . 1.0)
            (list 10 (- (car ins) 0.09) (cadr ins))
           '(42 . 1.0)
        )
        0.18 1 ocs
    )
    (clock:circle ins 0.5 ocs)
    (setq inc 0)
    (repeat 60
        (setq ang (* inc (/ pi 30.0)))
        (clock:lwpoly
            (list
                (cons 10 (polar ins ang 10.0))
                (cons 10 (polar ins ang  9.0))
            )
            0.0 0 ocs
        )
        (if (zerop (rem inc 5))
            (clock:lwpoly
                (list
                    (cons 10 (polar ins ang 10.0))
                    (cons 10 (polar ins ang  8.4))
                )
                0.2 0 ocs
            )
        )
        (setq inc (1+ inc))
    )
    (setq ang clock:05pi)
    (repeat 2
        (setq rad 2.5)
        (repeat 2
            (clock:circle (polar ins ang 4.45) rad ocs)
            (setq rad (- 2.7 rad))
        )
        (setq ang (+ ang ang))
    )
    (setq cen (mapcar '+ ins '(0.00 4.45))
          inc 0
    )
    (foreach str '("Mon" "Sun" "Sat" "Fri" "Thu" "Wed" "Tue")
        (setq ang (+ clock:05pi (* inc pi (/ 2.0 7.0))))
        (clock:lwpoly
            (list
                (cons 10 (polar cen ang 2.5))
                (cons 10 (polar cen ang 2.0))
            )
            0.1 0 ocs
        )
        (clock:mtext (polar cen ang 1.65) str 0.3 (- ang clock:05pi) ocs)
        (setq inc (1+ inc))
    )
    (setq cen (mapcar '- ins '(4.45 0.0))
          inc 0
    )
    (repeat 24
        (setq ang (* inc (/ pi 12.0)))
        (clock:lwpoly
            (list
                (cons 10 (polar cen ang 2.50))
                (cons 10 (polar cen ang 2.15))
            )
            0.0 0 ocs
        )
        (if (zerop (rem inc 2))
            (clock:lwpoly
                (list
                    (cons 10 (polar cen ang 2.5))
                    (cons 10 (polar cen ang 2.0))
                )
                0.1 0 ocs
            )
        )
        (if (zerop (rem inc 6))
            (clock:mtext (polar cen ang 1.65) (nth (/ inc 6) '("6" "24" "18" "12")) 0.3 0.0 ocs)
        )
        (setq inc (1+ inc))
    )
    (clock:lwpoly
        (list
            (cons 10 (mapcar '+ ins '(5.0 -0.9)))
           '(42 . 0.201387)
            (cons 10 (mapcar '+ ins '(7.8 -0.9)))
           '(42 . 0.0)
            (cons 10 (mapcar '+ ins '(7.8  0.9)))
           '(42 . 0.201387)
            (cons 10 (mapcar '+ ins '(5.0  0.9)))
           '(42 . 0.0)
        )
        0.0 1 ocs
    )
    (clock:mtext (mapcar '+ ins '(5.04  2.31)) "By Lee Mac" 0.5 0.0 ocs)
    (setq han
        (mapcar (function (lambda ( ent ) (cdr (assoc 5 (entget ent)))))
            (append
                (mapcar
                    (function
                        (lambda ( blk lst wid ins )
                            (if (not (tblsearch "block" blk))
                                (progn
                                    (entmake
                                        (list
                                           '(00 . "BLOCK")
                                           '(08 . "0")
                                            (cons 02 blk)
                                           '(10 0.0 0.0 0.0)
                                           '(70 . 0)
                                        )
                                    )
                                    (clock:lwpoly lst wid 0 '(0.0 0.0 1.0))
                                    (entmake '((0 . "ENDBLK") (8 . "0")))
                                )
                            )
                            (entmakex
                                (list
                                   '(000 . "INSERT")
                                    (cons 008 clock:layer)
                                    (cons 002 blk)
                                    (cons 010 ins)
                                    (cons 210 ocs)
                                )
                            )
                        )
                    )
                    (list clock:24hr clock:hour clock:minute clock:second clock:day)
                   '(
                        ((10 0.0 0.2) (10 0.0 1.3))
                        ((10 0.0 0.5) (10 0.0 4.5))
                        ((10 0.0 0.5) (10 0.0 8.0))
                        ((10 0.0 0.5) (10 0.0 8.0))
                        ((10 0.0 0.2) (10 0.0 1.3))
                        
                    )
                   '(0.1 0.2 0.2 0.0 0.1)
                    (list
                        (mapcar '- ins '(4.45 0.00))
                        ins ins ins
                        (mapcar '+ ins '(0.00 4.45))
                    )
                )
                (mapcar
                    (function
                        (lambda ( vec fmt )
                            (clock:mtext (mapcar '+ ins vec) (menucmd (strcat "m=$(edtime,$(getvar,date)," fmt ")")) 1.0 0.0 ocs)
                        )
                    )
                   '((0.00 -4.15) (0.00 -2.50) (6.40 0.00))
                   '("MONTH YYYY" "HH:MM" "DD")
                )
            )
        )
    )
    (setq clock:fillmode (getvar 'fillmode))
    (setvar 'fillmode 1)
    (vla-zoomwindow (vlax-get-acad-object)
        (vlax-3D-point (trans (polar ins (* clock:05pi 2.5) (+ 3.0 (sqrt 200.0))) ocs 0))
        (vlax-3D-point (trans (polar ins (* clock:05pi 0.5) (+ 3.0 (sqrt 200.0))) ocs 0))
    )
    han
)

;;----------------------------------------------------------------------;;

(defun clock:purgeclock ( / blk doc inc sel )
    (setq doc (vla-get-activedocument (vlax-get-acad-object))
          blk (vla-get-blocks doc)
    )
    (if (= 'vlr-command-reactor (type clock:reactor))
        (progn
            (vlr-remove clock:reactor)
            (setq clock:reactor nil)
        )
    )
    (if (= 'int (type clock:fillmode))
        (progn
            (setvar 'fillmode clock:fillmode)
            (setq clock:fillmode nil)
        )
    )
    (if (setq sel (ssget "_X" (list (cons 8 clock:layer))))
        (repeat (setq inc (sslength sel))
            (entdel (ssname sel (setq inc (1- inc))))
        )
    )
    (foreach bln (list clock:second clock:minute clock:hour clock:day clock:24hr)
        (if (tblsearch "block" bln)
            (vl-catch-all-apply 'vla-delete (list (vla-item blk bln)))
        )
    )
    (if (tblsearch "layer" clock:layer)
        (vl-catch-all-apply 'vla-delete (list (vla-item (vla-get-layers doc) clock:layer)))
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun clock:lwpoly ( lst wid flg ocs )
    (entmakex
        (append
            (list
               '(000 . "LWPOLYLINE")
               '(100 . "AcDbEntity")
               '(100 . "AcDbPolyline")
                (cons 008 clock:layer)
                (cons 090 (length (vl-remove-if-not '(lambda ( x ) (= 10 (car x))) lst)))
                (cons 070 flg)
                (cons 043 wid)
               '(038 . 0.0)
               '(062 . 256)
            )
            lst
            (list (cons 210 ocs))
        )
    )
)

;;----------------------------------------------------------------------;;

(defun clock:circle ( cen rad ocs )
    (entmakex
        (list
           '(0 . "CIRCLE")
            (cons 008 clock:layer)
            (cons 010 cen)
            (cons 040 rad)
           '(062 . 256)
            (cons 210 ocs)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun clock:mtext ( ins str hgt rot ocs )
    (entmakex
        (list
           '(000 . "MTEXT")
           '(100 . "AcDbEntity")
           '(100 . "AcDbMText")
            (cons 008 clock:layer)
            (cons 010 (trans ins ocs 0))
            (cons 011 (trans (list (cos rot) (sin rot) 0.0) ocs 0 t))
            (cons 040 hgt)
            (cons 001 str)
           '(071 . 5)
           '(062 . 2)
            (cons 210 ocs)
        )
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Clock.lsp | Version 1.1 | \\U+00A9 CAD Concepts "
        (menucmd "m=$(edtime,$(getvar,date),YYYY)")
        " www.lee-mac.com ::"
        "\n:: Available commands:"
        "\n::    \"clock\"     -  Toggle clock display."
        "\n::    \"runclock\"  -  Run clock continuously."
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;


;;-------------------------=={ Cursor Rotate }==------------------------;;
;;                                                                      ;;
;;  This program allows the user to rotate the AutoCAD crosshairs       ;;
;;  (that is, modifying the SNAPANG system variable) to align with an   ;;
;;  object residing at a selected point, to a fixed angle, or a fixed   ;;
;;  percentage representing a slope or incline grade.                   ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-09-25                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:cr ( / a e f l o u v x z )
    (setq z (trans '(0 0 1) 1 0 t)
          x (angle '(0 0) (trans (getvar 'ucsxdir) 0 z t))
    )
    (while
        (cond
            (   (or (null u) (= "Object" u))
                (initget "Angle Grade Cancel")
                (setq u (cond ((getpoint "\nPick point on object [Angle/Grade/Cancel] <Angle>: ")) ("Angle")))
                (cond
                    (   (= 'str (type u)))
                    (   (null (setq l (nentselp u)))
                        (princ "\nPoint does not lie on an object.")
                    )
                    (   (or (and (setq e (car l)
                                       o (vlax-ename->vla-object e)
                                 )
                                 (vlax-property-available-p o 'rotation)
                                 (setq a (vla-get-rotation o))
                            )
                            (and (not (vl-catch-all-error-p (setq u (vl-catch-all-apply 'vlax-curve-getclosestpointto (list e (trans u 1 0))))))
                                 (setq a (angle '(0 0) (trans (vlax-curve-getfirstderiv e (vlax-curve-getparamatpoint e u)) 0 z)))
                            )
                        )
                        (if (caddr l)
                            (setq f (lambda ( x ) (reverse (cdr (reverse x))))
                                  v (list (cos a) (sin a) 0.0)
                                  v (mapcar '(lambda ( x ) (apply '+ (mapcar '* (f x) v))) (f (caddr l)))
                                  a (angle '(0 0) (trans v 0 z))
                            )
                        )
                        (not (setvar 'snapang (- a x)))
                    )
                    (   (princ "\nIncompatible object selected."))
                )
            )
            (   (= "Angle" u)
                (initget "Object Grade Cancel")
                (if (numberp (setq u (getangle "\nSpecify angle [Object/Grade/Cancel] <Object>: ")))
                    (not (setvar 'snapang u))
                    t
                )
            )
            (   (= "Grade" u)
                (initget "Object Angle Cancel")
                (if (numberp (setq u (getreal "\nSpecify grade (%) [Object/Angle/Cancel] <Object>: ")))
                    (not (setvar 'snapang (atan (/ u 100.0))))
                    t
                )
            )
            (   (= "Cancel" u) nil)
        )
    )
    (princ)
)
(vl-load-com) (princ)


;;-----------------------=={  Dynamic Text Curve Align  }==----------------------;;
;;                                                                               ;;
;;  The Program will prompt the user to either Select existing text to align,    ;;
;;  or specify New Text. The user will then be prompted to select a curve, and   ;;
;;  the text specified/selected will be dynamically aligned to the selected      ;;
;;  curve.                                                                       ;;
;;                                                                               ;;
;;  Additionally, the user can press +/- to alter the text offset from the curve ;;
;;  and furthermore, toggle the text perpendicularity by pressing 'P' during     ;;
;;  text alignment. A Background Mask can be toggled by pressing 'B' when        ;;
;;  aligning MText. The Text can also be mirrored by pressing 'M' during text    ;;
;;  alignment. The TextStyle and Text Height may be altered by pressing 'S' when ;;
;;  aligning text.                                                               ;;
;;                                                                               ;;
;;  DTRemove:-                                                                   ;;
;;  ------------                                                                 ;;
;;  This function allows the user to remove Text/Object Associativity. Upon      ;;
;;  invoking this function, the user is prompted to select either Text or        ;;
;;  object.                                                                      ;;
;;                                                                               ;;
;;  If the user picks an object, all text associativity with that object is      ;;
;;  removed. If the user picks a text object, the associativity between the      ;;
;;  text and the object that it was aligned with, is removed.                    ;;
;;                                                                               ;;
;;  The user can also press 'A' to clear all associativity within the drawing.   ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  DTCurve / DTRemove                                         ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  ------------                                                                 ;;
;;  Text and MText Justification will be changed to Middle Center Justification  ;;
;;  to allow text to be correctly aligned.                                       ;;
;;                                                                               ;;
;;  Background Mask functionality can only be used when aligning MText.          ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © November 2009 - www.lee-mac.com                 ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  06/11/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  13/11/2009  -  Fixed bug that allowed text to aligned to multiple      ;;
;;                       objects.                                                ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  15/11/2009  -  Added DTRemove to remove align associativity.           ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  15/11/2009  -  Added Ability to Align MText.                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  16/11/2009  -  Added Ability to use a Background Mask, when aligning   ;;
;;                       MText.                                                  ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  16/11/2009  -  General Bug Fixes.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  17/11/2009  -  Updated code to work in all UCS.                        ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  17/11/2009  -  Added Logo to DTRemove function.                        ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  17/11/2009  -  Fixed Bug to allow correct text offset/perpendicularity ;;
;;                       setting when updating text by Reactor.                  ;;
;;                    -  General Bug Fixes.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  17/11/2009  -  Fixed Text Rotation bug for Vertical lines.             ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  17/11/2009  -  General Bug Fixes.                                      ;;
;;                    -  Added ability for user to manually input offset         ;;
;;                       distance.                                               ;;
;;-------------------------------------------------------------------------------;;
;;  2.1:  19/11/2009  -  DTRemove updated to remove all XData from object.       ;;
;;-------------------------------------------------------------------------------;;
;;  2.2:  20/11/2009  -  Added Mirror Text Option.                               ;;
;;-------------------------------------------------------------------------------;;
;;  2.3:  25/11/2009  -  Added TextStyle/Height Dialog.                          ;;
;;-------------------------------------------------------------------------------;;
;;  2.4:  02/12/2009  -  Fixed Dialog Bug.                                       ;;
;;-------------------------------------------------------------------------------;;
;;  2.5:  03/12/2009  -  Added Selection highlighting.                           ;;
;;-------------------------------------------------------------------------------;;
;;  2.6:  04/12/2009  -  Upgraded DTRemove function to include option to Remove  ;;
;;                       All associativity from objects.                         ;;
;;-------------------------------------------------------------------------------;;
;;  2.7:  07/12/2009  -  Re-Structured xData Storage to allow text to be aligned ;;
;;                       to same part of object upon re-alignment                ;;
;;-------------------------------------------------------------------------------;;
;;  2.8:  15/02/2010  -  Fixed realignment bug when line length is less than     ;;
;;                       text.                                                   ;;
;;                    -  General Bug Fixes.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  2.9:  16/02/2010  -  Updated Code Layout.                                    ;;
;;                    -  Added call to turn off reactor whilst main function is  ;;
;;                       running.                                                ;;
;;-------------------------------------------------------------------------------;;

;;-------------------------------------------------------------------------------;;
;;                           --=={  Preliminaries  }==--                         ;;
;;-------------------------------------------------------------------------------;;


(setq app "DTCURVE")                           ;; XData Application Name

(or *Mac$Str*  (setq *Mac$Str* "text"))        ;; First-time Default Text String
(or *Mac$Per*  (setq *Mac$Per* (/ pi 2.)))     ;; Default Perpendicularity Setting
(or *Mac$tOff* (setq *Mac$tOff* 1.))           ;; Default Offset Factor
(or *Mac$Mirr* (setq *Mac$Mirr* 0.))           ;; Default Mirror Setting
(setq *Mac$tObj* "MTEXT")                      ;; Default Text Object to be Created [TEXT/MTEXT]

(setq *Mac$Mask* nil)                          ;; Background Mask as Default [t/nil]

;;-------------------------------------------------------------------------------;;=


;;-------------------------------------------------------------------------------;;
;;                            --=={ Main Function  }==--                         ;;
;;-------------------------------------------------------------------------------;;


(defun c:DTCurve (/  ; --=={  Local Functions  }==--

                       *error* putxdat

                      ; --=={  Local Variables  }==--

                       CANG COBJ CODE
                       DATA DC_FNAME DC_TITLE DERV DIS DOC DRFT
                       ENT
                       GR
                       HENT HI HND
                       LANG
                       MSG
                       ONME OPT OSGRV OV
                       PLST PT
                       RSLT
                       SPC STR
                       TOBJ TSTR TSZE TYP
                       UFLAG
                       VAL VL
                       XDAT XTYPE XVAL

                      ; --=={  Global Variables  }==--

                      ;  *Mac$Str*   ~  Default Text String
                      ;  *Mac$tOff*  ~  Default Offset Factor
                      ;  *Mac$Per*   ~  Default Perpendicularity Setting
                      ;  *Mac$tObj*  ~  Default Object Creation Setting (TEXT/MTEXT)
                      ;  *Mac$Mask*  ~  Default Background Mask Setting
                      ;  *Mac$Mirr*  ~  Default Text Mirror Setting

                   )
  
  (vl-load-com)

  (setq dc_fname "LMAC_DTCurve_V2.9.dcl"
        dc_title "DTCurve V2.9 Settings")
  

  ;;  --=={  Error Handler  }==--

  (defun *error* (msg)
    
    (and tObj
         (or
           (and pLst
                (mapcar
                  (function
                    (lambda (x)
                      (vlax-put tObj (car x) (cdr x)))) pLst))
             (and
               (not
                 (vlax-erased-p tObj)) (vla-delete tObj))))

    (mapcar (function set) '(tObj pLst) (list 'nil 'nil))
    (if (and uflag doc) (vla-EndUndoMark doc))

    ;(vl-bt)

    (if (not (vlr-added-p *DCurve$Align$Reactor*))
      (vlr-add *DCurve$Align$Reactor*))

    (and ov (mapcar (function setvar) vl ov))
    
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    
    (redraw)

    (princ))
  
  
  ;;-------------------------------------------------------------------------------;;


  (defun putxdat (Obj App Data / xtype xval)

    (setq xtype
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbInteger '(0 . 1)) '(1001 1000))))

    (setq xval
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbVariant '(0 . 1)) (list App Data))))

    (vla-setXData Obj xtype xval))
  

  ;;-------------------------------------------------------------------------------;;
  

  (if (eq 4 (logand 4 (cdr (assoc 70 (tblsearch "LAYER" (getvar "CLAYER"))))))
    (progn
      (princ "\n<< Current Layer Locked >>") (exit)))
  

  (setq doc (vla-get-ActiveDocument
              (vlax-get-Acad-Object))
        
        spc (if (zerop (vla-get-activespace doc))
              (if (= (vla-get-mspace doc) :vlax-true)
                (vla-get-modelspace doc)
                (vla-get-paperspace doc))
              (vla-get-modelspace doc))
        
        drft  (vla-get-drafting
                (vla-get-preferences
                  (vlax-get-acad-object)))
        
        osGrv (osmode-grvecs-lst
                (vla-get-AutoSnapMarkerColor drft)
                  (vla-get-AutoSnapMarkerSize drft)))
  

  (setq vl '("OSMODE") ov (mapcar (function getvar) vl))

  (setq str "")
  
  ;;-------------------------------------------------------------------------------;;
  

  ;; LE - disable the reactor here to avoid the call to any notifier
  ;; must be before the selection of a text
  (if (vlr-added-p *DCurve$Align$Reactor*) (vlr-remove *DCurve$Align$Reactor*))
  

  ;;-------------------------------------------------------------------------------;;

  ;;  --=={  Text Entry/Selection  }==-- 

  (princ (setq msg (strcat "\nType or Select Text <" *Mac$Str* "> : ")))
  
  (while
    (progn
      (setq gr (grread 't 15 2) code (car gr) data (cadr gr))

      (cond (  (and (= 5 code) (listp data))

               (if (and (setq hi (car (nentselp data)))
                        (vl-position (cdr (assoc 0 (entget hi))) '("MTEXT" "TEXT")))
                 
                 (redraw (setq hEnt hi) 3)
                 
                 (if hEnt (setq hEnt (redraw hEnt 4))))
             
               t)

            (  (and (= 3 code) (listp data))

               (cond (  (setq ent (car (nentselp data)))

                        (if (not (vl-position (cdr (assoc 0 (entget ent))) '("MTEXT" "TEXT")))

                          (princ (strcat "\n** Object Must be Text/MText **" msg))
                          (progn
                            (if hEnt (setq hEnt (redraw hEnt 4)))

                            (setq rslt ent ent nil))))

                     (t (princ (strcat "\n** Missed, Try Again... **" msg)))))

            (  (= 25 code)

               (setq rslt str str nil))

            (  (= 2 code)

               (cond (  (<= 32 data 126)

                        (setq str (strcat str (princ (chr data)))))

                     (  (and (< 0 (strlen str)) (= 8 data))

                        (setq str (substr str 1 (1- (strlen str))))
                        (princ (vl-list->string '(8 32 8))))

                     (  (= 13 data)

                        (setq rslt str str nil))

                     (t )))
            
             (t ))))
  

  ;;-------------------------------------------------------------------------------;;

  ;;  --=={  Functions to Handle Result of User Input  }==-- 

  (vla-StartUndoMark doc)
  (setq uFlag t)  

  (cond (  (eq "" rslt))

        (  (eq 'STR (type rslt))
         
           (setq *Mac$Str* rslt))

        (  (eq 'ENAME (type rslt))

           (setq *Mac$Str* (vla-get-TextString
                             (setq tObj (vlax-ename->vla-object rslt)))
                 
                 tSze (vla-get-Height tObj)

                 Hnd  (vla-get-Handle tObj))
                 

           (cond (  (eq "AcDbText" (vla-get-ObjectName tObj))

                    (setq pLst (list (cons 'Alignment (vlax-get tObj 'Alignment))
                                     
                                     (if (eq acAlignmentLeft (vla-get-Alignment tObj))
                                       (cons 'InsertionPoint (vlax-get tObj 'InsertionPoint))
                                       (cons 'TextAlignmentPoint (vlax-get tObj 'TextAlignmentPoint))))))

                 (t (setq pLst (list (cons 'AttachmentPoint (vlax-get tObj 'AttachmentPoint))
                                     (cons 'InsertionPoint  (vlax-get tObj 'InsertionPoint))))))


           (foreach obj (vlr-owners *DCurve$Align$Reactor*)

             (if (not (vlax-erased-p obj))
               (progn

                 (vla-GetXData obj app 'typ 'val)

                 (if (and typ val)
                   (progn

                     (setq xDat

                            (cond (  (not (setq xDat
                                            (vlax-variant-value
                                              (cadr
                                                (vlax-safearray->list val)))))

                                     nil)

                                  (  (vl-remove-if-not
                                       (function
                                         (lambda (lst)
                                           (entget (handent (car lst)))))

                                       (read xDat)))

                                  (t nil)))
                     
                     (if (vl-position Hnd (mapcar (function car) xDat))
                       (progn
                         (putxDat obj app
                           (vl-prin1-to-string
                             (vl-remove-if
                               (function
                                 (lambda (x) (eq Hnd (car x)))) xDat))))))))

               (vlr-owner-remove *DCurve$Align$Reactor* obj)))))
  

  (setq tStr *Mac$Str*)
  (or tSze (setq tSze (getvar 'TEXTSIZE)))
  

  ;;-------------------------------------------------------------------------------;;

  ;;  --=={  Curve Selection  }==-- 

  (while
    (progn
      (setq ent (nentsel "\nSelect Curve: "))

      (cond (  (vl-consp ent)

               (if (vl-catch-all-error-p
                     (vl-catch-all-apply
                       (function vlax-curve-getEndParam)
                         (list (setq cObj (vlax-ename->vla-object (car ent))))))
                 
                 (princ "\n** Object not Valid **")))

            (t (princ "\n** Missed, Try Again... **")))))
  

  ;;-------------------------------------------------------------------------------;;

  ;;  --=={  Text Setup  ~  Alignment/Creation  }==-- 

  (cond (tObj

          (if (eq "AcDbText" (vla-get-ObjectName tObj))
            (vla-put-Alignment tObj acAlignmentMiddleCenter)
            (vla-put-attachmentpoint tObj acAttachmentPointMiddleCenter)))

        (t

          (if (eq "TEXT" (strcase *Mac$tObj*))
            
            (vla-put-Alignment
              (setq tObj (vla-addText spc tStr (vlax-3D-point '(0 0 0)) tSze)) acAlignmentMiddleCenter)
            
            (vla-put-AttachmentPoint
              (setq tObj (vla-addMText spc (vlax-3D-point '(0 0 0)) 0 tStr)) acAttachmentPointMiddleCenter))))
  

  (setq oNme (vla-get-ObjectName tObj))

  (and *Mac$Mask* (eq "AcDbMText" oNme)
       (vla-put-BackgroundFill tObj :vlax-true))

  ;; Program doesn't work too well with NODE/INSERTION Snap enabled

  (setvar "OSMODE" (boole 2 (getvar "OSMODE") 72))
  

  ;;-------------------------------------------------------------------------------;;

  ;;-------------------------------------------------------------------------------;;
  ;;               --=={  Main Alignment GrRead Loop  }==--                        ;;
  ;;-------------------------------------------------------------------------------;;
  

  (princ (setq msg (strcat "\n[+] or [-] for [O]ffset, [P]erpendicularity Toggle"
                           "\n[M]irror Text, [S]tyle Settings"
                           (if (eq "AcDbText" oNme) "" ", [B]ackground Mask"))))

  (while
    (progn
      (setq gr (grread t 15 0) code (car gr) data (cadr gr))

      (redraw)        

      (cond (  (and (= 5 code) (listp data))
               (setq data (trans data 1 0))

               (setq pt (vlax-curve-getClosestPointto cObj data))

               (if (and (< 0 (getvar "OSMODE") 16384)
                        (setq oPt (vl-remove-if (function null)
                                    (mapcar
                                      (function
                                        (lambda (x / o)
                                          (if (setq o (osnap data x))
                                            (list (distance data o) o x data)))) (get_osmode)))))
                 
                 (setq oPt (cdar (vl-sort oPt (function (lambda (a b) (< (car a) (car b)))))))
                 (setq oPt nil))

               (and oPt (OsMark oPt))

               (setq cAng (angle pt data) lAng (+ cAng *Mac$Per*))

               (if (equal lAng (/ pi 2.) 0.001)
                 (setq lAng (/ pi 2.)))

               (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                 (setq lAng (/ (* 3 pi) 2.)))

               (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                        (setq lAng (- lAng pi)))
                   
                     (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                        (setq lAng (+ lAng pi))))

               (  (if (eq "AcDbText" oNme)
                    vla-put-TextAlignmentPoint
                    vla-put-InsertionPoint)
                 
                 tObj (vlax-3D-point (polar pt cAng (* tSze *Mac$tOff*))))

               (vla-put-Rotation tObj (+ *Mac$Mirr* lAng))

             t)

            (  (= 2 code)

               (cond (  (vl-position data '(43 61))
                      
                        (setq *Mac$tOff* (+ 0.1 *Mac$tOff*)))

                     (  (= data 45)
                      
                        (setq *Mac$tOff* (-  *Mac$tOff* 0.1)))

                     (  (vl-position data '(77 109))

                        (setq *Mac$Mirr* (- pi *Mac$Mirr*)))

                     (  (vl-position data '(83 115))

                        (TextSettings tObj)
                        (setq tSze (vla-get-Height tObj))

                        (princ msg))

                     (  (vl-position data '(79 111))

                        (if (setq dis (getdist
                                        (strcat "\nSpecify Offset Distance <"
                                                (rtos (* tSze *Mac$tOff*) 2 3) "> : ")))
                          (setq *Mac$tOff* (/ dis (float tSze))))

                        (princ msg))

                     (  (vl-position data '(80 112))
                      
                        (setq *Mac$Per* (- (/ pi 2.) *Mac$Per*)))

                     (  (vl-position data '(66 98))

                        (if (eq oNme "AcDbMText")
                          (vla-put-backgroundfill tObj
                            (if (eq :vlax-true (vla-get-BackGroundFill tObj)) :vlax-false :vlax-true)))

                        t)

                     (  (= 6 data)  ; F3
                      
                        (cond (  (< 0 (getvar "OSMODE") 16384)                               
                                 (setvar "OSMODE" (+ 16384 (getvar "OSMODE")))
                                 (princ "\n<Osnap off>"))
                              
                              (t (setvar "OSMODE" (- (getvar "OSMODE") 16384))
                                 (princ "\n<Osnap on>")))

                        (princ msg))
                     
                     (  (vl-position data '(13 32))

                        (SubData)

                      nil)

                     (t )))

            (  (and (= 3 code) (listp data))
               (setq data (trans data 1 0))

               (SubData)

               (if (and (< 0 (getvar "OSMODE") 16384)
                        (setq oPt (vl-remove-if (function null)
                                    (mapcar
                                      (function
                                        (lambda (x / o)
                                          (if (setq o (osnap data x))
                                            (list (distance data o) o x data)))) (get_osmode)))))
                 
                 (setq oPt  (cdar  (vl-sort oPt (function (lambda (a b) (< (car a) (car b))))))
                       pt   (osnap (caddr oPt) (cadr oPt))

                       derv (angle '(0 0 0) (vlax-curve-getFirstDeriv cObj
                                              (vlax-curve-getParamatPoint cObj
                                                (vlax-curve-getClosestPointto cObj pt))))

                       derv (- (+ derv (if (< (distance (polar pt derv 1) data)
                                              (distance (polar pt (+ pi derv) 1) data)) 0 pi)) (/ pi 2.))))

               (setq pt   (vlax-curve-getClosestPointto cObj data) cAng (if derv derv (angle pt data)) lAng (+ cAng *Mac$Per*))

               (if (equal lAng (/ pi 2.) 0.001)
                 (setq lAng (/ pi 2.)))

               (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                 (setq lAng (/ (* 3 pi) 2.)))

               (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                        (setq lAng (- lAng pi)))

                     (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                        (setq lAng (+ lAng pi))))

               (  (if (eq "AcDbText" oNme)
                    vla-put-TextAlignmentPoint
                    vla-put-InsertionPoint)

                  tObj (vlax-3D-point (polar pt cAng (* tSze *Mac$tOff*))))

               (vla-put-Rotation tObj (+ *Mac$Mirr* lAng))

             nil)

            (  (= 25 code) nil)

            (t ))))
  

  ;;-------------------------------------------------------------------------------;;


  (vla-EndUndoMark doc)
  (redraw)

  ;; LE - make the reactor active here - we are safe to allow the chance
  ;; to update the possible text around the notifiers (lines) when they are modified
  (if (not (vlr-added-p *DCurve$Align$Reactor*)) (vlr-add *DCurve$Align$Reactor*))

  (mapcar (function setvar) vl ov)
  (princ))


;;-------------------------------------------------------------------------------;;
;;                           --=={  Sub Functions  }==--                         ;;
;;-------------------------------------------------------------------------------;;


(defun SubData (/ p# t#)
  
  (vla-GetXData cObj app 'typ 'val)
  (setq hand (list (vla-get-Handle tObj)
                   (vla-get-Handle cObj) *Mac$tOff* *Mac$Per* *Mac$Mirr*
                   (vlax-curve-getParamatPoint cObj
                     (setq p#
                       (vlax-curve-getClosestPointto cObj
                         (setq t#
                           (vlax-safearray->list
                             (vlax-variant-value
                               (  (if (eq "AcDbText" (vla-get-ObjectName tObj))
                                    vla-get-TextAlignmentPoint
                                    vla-get-InsertionPoint)

                                 tObj)))))))
                   
                   (- (angle p# t#)
                      (angle '(0 0 0)
                        (vlax-curve-getFirstDeriv cObj
                          (vlax-curve-getParamatPoint cObj p#))))))
  (if (and typ val)
    (progn
      (setq xDat (vl-remove-if-not
                   (function
                     (lambda (lst)
                       (entget (handent (car lst)))))
                   
                   (read
                     (vlax-variant-value
                       (cadr
                         (vlax-safearray->list val))))))

      (if (not (vl-position (car hand) (mapcar (function car) xdat)))
        (setq xdat (append xdat (list hand)))))

    (setq xdat (list hand)))   

  (putxdat cObj app (vl-prin1-to-string xdat))

  (or (vl-position cObj (vlr-owners *DCurve$Align$Reactor*))
      (vlr-owner-add *DCurve$Align$Reactor* cObj)))


;;-------------------------------------------------------------------------------;;
;;                           --=={  Reactor Callback  }==--                      ;;
;;-------------------------------------------------------------------------------;;

(defun DTCurveUpd (Obj Reac Args / *error*

                                   typ val ObjxDat tObj dist perp
                                   mirr para a#dif pt deriv cAng lAng)

  (defun *error* (msg)
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ))
  

  (cond (  (and *Reactor$Command$Call*

                (or (wcmatch *Reactor$Command$Call* "*UNDO")
                    (eq "U" *Reactor$Command$Call*))))    

        (t (if (vlax-erased-p obj)
             (vlr-owner-remove Reac Obj)

             (progn

               (or doc (setq doc (vla-get-ActiveDocument
                                   (vlax-get-acad-object))))
               
               (vla-getXdata obj app 'typ 'val)

               (setq ObjxDat

                            (cond (  (not (setq ObjxDat
                                            (vlax-variant-value
                                              (cadr
                                                (vlax-safearray->list val)))))

                                     nil)

                                  (  (vl-remove-if-not
                                       (function
                                         (lambda (lst)
                                           (entget (handent (car lst)))))

                                       (read ObjxDat)))

                                  (t nil)))

               (foreach lst  ObjxDat

                 (setq tObj  (vlax-ename->vla-object (handent (car lst))))

                 (mapcar (function set) '(dist perp mirr para a#dif) (cddr lst))

                 (if (eq "AcDbText" (vla-get-ObjectName tObj))
                   (vla-put-Alignment tObj acAlignmentMiddleCenter)
                   (vla-put-AttachmentPoint tobj acAttachmentPointMiddleCenter))

                 (setq pt
                        
                   (cond (  (vlax-curve-getPointatParam Obj para))

                         (  (vlax-curve-getClosestPointto Obj
                              (vlax-get tObj
                                (if (eq "AcDbText" (vla-get-ObjectName tObj))
                                  'TextAlignmentPoint
                                  'InsertionPoint))))))

                 (setq para  (vlax-curve-getParamatPoint Obj pt)

                       deriv (+ (angle '(0 0 0)
                                       (vlax-curve-getFirstDeriv Obj para)) a#dif))

                 (setq cAng deriv lAng (rem (+ perp cAng) (* 2. pi)))

                 (if (equal lAng (/ pi 2.) 0.001)
                   (setq lAng (/ pi 2.)))

                 (if (equal lAng (/ (* 3 pi) 2.) 0.001)
                   (setq lAng (/ (* 3 pi) 2.)))

                 (and (minusp lAng) (setq lAng (+ lAng (* 2 pi))))
                 
                 (cond (  (and (> lAng (/ pi 2)) (<= lAng pi))
                          (setq lAng (- lAng pi)))

                       (  (and (> lAng pi) (<= lAng (/ (* 3 pi) 2)))
                          (setq lAng (+ lAng pi))))

                 (  (if (eq "AcDbText" (vla-get-ObjectName tObj))
                      vla-put-TextAlignmentPoint
                      vla-put-InsertionPoint)

                      tObj (vlax-3D-point (polar pt cAng (* (vla-get-Height tObj) dist))))

                 (vla-put-Rotation tObj (+ mirr lAng)))))))
      
  (princ))


;;-------------------------------------------------------------------------------;;


(defun DTCurveComm (Reac Args)
  (setq *Reactor$Command$Call* (strcase (car Args)))
  (princ))


;;-------------------------------------------------------------------------------;;


(defun icon_GrList (col sze / -sze)

  (setq sze (1+ sze) -sze (- sze))

  (list col (list       0.  (1- -sze)) (list      0.      sze)
        col (list       1.  (1- -sze)) (list      1.      sze)
        col (list     -sze       sze)  (list     sze      sze)
        col (list (1- -sze) (1+  sze)) (list (1+ sze) (1+ sze))))

;;-------------------------------------------------------------------------------;;
;;                           --=={  OSnap Functions  }==--                       ;;
;;-------------------------------------------------------------------------------;;

(defun osMark (o / s)
  
  (setq s (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
        o (cons (trans (car o) 1 3) (cdr o)))

  (grvecs (cdr (assoc (cadr o) osGrv))
          (list (list s 0. 0. (caar o))
                (list 0. s 0. (cadar o))
                (list 0. 0. s 0.)
                (list 0. 0. 0. 1.))))


;;-------------------------------------------------------------------------------;;

(defun get_osmode nil ; by Evgeniy Elpanov
  (mapcar
    (function cdr)
      (vl-remove-if
        (function (lambda (x) (zerop (logand (getvar "OSMODE") (car x)))))
        '((1    . "_end")
          (2    . "_mid")
          (4    . "_cen")
          (8    . "_nod")
          (16   . "_qua")
          (32   . "_int")
          (64   . "_ins")
          (128  . "_per")
          (256  . "_tan")
          (512  . "_nea")
          (2048 . "_app")))))

;;-------------------------------------------------------------------------------;;

(defun osmode-grvecs-lst (col ass / -ASS ASS COL)
   ; By Evgeniy Elpanov (Modified by Lee Mac)
  
  (setq -ass (- ass))
  
  (list (list "_end"
              col (list -ass -ass) (list -ass  ass)
              col (list (1-  -ass) (1- -ass)) (list (1- -ass) (1+  ass))              
              col (list -ass  ass) (list  ass  ass)
              col (list (1-  -ass) (1+  ass)) (list (1+  ass) (1+  ass))              
              col (list  ass  ass) (list  ass -ass)
              col (list (1+   ass) (1+  ass)) (list (1+  ass) (1- -ass))              
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_mid"
              col (list -ass -ass) (list    0. ass)
              col (list (1-  -ass) (1- -ass)) (list 0. (1+  ass))
              col (list    0. ass) (list  ass -ass)
              col (list 0. (1+  ass)) (list (1+  ass) (1- -ass))
              col (list  ass -ass) (list -ass -ass)
              col (list (1+   ass) (1- -ass)) (list (1- -ass) (1- -ass)))
        
        (list "_cen"
              7   (list (* -ass 0.2) 0.)  (list (*  ass 0.2) 0.)
              7   (list  0. (* -ass 0.2)) (list  0.  (*  ass 0.2))
              col (list    -ass   0.)     (list (* -ass 0.86) (* ass  0.5))
              col (list (* -ass 0.86) (* ass  0.5))  (list (* -ass  0.5) (* ass 0.86))
              col (list (* -ass  0.5) (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_nod"
              col (list -ass -ass)    (list ass ass)
              col (list -ass ass)     (list ass -ass)
              col (list -ass 0.)      (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5))   (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5)  (* ass 0.86))  (list 0. ass)
              col (list 0. ass) (list (* ass 0.5)    (* ass 0.86))
              col (list (* ass 0.5)   (* ass 0.86))  (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5))   (list ass 0.)
              col (list ass 0.) (list (* ass 0.86)   (* -ass 0.5))
              col (list (* ass 0.86)  (* -ass 0.5))  (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5)   (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5)   (* -ass 0.86))
              col (list (* -ass 0.5)  (* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86) (* -ass 0.5))  (list -ass 0.))

        (list "_qua"
              col (list 0. -ass)   (list -ass 0.)
              col (list 0. (1- -ass))   (list (1- -ass) 0.)
              col (list -ass 0.)   (list 0. ass)
              col (list (1- -ass) 0.)   (list 0. (1+ ass))
              col (list 0. ass)    (list ass 0.)
              col (list 0. (1+ ass))    (list (1+ ass) 0.)
              col (list ass 0.)    (list 0. -ass)
              col (list (1+ ass) 0.)    (list 0. (1- -ass)))

        (list "_int"
              col (list -ass -ass) (list ass ass)
              col (list -ass (1+ -ass)) (list ass (1+ ass))
              col (list (1+ -ass) -ass) (list (1+ ass) ass)
              col (list -ass ass)  (list ass -ass)
              col (list -ass (1+ ass))  (list ass (1+ -ass))
              col (list (1+ -ass) ass)  (list (1+ ass) -ass))

        (list "_ins"
              col (list (* -ass 0.1) (* -ass 0.1)) (list -ass (* -ass 0.1))
              col (list -ass (* -ass 0.1)) (list -ass ass)
              col (list -ass ass) (list (* ass 0.1) ass)
              col (list (* ass 0.1) ass)   (list (* ass 0.1) (* ass 0.1))
              col (list (* ass 0.1) (* ass 0.1))   (list ass (* ass 0.1))
              col (list ass (* ass 0.1))   (list ass -ass)
              col (list ass -ass) (list (* -ass 0.1) -ass)
              col (list (* -ass 0.1) -ass) (list (* -ass 0.1) (* -ass 0.1))
              col (list (1- (* -ass 0.1)) (1- (* -ass 0.1))) (list (1- -ass) (1- (* -ass 0.1)))
              col (list (1- -ass) (1- (* -ass 0.1))) (list (1- -ass) (1+ ass))
              col (list (1- -ass) (1+ ass)) (list (1+ (* ass 0.1)) (1+ ass))
              col (list (1+ (* ass 0.1)) (1+ ass)) (list (1+ (* ass 0.1)) (1+ (* ass 0.1)))
              col (list (1+ (* ass 0.1)) (1+ (* ass 0.1))) (list (1+ ass) (1+ (* ass 0.1)))
              col (list (1+ ass) (1+ (* ass 0.1)))   (list (1+ ass) (1- -ass))
              col (list (1+ ass) (1- -ass)) (list (1- (* -ass 0.1)) (1- -ass))
              col (list (1- (* -ass 0.1))   (1- -ass)) (list (1- (* -ass 0.1)) (1- (* -ass 0.1))))

        (list "_tan"
              col (list -ass ass) (list ass ass)
              col (list (1- -ass) (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass 0.)  (list (* -ass 0.86) (* ass 0.5))
              col (list (* -ass 0.86) (* ass 0.5)) (list (* -ass 0.5) (* ass 0.86))
              col (list (* -ass 0.5) (* ass 0.86)) (list 0. ass)
              col (list 0. ass) (list  (* ass 0.5) (* ass 0.86))
              col (list (* ass 0.5)  (* ass 0.86)) (list (* ass 0.86) (* ass 0.5))
              col (list (* ass 0.86)  (* ass 0.5)) (list ass 0.)
              col (list ass 0.) (list (* ass 0.86) (* -ass 0.5))
              col (list (* ass 0.86) (* -ass 0.5)) (list (* ass 0.5) (* -ass 0.86))
              col (list (* ass 0.5) (* -ass 0.86)) (list 0. -ass)
              col (list 0. -ass)(list (* -ass 0.5) (* -ass 0.86))
              col (list (* -ass 0.5)(* -ass 0.86)) (list (* -ass 0.86) (* -ass 0.5))
              col (list (* -ass 0.86)(* -ass 0.5)) (list -ass 0.))

        (list "_per"
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass))
              col (list -ass 0.)   (list 0. 0.)
              col (list -ass -1.)  (list 0. -1.)
              col (list 0. 0.)     (list 0. -ass)
              col (list -1. 0.)    (list -1. -ass))

        (list "_nea"
              col (list -ass -ass) (list ass ass)
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass)) (list (1+ ass) (1+ ass))
              col (list -ass ass)  (list ass -ass)
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass) (1- -ass)) (list (1- -ass) (1- -ass)))

        (list "_app"
              col (list -ass -ass) (list ass ass)
              col (list ass -ass)  (list -ass ass)
              col (list -ass -ass) (list -ass ass)
              col (list (1- -ass)  (1- -ass)) (list (1- -ass) (1+ ass))
              col (list -ass ass)  (list ass ass)
              col (list (1- -ass)  (1+ ass))  (list (1+ ass) (1+ ass))
              col (list ass ass)   (list ass -ass)
              col (list (1+ ass)   (1+ ass))  (list (1+ ass) (1- -ass))
              col (list ass -ass)  (list -ass -ass)
              col (list (1+ ass)   (1- -ass)) (list (1- -ass) (1- -ass)))))


;;-------------------------------------------------------------------------------;;
;;                           --=={  DCL Functions  }==--                         ;;
;;-------------------------------------------------------------------------------;;

(defun TextSettings (Obj / *error* txt2num dcl_write logo

                           dcTag tLst oHgt tog hgt sty)
  

  (defun *error* (e)
    (and dcTag (unload_dialog dcTag))
    (and ofile (close ofile))
    (or (wcmatch (strcase e) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " e " **")))
    (princ))

  (defun txt2num ( txt )
    
    (cond (  (distof txt 5))
          (  (distof txt 2))
          (  (distof txt 1))
          (  (distof txt 4))
          (  (distof txt 3))))
          
  (defun dcl_write (fname / wPath ofile)

    (if (not (findfile fname))

      (if (setq wPath (findfile "ACAD.PAT"))
        (progn
          (setq wPath (vl-filename-directory wPath))
          
          (or (eq "\\" (substr wPath (strlen wPath)))
              (setq wPath (strcat wPath "\\")))

          (setq ofile (open (strcat wPath fname) "w"))
          (foreach str

           '("// DT_Curve.dcl for use in conjunction with DTCurve.lsp  //"
             "// Copyright © November 2009 Lee Mac                     //"
             ""
             "dt_curve : dialog { key = \"stitle\";"
             ""
             "  spacer;"
             "   "
             "  : row { alignment = centered;"
             "  "
             "    : boxed_column { label = \"TextStyle\";"
             "    "
             "      : popup_list { key = \"styl\"; alignment = centered; fixed_width = true; width = 20; }"
             ""
             "      spacer_1;"
             ""
             "    }"
             ""
             "    : boxed_column { label = \"Height\";"
             ""
             "      : row { children_alignment = centered; spacer;"
             ""
             "        : edit_box { key = \"hgt\"; edit_width = 5; alignment = centered; }"
             "  "
             "        spacer;"
             ""
             "        : column {"
             ""
             "          : spacer { height = 0.01; }"
             ""
             "          : toggle { key = \"bstyl\"; label = \" By Style\"; alignment = centered; }"
             ""
             "        }"
             ""
             "      }"
             "      "
             "      spacer;"
             "      "
             "    }"
             ""
             "  }"
             ""
             "  spacer;"
             ""
             "  : row { alignment = centered;"
             ""
             "    : spacer { width = 16.06; fixed_width = true; }"
             ""
             "    ok_cancel;"
             ""
             "    : image { key = \"logo\"  ; alignment    = centered;"
             "              width = 16.06 ; fixed_width  = true;"
             "              height = 2.06 ; fixed_height = true; color = -15; }"
             "              "
             "  }"
             "  "
             "}")

            (write-line str ofile))
          
        (setq ofile (close ofile))
          
        t)  ; File written successfully
        
    nil) ; Filepath not Found
      
  t)) ; DCL file already exists


  ;;-------------------------------------------------------------------------------;;
  

  (defun logo (key)
    
    (start_image key)
    (mapcar 'vector_image
            '(22 21 1 0 0 0 0 7 0 0 0 0 1 6 6 6 6 7 43 36 27 36 30 21 21 21 22 22 22
              22 21 21 21 28 28 28 27 27 30 29 29 30 52 43 43 43 44 44 46 46 45 45 45
              45 52 52 52 51 51 51 51 51 52 62 65 66 68 68 68 68 67 67 75 75 75 74 74
              73 66 58 58 59 59 59 59 52 57 57 56 56 56 56 57 58 65 65 65 65 66 95 94
              94 92 91 91 91 90 89 89 88 87 86 85 74 74 75 75 76 77 78 79 80 81 82 83
              84 85 86 87 88 88 89 90 91 92 93 94 95 74 73 73 72 72 71 71 71 71 71 71
              71 72 72 72 73 84 83 82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79
              79 80 81 82 83 94 94 95 83 83 82 81 80 79 78 77 76 75 74 84 85 86 87 88
              89 89 90 91 91 91 91 92 95 94 93 92 91 90 89 89 88 87 86 85 84)

            '(20 20 23 23 23 24 24 0 0 0 0 1 1 20 1 1 1 0 2 24 7 15 0 0 0 0 1 1 23 23
              23 24 24 24 24 24 23 23 2 1 1 0 0 0 0 0 1 1 7 23 23 23 24 24 24 24 24 23
              23 1 1 1 0 10 16 19 21 22 23 24 24 24 24 24 24 23 23 22 4 4 5 5 6 6 7 24
              24 24 24 23 23 22 19 16 7 7 6 5 5 22 22 22 17 17 18 18 19 20 20 20 21 21
              21 21 22 23 23 23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22
              22 22 7 8 8 9 10 11 12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19
              18 17 16 15 14 13 12 12 11 10 9 9 8 8 8 7 7 7 7 4 4 4 4 4 4 4 5 5 6 6 7 7
              8 8 8 9 9 9 10 11 11 11 11 7 7 7 6 6 5 5 4 4 4 4 4 4)

            '(21 6 0 0 0 0 21 0 0 0 0 1 1 6 6 6 7 7 36 46 36 30 21 21 21 22 22 22 22 21
              21 21 28 28 28 27 27 27 29 29 30 30 43 43 43 44 44 43 46 45 45 45 45 52 52
              52 51 51 51 51 51 52 52 65 58 68 68 68 68 67 67 75 75 75 74 74 73 65 58 58
              59 59 59 59 51 57 57 56 56 56 56 57 66 62 65 65 65 66 66 94 94 95 91 91 91
              90 89 89 88 87 86 85 84 74 75 75 76 77 78 79 80 81 82 83 84 85 86 87 88 88
              89 90 91 92 93 94 95 92 73 73 72 72 71 71 71 71 71 71 71 72 72 72 73 74 83
              82 81 80 79 79 78 77 77 76 76 76 76 76 77 77 78 79 79 80 81 82 83 84 94 95
              94 83 82 81 80 79 78 77 76 75 74 74 85 86 87 88 89 89 90 91 91 91 91 92 95
              94 93 92 91 90 89 89 88 87 86 85 84 83)

            '(20 20 23 23 24 24 24 0 0 0 1 1 23 1 1 1 0 0 15 7 24 2 0 0 0 1 1 23 23 23 24
              24 24 24 24 23 23 7 1 1 0 0 0 0 0 1 1 2 23 23 23 24 24 24 24 24 23 23 1 1 1
              0 0 16 16 21 22 23 24 24 24 24 24 24 23 23 22 7 4 5 5 6 6 7 22 24 24 24 23
              23 22 19 19 10 7 6 5 5 4 22 22 22 17 18 18 19 20 20 20 21 21 21 21 22 23 23
              23 24 24 24 25 25 25 25 25 25 25 25 24 24 24 23 23 22 22 22 22 17 8 8 9 10 11
              12 13 14 15 16 17 18 19 19 20 21 21 21 21 20 20 19 19 18 17 16 15 14 13 12 12
              11 10 9 9 8 8 8 7 7 7 7 7 4 4 4 4 4 4 5 5 6 6 7 7 8 8 8 9 9 9 10 11 11 11 11
              7 7 7 6 6 5 5 4 4 4 4 4 4 4)

            '(178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178 178
              178 178 178 178 178 178 178 178 178))
    
  (end_image))
  

  ;;-------------------------------------------------------------------------------;;
  

  (setq doc (cond (doc) ( (vla-get-ActiveDocument (vlax-get-acad-object)))))

  (vlax-for ts (vla-get-TextStyles doc)
    (setq tLst (cons (vla-get-Name ts) tLst)))

  (if  (= (setq oHgt (vla-get-Height Obj))
          (vla-get-Height
            (vla-item
              (vla-get-TextStyles doc)
                (vla-get-StyleName obj))))
    
    (setq tog "1")
    (setq tog "0"))

  (cond  (  (not (dcl_write dc_fname))

            (princ "\n** DCL File could not be Written **"))

         (  (<= (setq dcTag (load_dialog dc_fname)) 0)

            (princ "\n** Dialog Definition File not Found **"))

         (  (not (new_dialog "dt_curve" dcTag))

            (princ "\n** Dialog could not be Displayed **"))

         (t

            (set_tile "stitle" dc_title)

            (set_tile  "bstyl" tog)
            (set_tile  "hgt" (vl-princ-to-string oHgt))
            (mode_tile "hgt" (atoi (get_tile "bstyl")))

            (start_list "styl")
            (mapcar 'add_list (setq tLst (acad_strlsort
                                           (vl-remove-if
                                             (function
                                               (lambda (x) (or (null x) (eq "" x)))) tLst))))
            (end_list)

            (set_tile "styl" (itoa (vl-position (vla-get-StyleName Obj) tLst)))

            (logo "logo")            

            (action_tile "bstyl"
              (vl-prin1-to-string
                (quote
                  (progn
                    (set_tile "hgt" (vl-princ-to-string
                                      (cond (  (zerop (setq sHgt (vla-get-Height
                                                                   (vla-item
                                                                     (vla-get-TextStyles doc)
                                                                       (nth (atoi (get_tile "styl")) tLst))))) 1.)
                                            (sHgt))))
                    
                    (mode_tile "hgt" (atoi $value))))))

            (action_tile "styl"
              (vl-prin1-to-string
                (quote
                  (progn
                    (set_tile "hgt" (vl-princ-to-string
                                      (cond (  (zerop (setq sHgt (vla-get-Height
                                                                   (vla-item
                                                                     (vla-get-TextStyles doc)
                                                                       (nth (atoi $value) tLst))))) 1.)
                                            (sHgt))))))))
            
            (action_tile "cancel" "(done_dialog)")

            (action_tile "accept"
              (vl-prin1-to-string
                (quote
                  (progn

                    (cond (  (not (setq hgt (txt2num (get_tile "hgt"))))

                             (alert "**  Text Height must be Numerical **"))

                          (t

                             (vla-put-StyleName Obj (setq sty (nth (atoi (get_tile "styl")) tLst)))

                             (or (and (eq "0" (get_tile "bstyl"))
                                      (not (vla-put-Height Obj hgt)))

                                 (vla-put-height Obj
                                   (cond ( (zerop (setq sHgt (vla-get-Height
                                                               (vla-item
                                                                 (vla-get-TextStyles doc) sty)))) 1.)
                                         (sHgt))))                                           
                           
                             (done_dialog)))))))
          
          
            (start_dialog)
            (unload_dialog dcTag))))


;;-------------------------------------------------------------------------------;;
;;                        --=={  DTRemove Function  }==--                        ;;
;;-------------------------------------------------------------------------------;;


;; DTRemove  ~  Function to Remove Text/Object Associativity

(defun c:DTRemove (/  ; --=={  Local Functions  }==--

                        *error* putxdat logo_print

                      ; --=={  Local Variables  }==--

                        uflag doc typ val xDat gr code
                        data ent tag han cHan cObj)
  (vl-load-com)

  (setq doc   (vla-get-ActiveDocument
                (vlax-get-acad-object))

        drft  (vla-get-drafting
                (vla-get-preferences
                  (vlax-get-acad-object)))
        
        logo  (icon_GrList 3 (vla-get-AutoSnapMarkerSize drft)))
  

  ;;-------------------------------------------------------------------------------;;
  

  (defun logo_print (pt / s)
  
    (setq s  (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE")))
          pt (trans pt 1 3))

    (grvecs logo (list (list s 0. 0. (car pt))
                       (list 0. s 0. (cadr pt))
                       (list 0. 0. s 0.)
                       (list 0. 0. 0. 1.))))
  

  ;;-------------------------------------------------------------------------------;;
  

  (defun *error* (msg)

    (if (and uflag doc) (vla-EndUndoMark doc))

    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))

    (redraw)
    (princ))
  

  ;;-------------------------------------------------------------------------------;;
  

  (defun putxdat (Obj App Data / xtype xval)

    (setq xtype
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbInteger '(0 . 1)) '(1001 1000))))

    (setq xval
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-vbVariant '(0 . 1)) (list App Data))))

    (vla-setXData Obj xtype xval))
  

  ;;-------------------------------------------------------------------------------;;
  

  (foreach obj (vlr-owners *DCurve$Align$Reactor*)

             (if (vlax-erased-p obj)
               (vlr-owner-remove *DCurve$Align$Reactor* obj)

               (progn
             
                 (vla-GetXData obj app 'typ 'val)

                 (if (and typ val)
                   (progn

                     (setq xDat (cons (read (cdr (assoc 1000 (apply (function mapcar)
                                                                    (cons (function cons)
                                                                          (list (vlax-safearray->list typ)
                                                                                (mapcar (function vlax-variant-value)
                                                                                        (vlax-safearray->list val)))))))) xDat)))))))

  (setq xDat (apply (function append) xDat))

  (cond (  (not (or (vlr-owners *DCurve$Align$Reactor*) xDat))    

           (princ "\n** No Text/Object Associativity Found **"))
        
        (t        

           (vla-StartUndoMark doc)
           (setq uFlag t)

           (setq msg (princ "\nSelect Text/Object to Remove Associativity, Remove [A]ll" ))
         
           (while
             (progn
               (setq gr (grread 't 15 2) code (car gr) data (cadr gr))
               (redraw)

               (cond (  (and (= 5 code) (listp data))

                        (if (setq ent (car (nentselp data)))
                          (progn

                            (setq hand (cdr (assoc 5 (entget ent))))

                            (and (or (vl-position hand (mapcar (function car)  xDat))
                                     (vl-position hand (mapcar (function cadr) xDat)))
                                 
                                 (logo_print (polar data (+ (/ pi 4.) (getvar "SNAPANG"))
                                                    (* (+ 30. (getvar "PICKBOX")) (/ (getvar "VIEWSIZE")
                                                                                     (cadr (getvar "SCREENSIZE")))))))))
                      
                        t)

                     (  (and (= 3 code) (listp data))

                        (cond (  (setq ent (car (nentselp data)))

                                 (cond (  (vl-position (cdr (assoc 0 (entget ent))) '("MTEXT" "TEXT"))

                                          (if (setq tag (assoc (setq han (cdr (assoc 5 (entget ent)))) xDat))
                                            (progn

                                              (setq cHan (cadr tag) xDat (vl-remove-if
                                                                           (function
                                                                             (lambda (x)
                                                                               (or (eq tag x)
                                                                                   (not (eq cHan (cadr x)))))) xDat))
                                              
                                              (putxDat (vla-HandletoObject doc cHan) app (vl-prin1-to-string xDat))
                                              (princ "\nText/Object Associativity Removed.")

                                              nil)

                                            (princ (strcat "\n** Text not Aligned **" msg))))

                                       (  (not (vl-catch-all-error-p
                                                 (vl-catch-all-apply 'vlax-curve-getEndParam
                                                   (list (setq cObj (vlax-ename->vla-object ent))))))

                                          (vla-GetXData cObj app 'typ 'val)

                                          (if (and typ val)
                                            (progn
                                              
                                              (vlr-owner-remove *DCurve$Align$Reactor* cObj)
                                              (putxDat cObj app "")                                              
                                              (entmod (list (cons -1 ent) (list -3 (list app))))
                                              
                                              (princ "\nAll Text Associativity with this Object Erased.") nil)

                                            (princ (strcat "\n** No Text/Object Associativity Found **" msg))))

                                       (t (princ (strcat "\n** Invalid Object Selection **" msg)))))

                              (t (princ (strcat "\n** Missed, Try Again... **" msg)))))

                     (  (= code 25) nil)

                     (  (= code 2)

                        (cond  (  (vl-position data '(13 32)) nil)

                               (  (vl-position data '(65 97))

                                  (foreach obj (vlr-owners *DCurve$Align$Reactor*)

                                    (vla-GetxData obj app 'typ 'val)

                                    (if (and typ val)
                                      (progn
                                        
                                        (vlr-owner-remove *DCurve$Align$Reactor* obj)
                                        (putxDat obj app "")                                        
                                        (entmod (list (cons -1 (vlax-vla-object->ename obj)) (list -3 (list app)))))))

                                  (princ "\nAll Associativity Removed.") nil)

                               (t )))

                     (t ))))))

  (vla-EndUndoMark doc)
  (redraw)
  
  (princ))


;;-------------------------------------------------------------------------------;;
;;                        --=={  Loading Function  }==--                         ;;
;;-------------------------------------------------------------------------------;;


(vl-load-com)

(if (vl-catch-all-error-p
      (vl-catch-all-apply
        (function
          (lambda (/ unique xdat doc)

            (defun unique (lst / result)
              (reverse
                (while (setq itm (car lst))
                  (setq lst (vl-remove itm lst)
                        result (cons itm result)))))

            (setq *DCurve$Align$Reactor*

                   (cond (  (setq pos (vl-position "DCurve$Align$Reactor"
                                        (mapcar 'vlr-data (cdar (vlr-reactors :VLR-Object-Reactor)))))
                          
                            (nth pos (cdar (vlr-reactors :VLR-Object-Reactor))))

                         (t (setq *DCurve$Align$Reactor* (vlr-object-reactor nil "DCurve$Align$Reactor"
                                                           (list
                                                             (cons :vlr-modified 'DTCurveUpd)))))))

            (setq *DCurve$Align$Reactor$Comm*

                   (cond (  (setq pos (vl-position "DCurve$Align$Reactor$Comm"
                                        (mapcar 'vlr-data (cdar (vlr-reactors :VLR-Command-Reactor)))))
                          
                            (nth pos (cdar (vlr-reactors :VLR-Command-Reactor))))

                         (t (setq *DCurve$Align$Reactor$Comm* (vlr-command-reactor "DCurve$Align$Reactor$Comm"
                                                                (list
                                                                  (cons :vlr-CommandWillStart 'DTCurveComm)))))))
            
            (mapcar
              (function
                (lambda (object) (vlr-owner-remove *DCurve$Align$Reactor* object)))
              
              (vlr-owners *DCurve$Align$Reactor*))

            (vlax-for lay (vla-get-layouts
                            (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))))

              (vlax-for obj (vla-get-Block lay)

                (vla-getXdata obj app 'typ 'val)
                
                (if val
                  (setq xdat
                    (cons
                      (vlax-variant-value
                        (cadr
                          (vlax-safearray->list val))) xdat)))

                (setq typ nil val nil)))

            (foreach handle (unique
                              (mapcar (function cadr)
                                      (apply (function append)
                                             (mapcar (function read) (vl-remove 'nil xdat)))))

              (vlr-owner-add *DCurve$Align$Reactor* (vla-HandletoObject doc handle)))))))

  (princ "\n** Error Loading DTCurve **")

;;-------------------------------------------------------------------------------;;

  (progn
    (princ "\n:: DTCurve.lsp | Version 2.9 | © Lee Mac 2009 www.lee-mac.com ::")
    (princ "\n:: Type \"DTCurve\" to Invoke ::")
  )
)
(princ)
 
;;-------------------------------------------------------------------------------;;
;;                                 End of File                                   ;;
;;-------------------------------------------------------------------------------;;


;;---------------------=={ Polyline Information }==---------------------;;
;;                                                                      ;;
;;  This program provides the user with detailed information about      ;;
;;  every segment of a selected polyline in the form of either an       ;;
;;  AutoCAD Table (if available), Text file, or CSV file.               ;;
;;                                                                      ;;
;;  Upon calling the program with the command syntax 'polyinfo' at the  ;;
;;  AutoCAD command-line, the user is prompted to select an LWPolyline  ;;
;;  to be queried from the active drawing. At this prompt the user      ;;
;;  also has the option to choose the form of output for the            ;;
;;  information harvested by the program; this output format will be    ;;
;;  remembered between drawing sessions to enable streamlined repeated  ;;
;;  program usage.                                                      ;;
;;                                                                      ;;
;;  The program will output LWPolyline segment data to either an        ;;
;;  AutoCAD Table Object created in the active drawing (if such object  ;;
;;  is available in the version of AutoCAD in which the program is      ;;
;;  being executed), or a tab-delimited Text file or CSV file           ;;
;;  automatically created (streamlining the program to minimise         ;;
;;  prompts) in the working directory of the active drawing.            ;;
;;                                                                      ;;
;;  For every segment of the selected LWPolyline, the program will      ;;
;;  extract the following information:                                  ;;
;;                                                                      ;;
;;      • Segment Number                                                ;;
;;      • Segment Start Vertex Coordinate                               ;;
;;      • Segment End Vertex Coordinate                                 ;;
;;      • Segment Start Width                                           ;;
;;      • Segment End Width                                             ;;
;;      • Segment Length                                                ;;
;;      • Arc Centre (if arc segment)                                   ;;
;;      • Arc Radius (if arc segment)                                   ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-10                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2012-07-16                                      ;;
;;                                                                      ;;
;;  - Added Table & Text file output options.                           ;;
;;  - Removed basic LWPolyline properties.                              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2014-06-14                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing final segment to be omitted from output data    ;;
;;    when processing closed polylines.                                 ;;
;;----------------------------------------------------------------------;;
;;  Version 1.3    -    2015-04-13                                      ;;
;;                                                                      ;;
;;  - Fixed bug causing the program to crash when processing polylines  ;;
;;    containing arc segments.                                          ;;
;;----------------------------------------------------------------------;;

(defun c:polyinfo ( / *error* ent enx flg ins lst out seg tmp )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (= 'str (type out)) (setenv "LMac\\PolyInfo" out))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
   
    (if (null (setq out (getenv "LMac\\PolyInfo")))
        (setq out "TXT")
    )
    (princ
        (strcat "\nOutput Format: "
            (cond
                (   (= out "TXT") "Text File")
                (   (= out "CSV") "CSV File")
                (   "AutoCAD Table"   )
            )
        )
    )

    (while
        (progn
            (setvar 'errno 0)
            (initget "Output")
            (setq ent (entsel "\nSelect polyline [Output]: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent)
                    nil
                )
                (   (= "Output" ent)
                    (polyinfo:chooseoutput  'out)
                    (setenv "LMac\\PolyInfo" out)
                    (princ
                        (strcat "\nOutput Format: "
                            (cond
                                (   (= out "TXT") "Text File")
                                (   (= out "CSV") "CSV File")
                                (   "AutoCAD Table"   )
                            )
                        )
                    )
                )
                (   (/= "LWPOLYLINE" (cdr (assoc 0 (entget (setq ent (car ent))))))
                    (princ "\nSelected object is not an LWPolyline.")
                )
            )
        )
    )
    (cond
        (   (and
                (= 'ename (type ent))
                (= "Table" out)
                (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
            )
            (princ "\nCurrent layer locked.")
        )
        (   (= 'ename (type ent))
            (setq seg 0
                  enx (entget ent)
                  lst (LM:lwvertices enx)
                  lst
                (cons
                    (append '("SEG." "START X" "START Y" "END X" "END Y" "WIDTH 1" "WIDTH 2" "LENGTH")
                        (if (setq flg (vl-some '(lambda ( x ) (not (zerop (cdr (assoc 42 x))))) lst))
                           '("CENTRE X" "CENTRE Y" "RADIUS")
                        )
                    )
                    (mapcar
                        (function
                            (lambda ( l1 l2 / b p q )
                                (setq p (cdr (assoc 10 l1))
                                      q (cdr (assoc 10 l2))
                                      b (cdr (assoc 42 l1))
                                )
                                (append
                                    (list (itoa (setq seg (1+ seg))))
                                    (mapcar 'rtos p)
                                    (mapcar 'rtos q)
                                    (list
                                        (rtos (cdr (assoc 40 l1)))
                                        (rtos (cdr (assoc 41 l1)))
                                    )
                                    (if (zerop b)
                                        (cons (rtos (distance p q)) (if flg '("" "" "")))
                                        (append
                                            (list (rtos (abs (* (LM:bulgeradius p q b) (atan b) 4))))
                                            (mapcar 'rtos (LM:bulgecentre p q b))
                                            (list (rtos (LM:bulgeradius p q b)))
                                        )
                                    )
                                )
                            )
                        )
                        lst
                        (if (= 1 (logand 1 (cdr (assoc 70 enx))))
                            (append (cdr lst) (list (car lst)))
                            (cdr lst)
                        )
                    )
                )
            )
            (cond
                (   (= out "TXT")
                    (if (LM:writetxt lst (setq tmp (vl-filename-mktemp (cdr (assoc 5 enx)) (getvar 'dwgprefix) ".txt")))
                        (startapp "explorer" tmp)
                    )
                )
                (   (= out "CSV")
                    (if (LM:writecsv lst (setq tmp (vl-filename-mktemp (cdr (assoc 5 enx)) (getvar 'dwgprefix) ".csv")))
                        (startapp "explorer" tmp)
                    )
                )
                (   (setq ins (getpoint "\nSpecify point for table: "))
                    (LM:startundo (LM:acdoc))
                    (LM:addtable  (vlax-get-property (LM:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace)) (trans ins 1 0) nil lst nil)
                    (LM:endundo   (LM:acdoc))
                )
            )
        )
    )
    (princ)
)

;; Add Table  -  Lee Mac
;; Generates a table at the given point, populated with the given data and optional title.
;; spc - [vla] VLA Block object
;; ins - [lst] WCS insertion point for table
;; ttl - [str] [Optional] Table title
;; lst - [lst] Matrix list of table cell data
;; eqc - [bol] If T, columns are of equal width
;; Returns: [vla] VLA Table Object

(defun LM:addtable ( spc ins ttl lst eqc / dif hgt i j obj stn sty wid )
    (setq sty
        (vlax-ename->vla-object
            (cdr
                (assoc -1
                    (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "acad_tablestyle")))
                        (getvar 'ctablestyle)
                    )
                )
            )
        )
    )
    (setq hgt (vla-gettextheight sty acdatarow))
    (if (LM:annotative-p (setq stn (vla-gettextstyle sty acdatarow)))
        (setq hgt (/ hgt (cond ((getvar 'cannoscalevalue)) (1.0))))
    )
    (setq wid
        (mapcar
           '(lambda ( col )
                (apply 'max
                    (mapcar
                       '(lambda ( str )
                            (   (lambda ( box ) (if box (+ (* 2.5 hgt) (- (caadr box) (caar box))) 0.0))
                                (textbox
                                    (list
                                        (cons 01 str)
                                        (cons 40 hgt)
                                        (cons 07 stn)
                                    )
                                )
                            )
                        )
                        col
                    )
                )
            )
            (apply 'mapcar (cons 'list lst))
        )
    )
    (if 
        (and ttl
            (< 0.0
                (setq dif
                    (/
                        (-
                            (   (lambda ( box ) (if box (+ (* 2.5 hgt) (- (caadr box) (caar box))) 0.0))
                                (textbox
                                    (list
                                        (cons 01 ttl)
                                        (cons 40 hgt)
                                        (cons 07 stn)
                                    )
                                )
                            )
                            (apply '+ wid)
                        )
                        (length wid)
                    )
                )
            )
        )
        (setq wid (mapcar '(lambda ( x ) (+ x dif)) wid))
    )
    (setq obj
        (vla-addtable spc
            (vlax-3D-point ins)
            (1+ (length lst))
            (length (car lst))
            (* 2.0 hgt)
            (if eqc
                (apply 'max wid)
                (/ (apply '+ wid) (float (length (car lst))))
            )
        )
    )
    (vla-put-regeneratetablesuppressed obj :vlax-true)
    (vla-put-stylename obj (getvar 'ctablestyle))
    (setq i -1)
    (if (null eqc)
        (foreach col wid
            (vla-setcolumnwidth obj (setq i (1+ i)) col)
        )
    )
    (if ttl
        (progn
            (vla-settext obj 0 0 ttl)
            (setq i 1)
        )
        (progn
            (vla-deleterows obj 0 1)
            (setq i 0)
        )
    )
    (foreach row lst
        (setq j 0)
        (foreach val row
            (vla-settext obj i j val)
            (setq j (1+ j))
        )
        (setq i (1+ i))
    )
    (vla-put-regeneratetablesuppressed obj :vlax-false)
    obj
)

;; Write CSV  -  Lee Mac
;; Writes a matrix list of cell values to a CSV file.
;; lst - [lst] list of lists, sublist is row of cell values
;; csv - [str] filename of CSV file to write
;; Returns T if successful, else nil

(defun LM:writecsv ( lst csv / des sep )
    (if (setq des (open csv "w"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
            (foreach row lst (write-line (LM:lst->csv row sep) des))
            (close des)
            t
        )
    )
)

;; List -> CSV  -  Lee Mac
;; Concatenates a row of cell values to be written to a CSV file.
;; lst - [lst] list containing row of CSV cell values
;; sep - [str] CSV separator token

(defun LM:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat (LM:csv-addquotes (car lst) sep) sep (LM:lst->csv (cdr lst) sep))
        (LM:csv-addquotes (car lst) sep)
    )
)

(defun LM:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)    
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)

;; Write Text File  -  Lee Mac
;; Writes a matrix of values to a tab-delimited Text file.
;; lst - [lst] list of lists, sublist is line of text values
;; txt - [str] filename of Text file to write
;; Returns T if successful, else nil

(defun LM:writetxt ( lst txt / des )
    (if (setq des (open txt "w"))
        (progn
            (foreach itm lst (write-line (LM:lst->str itm "\t") des))
            (close des)
            t
        )
    )
)

;; List to String  -  Lee Mac
;; Concatenates each string in a supplied list, separated by a given delimiter
;; lst - [lst] List of strings to concatenate
;; del - [str] Delimiter string to separate each item

(defun LM:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (LM:lst->str (cdr lst) del))
        (car lst)
    )
)

;; Annotative-p  -  Lee Mac
;; Predicate function to determine whether a Text Style is annotative.
;; sty - [str] Name of Text Style

(defun LM:annotative-p ( sty )
    (and (setq sty (tblobjname "style" sty))
         (setq sty (cadr (assoc -3 (entget sty '("AcadAnnotative")))))
         (= 1 (cdr (assoc 1070 (reverse sty))))
    )
)

;; LW Vertices  -  Lee Mac
;; Returns a list of lists in which each sublist describes
;; the position, starting width, ending width and bulge of the
;; vertex of a supplied LWPolyline

(defun LM:lwvertices ( e )
    (if (setq e (member (assoc 10 e) e))
        (cons
            (list
                (assoc 10 e)
                (assoc 40 e)
                (assoc 41 e)
                (assoc 42 e)
            )
            (LM:lwvertices (cdr e))
        )
    )
)

;; Bulge Radius  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the radius of the arc described by the given bulge and vertices

(defun LM:bulgeradius ( p1 p2 b )
    (/ (* (distance p1 p2) (1+ (* b b))) 4 (abs b))
)

;; Bulge Centre  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the centre of the arc described by the given bulge and vertices

(defun LM:bulgecentre ( p1 p2 b )
    (polar p1
        (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
        (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
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

(vl-load-com)
(eval
    (append
        (list 'defun 'polyinfo:chooseoutput '( sym ))
        (if (vlax-method-applicable-p (vla-get-modelspace (LM:acdoc)) 'addtable)
            (list
               '(initget "Table TXT CSV")
               '(set sym (cond ((getkword (strcat "\nChoose Output [Table/TXT/CSV] <" (eval sym) ">: "))) ((eval sym))))
            )
            (list
               '(initget "TXT CSV")
               '(set sym (cond ((getkword (strcat "\nChoose Output [TXT/CSV] <" (eval sym) ">: "))) ((eval sym))))
            )
        )
    )
)

(princ
    (strcat
        "\n:: PolyInfo.lsp | Version 1.3 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"polyinfo\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;

;;-----------------------=={ Circle Tangents  }==-----------------------;;
;;                                                                      ;;
;;  This program allows the user to dynamically construct two circles   ;;
;;  connected with a pair of lines meeting the circumference of each    ;;
;;  circle at a tangent, resulting in a belt or cam shape.              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'ctan' at the AutoCAD               ;;
;;  command-line, the program will issue four successive prompts: the   ;;
;;  user is prompted to specify the center of the first circle, the     ;;
;;  radius of the first circle, followed by the center & radius of      ;;
;;  the second circle.                                                  ;;
;;                                                                      ;;
;;  During each of these prompts, the circles and adjoining lines are   ;;
;;  displayed dynamically in real-time relative to the position of the  ;;
;;  AutoCAD cursor.                                                     ;;
;;                                                                      ;;
;;  Following valid responses to all prompts, the program will          ;;
;;  construct the resulting shape using a 2D polyline (LWPolyline).     ;;
;;                                                                      ;;
;;  However, if the radius of the second circle is greater than the     ;;
;;  combination of the distance between the circle centers & radius of  ;;
;;  the first circle, the program will instead construct a circle       ;;
;;  centered at the second given center, with radius equal to this      ;;
;;  maximum limit.                                                      ;;
;;                                                                      ;;
;;  Similarly, if the distance between the two circle centers is less   ;;
;;  than the radius of the first circle, the program will construct     ;;
;;  only the first circle.                                              ;;
;;                                                                      ;;
;;  Although the dynamic visual effect is dependent on heavy use of     ;;
;;  the AutoLISP grread function, this program utilises my GrSnap       ;;
;;  utility to enable full Object Snap functionality during the         ;;
;;  dynamic prompts. The latest version and full documentation for      ;;
;;  this utility may be found at: http://www.lee-mac.com/grsnap.html    ;;
;;                                                                      ;;
;;  Finally, this program has been designed to perform successfully     ;;
;;  under all UCS & View settings.                                      ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-08-25                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:ctan ( / *error* grcircle grarc grgetpoint an1 an2 cn1 cn2 di1 di2 ocs rd1 rd2 tmp )

    (setq ctan:res 40 ;; arc resolution (int > 0)
          ctan:2pi (+ pi pi)
          ctan:inc (/ ctan:2pi ctan:res)
    )
    
    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (redraw) (princ)
    )

    (defun grcircle ( cen rad / ang )
        (setq ang 0.0)
        (repeat ctan:res
            (grdraw (polar cen ang rad) (polar cen (setq ang (+ ang ctan:inc)) rad) 1)
        )
    )

    (defun grarc ( cen pt1 pt2 / ang rad )
        (setq ang (angle cen pt1)
              rad (distance cen pt1)
        )
        (repeat (fix (/ (rem (+ (- (angle cen pt2) ang) ctan:2pi) ctan:2pi) ctan:inc))
            (grdraw pt1 (setq pt1 (polar cen (setq ang (+ ang ctan:inc)) rad)) 1)
        )
        (grdraw pt1 pt2 1)
    )

    (defun grgetpoint ( msg bpt flg fun / gr1 gr2 osf osm rtn str tmp )
        (setq osf (LM:grsnap:snapfunction)
              osm (getvar 'osmode)
              fun (eval fun)
              str ""
        )
        (princ msg)
        (while
            (progn
                (setq gr1 (grread t 15 0)
                      gr2 (cadr gr1)
                      gr1 (car  gr1)
                )
                (cond
                    (   (= 5 gr1) (redraw)
                        (osf gr2 osm)
                        (fun gr2)
                        t
                    )
                    (   (= 3 gr1) nil)
                    (   (= 2 gr1)
                        (cond
                            (   (= 6 gr2)
                                (if (zerop (logand 16384 (setq osm (setvar 'osmode (boole 6 16384 (getvar 'osmode))))))
                                    (princ "\n<Osnap on>")
                                    (princ "\n<Osnap off>")
                                )
                                (princ msg)
                            )
                            (   (= 8 gr2)
                                (if (< 0 (strlen str))
                                    (progn
                                        (princ "\010\040\010")
                                        (setq str (substr str 1 (1- (strlen str))))
                                    )
                                )
                                t
                            )
                            (   (< 32 gr2 127)
                                (setq str (strcat str (princ (chr gr2))))
                            )
                            (   (member gr2 '(13 32))
                                (cond
                                    (   (= "" str) nil)
                                    (   (setq gr2 (LM:grsnap:parsepoint bpt str))
                                        (setq osm 16384)
                                        nil
                                    )
                                    (   (setq tmp (LM:grsnap:snapmode str))
                                        (setq osm tmp
                                              str ""
                                        )
                                    )
                                    (   (and  flg (distof str))
                                        (setq gr2 (mapcar '+ bpt (list (distof str) 0.0 0.0))
                                              osm 16384
                                        )
                                        nil
                                    )
                                    (   (setq str "")
                                        (princ (strcat "\n2D / 3D Point Required." msg))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        (if (listp gr2) (osf gr2 osm))
    )

    (LM:startundo (LM:acdoc))
    (if (setq cn1 (getpoint "\nSpecify center of 1st circle: "))
        (progn
            (while
                (and
                    (setq tmp
                        (grgetpoint "\nSpecify 1st radius: " cn1 t
                            (function
                                (lambda ( gr2 )
                                    (grcircle cn1 (distance cn1 gr2))
                                )
                            )
                        )
                    )
                    (equal 0.0 (setq rd1 (distance cn1 tmp)) 1e-8)
                )
                (princ "\nRadius cannot be zero.")
            )
            (if
                (and tmp
                    (setq cn2
                        (grgetpoint "\nSpecify center of 2nd circle: " cn1 nil
                            (function
                                (lambda ( gr2 / an1 an2 di1 pt1 pt2 )
                                    (if (< rd1 (setq di1 (distance cn1 gr2)))
                                        (progn
                                            (setq an1 (angle cn1 gr2)
                                                  an2 (atan (sqrt (- (* di1 di1) (* rd1 rd1))) rd1)
                                                  pt1 (polar cn1 (+ an1 an2) rd1)
                                                  pt2 (polar cn1 (- an1 an2) rd1)
                                            )
                                            (grarc  cn1 pt1 pt2)
                                            (grdraw gr2 pt1 1)
                                            (grdraw gr2 pt2 1)
                                        )
                                        (grcircle cn1 rd1)
                                    )
                                )
                            )
                        )
                    )
                    (setq di1 (distance cn1 cn2)
                          an1 (angle cn1 cn2)
                          ocs (trans '(0.0 0.0 1.0) 1 0 t)
                    )
                )
                (if (< rd1 di1)
                    (if
                        (setq tmp
                            (grgetpoint "\nSpecify 2nd radius: " cn2 t
                                (function
                                    (lambda ( gr2 / an2 pt1 pt2 pt3 pt4 )
                                        (if (< (abs (setq di2 (- rd1 (setq rd2 (distance cn2 gr2))))) di1)
                                            (progn
                                                (setq an2 (atan (sqrt (- (* di1 di1) (* di2 di2))) di2)
                                                      pt1 (polar cn1 (+ an1 an2) rd1)
                                                      pt2 (polar cn1 (- an1 an2) rd1)
                                                      pt3 (polar cn2 (- an1 an2) rd2)
                                                      pt4 (polar cn2 (+ an1 an2) rd2)
                                                )
                                                (grarc  cn1 pt1 pt2)
                                                (grarc  cn2 pt3 pt4)
                                                (grdraw pt1 pt4 1)
                                                (grdraw pt2 pt3 1)
                                            )
                                            (grcircle cn2 (+ di1 rd1))
                                        ) 
                                    )
                                )
                            )
                        )
                        (if (< (abs (setq di2 (- rd1 (setq rd2 (distance cn2 tmp))))) di1)
                            (progn
                                (setq an2 (atan (sqrt (- (* di1 di1) (* di2 di2))) di2))
                                (entmake
                                    (list
                                       '(000 . "LWPOLYLINE")
                                       '(100 . "AcDbEntity")
                                       '(100 . "AcDbPolyline")
                                       '(090 . 40)
                                       '(070 . 01)
                                        (cons 010 (trans (polar cn1 (+ an1 an2) rd1) 1 ocs))
                                        (cons 042 (/ (sin (/ (- pi an2) 2.0)) (cos (/ (- pi an2) 2.0))))
                                        (cons 010 (trans (polar cn1 (- an1 an2) rd1) 1 ocs))
                                        (cons 010 (trans (polar cn2 (- an1 an2) rd2) 1 ocs))
                                        (cons 042 (/ (sin (/ an2 2.0)) (cos (/ an2 2.0))))
                                        (cons 010 (trans (polar cn2 (+ an1 an2) rd2) 1 ocs))
                                        (cons 210 ocs)
                                    )
                                )
                            )
                            (entmake
                                (list
                                   '(000 . "CIRCLE")
                                    (cons 010 (trans cn2 1 ocs))
                                    (cons 040 (+ di1 rd1))
                                    (cons 210 ocs)
                                )
                            )
                        )
                    )
                    (entmake
                        (list
                           '(000 . "CIRCLE")
                            (cons 010 (trans cn1 1 ocs))
                            (cons 040 rd1)
                            (cons 210 ocs)
                        )
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
)

;; Object Snap for grread: Snap Function  -  Lee Mac
;; Returns: [fun] A function requiring two arguments:
;; p - [lst] UCS Point to be snapped
;; o - [int] Object Snap bit code
;; The returned function returns either the snapped point (displaying an appropriate snap symbol)
;; or the supplied point if the snap failed for the given Object Snap bit code.

(defun LM:grsnap:snapfunction ( )
    (eval
        (list 'lambda '( p o / q )
            (list 'if '(zerop (logand 16384 o))
                (list 'if
                   '(setq q
                        (cdar
                            (vl-sort
                                (vl-remove-if 'null
                                    (mapcar
                                        (function
                                            (lambda ( a / b )
                                                (if (and (= (car a) (logand (car a) o)) (setq b (osnap p (cdr a))))
                                                    (list (distance p b) b (car a))
                                                )
                                            )
                                        )
                                       '(
                                            (0001 . "_end")
                                            (0002 . "_mid")
                                            (0004 . "_cen")
                                            (0008 . "_nod")
                                            (0016 . "_qua")
                                            (0032 . "_int")
                                            (0064 . "_ins")
                                            (0128 . "_per")
                                            (0256 . "_tan")
                                            (0512 . "_nea")
                                            (2048 . "_app")
                                            (8192 . "_par")
                                        )
                                    )
                                )
                               '(lambda ( a b ) (< (car a) (car b)))
                            )
                        )
                    )
                    (list 'LM:grsnap:displaysnap '(car q)
                        (list 'cdr
                            (list 'assoc '(cadr q)
                                (list 'quote
                                    (LM:grsnap:snapsymbols
                                        (atoi (cond ((getenv "AutoSnapSize")) ("5")))
                                    )
                                )
                            )
                        )
                        (LM:OLE->ACI
                            (if (= 1 (getvar 'cvport))
                                (atoi (cond ((getenv "Layout AutoSnap Color")) ("117761")))
                                (atoi (cond ((getenv  "Model AutoSnap Color")) ("104193")))
                            )
                        )
                    )
                )
            )
           '(cond ((car q)) (p))
        )
    )
)

;; Object Snap for grread: Display Snap  -  Lee Mac
;; pnt - [lst] UCS point at which to display the symbol
;; lst - [lst] grvecs vector list
;; col - [int] ACI colour for displayed symbol
;; Returns nil

(defun LM:grsnap:displaysnap ( pnt lst col / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col lst)
        (list
            (list scl 0.0 0.0 (car  pnt))
            (list 0.0 scl 0.0 (cadr pnt))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

;; Object Snap for grread: Snap Symbols  -  Lee Mac
;; p - [int] Size of snap symbol in pixels
;; Returns: [lst] List of vector lists describing each Object Snap symbol

(defun LM:grsnap:snapsymbols ( p / -p -q -r a c i l q r )
    (setq -p (- p) q (1+  p)
          -q (- q) r (+ 2 p)
          -r (- r) i (/ pi 6.0)
           a 0.0
    )
    (repeat 12
        (setq l (cons (list (* r (cos a)) (* r (sin a))) l)
              a (- a i)
        )
    )
    (setq c (apply 'append (mapcar 'list (cons (last l) l) l)))
    (list
        (list 1
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 2
            (list -r -q) (list 0  r) (list 0  r) (list r -q)
            (list -p -p) (list p -p) (list p -p) (list 0  p) (list 0  p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list 0  q) (list 0  q) (list -q -q)
        )
        (cons 4 c)
        (vl-list* 8 (list -r -r) (list r r) (list r -r) (list -r r) c)
        (list 16
            (list p 0) (list 0 p) (list 0 p) (list -p 0) (list -p 0) (list 0 -p) (list 0 -p) (list p 0)
            (list q 0) (list 0 q) (list 0 q) (list -q 0) (list -q 0) (list 0 -q) (list 0 -q) (list q 0)
            (list r 0) (list 0 r) (list 0 r) (list -r 0) (list -r 0) (list 0 -r) (list 0 -r) (list r 0)
        )
        (list 32
            (list  r r) (list -r -r) (list  r q) (list -q -r) (list  q r) (list -r -q)
            (list -r r) (list  r -r) (list -q r) (list  r -q) (list -r q) (list  q -r)
        )
        (list 64
            '( 0  1) (list  0  p) (list  0  p) (list -p  p) (list -p  p) (list -p -1) (list -p -1) '( 0 -1)
            '( 0 -1) (list  0 -p) (list  0 -p) (list  p -p) (list  p -p) (list  p  1) (list  p  1) '( 0  1)
            '( 1  2) (list  1  q) (list  1  q) (list -q  q) (list -q  q) (list -q -2) (list -q -2) '(-1 -2)
            '(-1 -2) (list -1 -q) (list -1 -q) (list  q -q) (list  q -q) (list  q  2) (list  q  2) '( 1  2)
        )
        (list 128
            (list (1+ -p) 0) '(0 0) '(0 0) (list 0 (1+ -p))
            (list (1+ -p) 1) '(1 1) '(1 1) (list 1 (1+ -p))
            (list -p q) (list -p -p) (list -p -p) (list q -p)
            (list -q q) (list -q -q) (list -q -q) (list q -q)
        )
        (vl-list* 256 (list -r r)  (list r r) (list -r (1+ r)) (list r (1+ r)) c)
        (list 512
            (list -p -p) (list  p -p) (list -p  p) (list p p) (list -q -q) (list  q -q)
            (list  q -q) (list -q  q) (list -q  q) (list q q) (list  q  q) (list -q -q)
        )
        (list 2048
            (list   -p     -p) (list    p      p) (list   -p      p) (list    p     -p)
            (list (+ p 05) -p) (list (+ p 06) -p) (list (+ p 05) -q) (list (+ p 06) -q)
            (list (+ p 09) -p) (list (+ p 10) -p) (list (+ p 09) -q) (list (+ p 10) -q)
            (list (+ p 13) -p) (list (+ p 14) -p) (list (+ p 13) -q) (list (+ p 14) -q)
            (list -p -p) (list p -p) (list p -p) (list p p) (list p p) (list -p p) (list -p p) (list -p -p)
            (list -q -q) (list q -q) (list q -q) (list q q) (list q q) (list -q q) (list -q q) (list -q -q)
        )
        (list 8192 (list r 1) (list -r -q) (list r 0) (list -r -r) (list r q) (list -r -1) (list r r) (list -r 0))
    )
)

;; Object Snap for grread: Parse Point  -  Lee Mac
;; bpt - [lst] Basepoint for relative point input, e.g. @5,5
;; str - [str] String representing point input
;; Returns: [lst] Point represented by the given string, else nil

(defun LM:grsnap:parsepoint ( bpt str / str->lst lst )
 
    (defun str->lst ( str / pos )
        (if (setq pos (vl-string-position 44 str))
            (cons (substr str 1 pos) (str->lst (substr str (+ pos 2))))
            (list str)
        )
    )

    (if (wcmatch str "`@*")
        (setq str (substr str 2))
        (setq bpt '(0.0 0.0 0.0))
    )           

    (if
        (and
            (setq lst (mapcar 'distof (str->lst str)))
            (vl-every 'numberp lst)
            (< 1 (length lst) 4)
        )
        (mapcar '+ bpt lst)
    )
)

;; Object Snap for grread: Snap Mode  -  Lee Mac
;; str - [str] Object Snap modifier
;; Returns: [int] Object Snap bit code for the given modifier, else nil

(defun LM:grsnap:snapmode ( str )
    (vl-some
        (function
            (lambda ( x )
                (if (wcmatch (car x) (strcat (strcase str t) "*"))
                    (progn
                        (princ (cadr x)) (caddr x)
                    )
                )
            )
        )
       '(
            ("endpoint"      " of " 00001)
            ("midpoint"      " of " 00002)
            ("center"        " of " 00004)
            ("node"          " of " 00008)
            ("quadrant"      " of " 00016)
            ("intersection"  " of " 00032)
            ("insert"        " of " 00064)
            ("perpendicular" " to " 00128)
            ("tangent"       " to " 00256)
            ("nearest"       " to " 00512)
            ("appint"        " of " 02048)
            ("parallel"      " to " 08192)
            ("none"          ""     16384)
        )
    )
)

;; OLE -> ACI  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->ACI ( c )
    (apply 'LM:RGB->ACI (LM:OLE->RGB c))
)

;; OLE -> RGB  -  Lee Mac
;; Args: c - [int] OLE Colour

(defun LM:OLE->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(24 16 8))
)

;; RGB -> ACI  -  Lee Mac
;; Args: r,g,b - [int] Red, Green, Blue values

(defun LM:RGB->ACI ( r g b / c o )
    (if (setq o (vla-getinterfaceobject (LM:acapp) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2))))
        (progn
            (setq c (vl-catch-all-apply '(lambda ( ) (vla-setrgb o r g b) (vla-get-colorindex o))))
            (vlax-release-object o)
            (if (vl-catch-all-error-p c)
                (prompt (strcat "\nError: " (vl-catch-all-error-message c)))
                c
            )
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

;; Application Object  -  Lee Mac
;; Returns the VLA Application Object

(defun LM:acapp nil
    (eval (list 'defun 'LM:acapp 'nil (vlax-get-acad-object)))
    (LM:acapp)
)

(vl-load-com)
(princ
    (strcat
        "\n:: CircleTangents.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"ctan\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;