;; Single Strikethrough - Single
(defun c:strike ( )
    (LM:strikethrough:single
       '(
            (0.0 0.1)
        )
    )
)

;; Single Strikethrough - Multiple
(defun c:mstrike ( )
    (LM:strikethrough:selection
       '(
            (0.0 0.1)
        )
    )
)

;; Double Strikethrough - Single
(defun c:strike2 ( )
    (LM:strikethrough:single
       '(
            ( 0.15 0.1)
            (-0.15 0.1)
        )
    )
)

;; Double Strikethrough - Multiple
(defun c:mstrike2 ( )
    (LM:strikethrough:selection
       '(
            ( 0.15 0.1)
            (-0.15 0.1)
        )
    )
)

;; Triple Strikethrough - Single
(defun c:strike3 ( )
    (LM:strikethrough:single 
       '(
            ( 0.2 0.1)
            ( 0.0 0.1)
            (-0.2 0.1)
        )
    )
)

;; Triple Strikethrough - Multiple
(defun c:mstrike3 ( )
    (LM:strikethrough:selection 
       '(
            ( 0.2 0.1)
            ( 0.0 0.1)
            (-0.2 0.1)
        )
    )
)

;; Underline - Single
(defun c:under ( )
    (LM:strikethrough:single 
       '(
            (-0.8 0.1)
        )
    )
)

;; Underline - Multiple
(defun c:munder ( )
    (LM:strikethrough:selection 
       '(
            (-0.8 0.1)
        )
    )
)

;; Double Underline - Single
(defun c:under2 ( )
    (LM:strikethrough:single 
       '(
            (-0.8  0.05)
            (-1.0  0.05)
        )
    )
)

;; Double Underline - Multiple
(defun c:munder2 ( )
    (LM:strikethrough:selection 
       '(
            (-0.8  0.05)
            (-1.0  0.05)
        )
    )
)

;; Double Overline & Underline - Single
(defun c:overunder2 ( )
    (LM:strikethrough:single 
       '(
            ( 1.0 0.05)
            ( 0.8 0.05)
            (-0.8 0.05)
            (-1.0 0.05)
        )
    )
)

;; Double Overline & Underline - Multiple
(defun c:moverunder2 ( )
    (LM:strikethrough:selection 
       '(
            ( 1.0 0.05)
            ( 0.8 0.05)
            (-0.8 0.05)
            (-1.0 0.05)
        )
    )
)

;; Strikthrough Text: Single Selection  -  Lee Mac
;; Prompts the user to select a single text, mtext or attribute object and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:single ( l / *error* e )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (while
        (progn
            (setvar 'errno 0)
            (setq e (nentselp "\nSelect text, mtext or attribute: "))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null e) nil)
                (   (and (= 2 (length e))
                         (wcmatch (cdr (assoc 0 (entget (car e)))) "TEXT,MTEXT,ATTRIB")
                    )
                    (LM:strikethrough (car e) l)
                )
                (   (princ "\nInvalid object selected."))
            )
        )
    )
    (princ)
)

;; Strikthrough Text: Multiple Selection  -  Lee Mac
;; Prompts the user for a selection of text, mtext or attributed blocks and evaluates the
;; strikethrough function with the supplied parameter list.
;; l - [lst] List of ((<Spacing Factor> <Width Factor>) ... )

(defun LM:strikethrough:selection ( l / *error* a e i s x )

    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (if (setq s (ssget '((-4 . "<OR") (0 . "TEXT,MTEXT") (-4 . "<AND") (0 . "INSERT") (66 . 1) (-4 . "AND>") (-4 . "OR>"))))
        (repeat (setq i (sslength s))
            (setq i (1- i)
                  e (ssname s i)
            )
            (if (= "INSERT" (cdr (assoc 0 (entget e))))
                (progn
                    (setq a (entnext e)
                          x (entget  a)
                    )
                    (while (= "ATTRIB" (cdr (assoc 0 x)))
                        (LM:strikethrough a l)
                        (setq a (entnext a)
                              x (entget  a)
                        )
                    )
                )
                (LM:strikethrough e l)
            )
        )
    )
    (princ)
)

;; Strikethrough Text  -  Lee Mac
;; Generates polylines through the supplied text object, with spacing & width given by the supplied parameter list.
;; ent - [ent] Text or MText entity
;; par - [lst] List of ((<Spacing Factor> <Width Factor>) ... ) for each polyline
;; Returns: [lst] List of created polyline entities

(defun LM:strikethrough ( ent par / ang enx hgt lst md1 md2 rtn )
    (if (setq lst (LM:textbox (setq enx (entget ent))))
        (progn
            (setq hgt (cdr (assoc 40 enx))
                  md1 (mid   (car  lst) (last  lst))
                  md2 (mid   (cadr lst) (caddr lst))
                  ang (angle (car  lst) (last  lst))
            )
            (foreach itm par
                (setq rtn
                    (cons
                        (entmakex
                            (append
                               '(   (000 . "LWPOLYLINE")
                                    (100 . "AcDbEntity")
                                    (100 . "AcDbPolyline")
                                    (090 . 2)
                                    (070 . 0)
                                )
                                (LM:defaultprops enx)
                                (list
                                    (cons  043 (* (cadr itm) hgt))
                                    (cons  038 (caddar lst))
                                    (cons  010 (polar md1 ang (* (car itm) hgt)))
                                    (cons  010 (polar md2 ang (* (car itm) hgt)))
                                    (assoc 210 enx)
                                )
                            )
                        )
                        rtn
                    )
                )
            )
        )
    )
    rtn
)

;; Midpoint  -  Lee Mac
;; Returns the midpoint of two points

(defun mid ( a b )
    (mapcar (function (lambda ( a b ) (/ (+ a b) 2.0))) a b)
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

;; Text Box  -  Lee Mac (based on code by gile)
;; Returns the bounding box of a text, mtext, or attribute entity (in OCS)
;; enx - [lst] Text, MText or Attribute DXF data list

(defun LM:textbox ( enx / bpt hgt jus lst ocs org rot wid )
    (cond
        (   (and (= "ATTRIB" (cdr (assoc 000 enx)))
                 (= "Embedded Object" (cdr (assoc 101 enx)))
            )
            (LM:textbox (cons '(000 . "MTEXT") (member '(101 . "Embedded Object") enx)))
        )
        (   (cond
                (   (wcmatch  (cdr (assoc 000 enx)) "ATTRIB,TEXT")
                    (setq bpt (cdr (assoc 010 enx))
                          rot (cdr (assoc 050 enx))
                          lst (textbox enx)
                          lst (list (car lst) (list (caadr lst) (cadar lst)) (cadr lst) (list (caar lst) (cadadr lst)))
                    )
                )
                (   (= "MTEXT" (cdr (assoc 000 enx)))
                    (setq ocs  (cdr (assoc 210 enx))
                          bpt  (trans (cdr (assoc 010 enx)) 0 ocs)
                          rot  (angle '(0.0 0.0) (trans (cdr (assoc 011 enx)) 0 ocs))
                          wid  (cdr (assoc 042 enx))
                          hgt  (cdr (assoc 043 enx))
                          jus  (cdr (assoc 071 enx))
                          org  (list (cond ((member jus '(2 5 8)) (/ wid -2.0)) ((member jus '(3 6 9)) (- wid))      (0.0))
                                     (cond ((member jus '(1 2 3)) (- hgt))      ((member jus '(4 5 6)) (/ hgt -2.0)) (0.0))
                               )
                          lst  (list org (mapcar '+ org (list wid 0)) (mapcar '+ org (list wid hgt)) (mapcar '+ org (list 0 hgt)))
                    )
                )
            )
            (   (lambda ( m ) (mapcar '(lambda ( p ) (mapcar '+ (mxv m p) bpt)) lst))
                (list
                    (list (cos rot) (sin (- rot)) 0.0)
                    (list (sin rot) (cos rot)     0.0)
                   '(0.0 0.0 1.0)
                )
            )
        )
    )
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

(princ)