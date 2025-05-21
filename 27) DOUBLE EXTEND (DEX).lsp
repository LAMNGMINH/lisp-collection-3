;;-------------------------=={ Double Extend }==------------------------;;
;;                                                                      ;;
;;  This program allows the user to extend both ends of all lines,      ;;
;;  arcs & polylines in a selection by a given amount.                  ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'dex' (Double Extend) at the        ;;
;;  AutoCAD command-line, the user is first prompted to specify the     ;;
;;  amount by which to extend the objects at both ends. At this prompt, ;;
;;  the program will offer the last entered extension value as a        ;;
;;  default option; this default value will also be remembered between  ;;
;;  drawing sessions.                                                   ;;
;;                                                                      ;;
;;  Following a valid response, the program will then prompt the user   ;;
;;  to make a selection of lines, arcs, open 2D polylines (both light   ;;
;;  & heavy), or open 3D polylines whose start & end points (or start   ;;
;;  & end segments) are to be lengthened by the given amount.           ;;
;;                                                                      ;;
;;  The program is compatible with all of the aforementioned objects    ;;
;;  contructed in all UCS planes, and with polylines containing arc     ;;
;;  segments.                                                           ;;
;;                                                                      ;;
;;  The program will automatically ignore object end points for which   ;;
;;  an extension by the specified distance is not possible, for         ;;
;;  example, if extending both ends of an arc by the given distance     ;;
;;  will cause the end points to meet or overlap.                       ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2015  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2015-05-03                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:dex ( / *error* an1 an2 dis ent enx ext idx lst pt1 pt2 rad sel tmp typ )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (if (and (setq tmp (getenv "LMac\\dex"))
             (setq tmp (distof tmp))
             (<  0 tmp)
        )
        (setq ext tmp)
    )
    (initget 6)
    (if
        (and
            (setq ext
                (cond
                    (   (getdist
                            (strcat "\nSpecify extension"
                                (if ext (strcat " <" (rtos ext) "> : ") ": ")
                            )
                        )
                    )
                    (   ext   )
                )
            )
            (setq sel
                (LM:ssget "\nSelect lines, arcs and/or polylines to extend: "
                   '(   "_:L"
                        (
                            (-4 . "<OR")
                                (0 . "LINE,ARC")
                                (-4 . "<AND")
                                    (0 . "LWPOLYLINE")
                                    (-4 . "<NOT")
                                        (-4 . "&=") (70 . 1)
                                    (-4 . "NOT>")
                                (-4 . "AND>")
                                (-4 . "<AND")
                                    (0 . "POLYLINE")
                                    (-4 . "<NOT")
                                        (-4 . "&=") (70 . 87)
                                    (-4 . "NOT>")
                                (-4 . "AND>")
                            (-4 . "OR>")
                        )
                    )
                )
            )
        )
        (progn
            (setenv "LMac\\dex" (rtos ext))
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      enx (entget ent)
                      typ (cdr (assoc 0 enx))
                )
                (cond
                    (   (= "LINE" typ)
                        (setq pt1 (cdr (assoc 10 enx))
                              pt2 (cdr (assoc 11 enx))
                              dis (distance pt1 pt2)
                        )
                        (if (not (equal 0.0 dis 1e-8))
                            (progn
                                (setq dis (/ (+ dis ext) dis))
                                (entmod
                                    (subst
                                        (cons  10 (mapcar '+ pt2 (vxs (mapcar '- pt1 pt2) dis)))
                                        (assoc 10 enx)
                                        (subst
                                            (cons  11 (mapcar '+ pt1 (vxs (mapcar '- pt2 pt1) dis)))
                                            (assoc 11 enx)
                                            enx
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (   (= "ARC" typ)
                        (setq rad (cdr (assoc 40 enx))
                              an1 (cdr (assoc 50 enx))
                              an2 (cdr (assoc 51 enx))
                        )
                        (if (< (+ (* rad (rem (+ (- an2 an1) pi pi) (+ pi pi))) ext ext) (* 2.0 rad pi))
                            (entmod
                                (subst
                                    (cons  50 (- an1 (/ ext rad)))
                                    (assoc 50 enx)
                                    (subst
                                        (cons  51 (+ an2 (/ ext rad)))
                                        (assoc 51 enx)
                                        enx
                                    )
                                )
                            )
                        )
                    )
                    (   (= "LWPOLYLINE" typ)
                        (entmod
                            (append
                                (reverse (member (assoc 39 enx) (reverse enx)))
                                (apply 'append (LM:dex:extendpoly (LM:lwvertices enx) ext))
                                (list (assoc 210 enx))
                            )
                        )
                    )
                    (   (= "POLYLINE" typ)
                        (while (/= "SEQEND" (cdr (assoc 0 enx)))
                            (setq ent (entnext ent)
                                  enx (entget  ent)
                                  lst (cons enx lst)
                            )
                        )
                        (foreach vtx (LM:dex:extendpoly (reverse (cdr lst)) ext) (entmod vtx))
                        (entupd (cdr (assoc -2 enx)))
                    )
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

(defun LM:dex:extendpoly ( lst ext / ang bul cen dis len pt1 pt2 pt3 rad )
    (setq pt1 (cdr (assoc 10 (car  lst)))
          pt2 (cdr (assoc 10 (cadr lst)))
          bul (cdr (assoc 42 (car  lst)))
          dis (distance pt1 pt2)
    )
    (if (equal 0.0 bul 1e-8)
        (if (not (equal 0.0 dis 1e-8))
            (setq dis (/ (+ dis ext) dis)
                  lst
                (cons
                    (subst
                        (cons  10 (mapcar '+ pt2 (vxs (mapcar '- pt1 pt2) dis)))
                        (assoc 10 (car lst))
                        (car lst)
                    )
                    (cdr lst)
                )
            )
        )
        (progn
            (setq cen (LM:bulgecentre pt1 pt2 bul)
                  rad (/ (* dis (1+ (* bul bul))) 4 (abs bul))
                  len (abs (* 4 (atan bul) rad))
            )
            (if (< (+ len ext) (* rad 2 pi))
                (setq pt3 (polar cen ((if (minusp bul) + -) (angle cen pt1) (/ ext rad)) rad)
                      ang ((if (minusp bul) - +) (atan bul) (/ ext rad 4.0))
                      lst
                    (cons
                        (subst
                            (cons  10 pt3)
                            (assoc 10 (car lst))
                            (subst
                                (cons  42 (/ (sin ang) (cos ang)))
                                (assoc 42 (car lst))
                                (car lst)
                            )
                        )
                        (cdr lst)
                    )
                )
            )
        )
    )
    (setq lst (reverse lst)
          pt1 (cdr (assoc 10 (car  lst)))
          pt2 (cdr (assoc 10 (cadr lst)))
          bul (cdr (assoc 42 (cadr lst)))
          dis (distance pt1 pt2)
    )
    (if (equal 0.0 bul 1e-8)
        (if (not (equal 0.0 dis 1e-8))
            (setq dis (/ (+ dis ext) dis)
                  lst
                (cons
                    (subst
                        (cons  10 (mapcar '+ pt2 (vxs (mapcar '- pt1 pt2) dis)))
                        (assoc 10 (car lst))
                        (car lst)
                    )
                    (cdr lst)
                )
            )
        )
        (progn
            (setq cen (LM:bulgecentre pt2 pt1 bul)
                  rad (/ (* dis (1+ (* bul bul))) 4 (abs bul))
                  len (abs (* 4 (atan bul) rad))
            )
            (if (< (+ len ext) (* rad 2 pi))
                (setq pt3 (polar cen ((if (minusp bul) - +) (angle cen pt1) (/ ext rad)) rad)
                      ang ((if (minusp bul) - +) (atan bul) (/ ext rad 4.0))
                      lst
                    (vl-list*
                        (subst
                            (cons  10 pt3)
                            (assoc 10 (car lst))
                            (car lst)
                        )
                        (subst
                            (cons  42 (/ (sin ang) (cos ang)))
                            (assoc 42 (cadr lst))
                            (cadr lst)
                        )
                        (cddr lst)
                    )
                )
            )
        )
    )
    (reverse lst)
)

;; Vector x Scalar  -  Lee Mac
;; Args: v - vector in R^n, s - real scalar

(defun vxs ( v s )
    (mapcar '(lambda ( n ) (* n s)) v)
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


(vl-load-com)
(princ
    (strcat
        "\n:: DoubleExtend.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"dex\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;