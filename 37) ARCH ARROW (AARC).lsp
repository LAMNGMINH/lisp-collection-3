;;---------------------------=={ Arrow Arc }==--------------------------;;
;;                                                                      ;;
;;  This program enables the user to construct an arc with arrowheads   ;;
;;  at each end point or at both end points, with the arrowheads        ;;
;;  aligned with the arc.                                               ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'aarc' (Arrow Arc) at the AutoCAD   ;;
;;  command-line, the user may construct an arc with the same options   ;;
;;  available as the standard in-built AutoCAD ARC command.             ;;
;;                                                                      ;;
;;  If the constructed arc is long enough to accommodate one or two     ;;
;;  arrowheads, the program will proceed to generate a 2D Polyline      ;;
;;  (LWPolyline) arc segment with additional segments of varying width  ;;
;;  at the start and/or end point forming the arrowheads.               ;;
;;                                                                      ;;
;;  The dimensions of the resulting arrowheads and the option to        ;;
;;  determine whether the arrowheads are created at the start of the    ;;
;;  arc, end of the arc, or at both the start & end, may be altered     ;;
;;  using the 'aarcsettings' command; these parameters will be          ;;
;;  remembered between drawing sessions.                                ;;
;;                                                                      ;;
;;  This program will also perform successfully under all UCS & View    ;;
;;  configurations.                                                     ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2016  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2012-07-17                                      ;;
;;                                                                      ;;
;;  - First release.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Version 1.1    -    2013-05-26                                      ;;
;;                                                                      ;;
;;  - Added 'aarcsettings' command to enable the user to alter the      ;;
;;    arrow length & width without modifying the code.                  ;;
;;----------------------------------------------------------------------;;
;;  Version 1.2    -    2016-02-27                                      ;;
;;                                                                      ;;
;;  - Program modified to allow the user to specify whether to create   ;;
;;    an arrowhead at the start or end of the arc, or at both.          ;;
;;----------------------------------------------------------------------;;

(defun c:aarc ( / *error* an1 an2 an3 ang arl arw cen ent enx flg rad typ )
    
    (defun *error* ( msg )
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (and (setq arl (getenv "LMac\\aarcl")) (setq arl (distof arl 2))))
        (setq arl 1.0)
    )
    (if (not (and (setq arw (getenv "LMac\\aarcw")) (setq arw (distof arw 2))))
        (setq arw 0.5)
    )
    (if (not (and (setq typ (getenv "LMac\\aarct")) (member typ '("Start" "End" "Both"))))
        (setq typ "Both")
    )
    (princ
        (strcat
            "\nArrow Length: " (rtos arl 2) " | Width: " (rtos arw 2) " | Arrowheads: " typ
            "\nType \"aarcsettings\" to alter settings.\n"
        )
    )
    (setq ent (entlast))
    (command "_.arc")
    (while (= 1 (logand 1 (getvar 'cmdactive)))
        (command "\\")
    )
    (if (not (eq ent (setq ent (entlast))))
        (progn
            (setq enx (entget ent)
                  cen (cdr (assoc 10 enx))
                  rad (cdr (assoc 40 enx))
                  an1 (cdr (assoc 50 enx))
                  an2 (cdr (assoc 51 enx))
                  an3 (/ arl rad)
                  ang (rem (+ pi pi (- an2 an1)) (+ pi pi))
                  flg (equal (trans (getvar 'lastpoint) 1 ent) (polar cen an1 rad) 1e-3)
            )
            (if (< arl (* rad ang (if (= typ "Both") 0.5 1.0)))
                (if
                    (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (if (= "Both" typ) 4 3))
                               '(070 . 0)
                                (cons 010 (polar cen an1 rad))
                               '(040 . 0.0)
                            )
                            (cond
                                (   (or (and flg (= "Start" typ)) (and (not flg) (= "End" typ)))
                                    (list
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ (- ang an3) 4.0)))
                                        (cons 010 (polar cen (- an2 an3) rad))
                                        (cons 040 arw)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ an3 4.0)))
                                    )
                                )
                                (   (= "Both" typ)
                                    (list
                                        (cons 041 arw)
                                        (cons 042 (tan (/ an3 4.0)))
                                        (cons 010 (polar cen (+ an1 an3) rad))
                                       '(040 . 0.0)
                                       '(041 . 0.0)                         
                                        (cons 042 (tan (/ (- ang an3 an3) 4.0)))
                                        (cons 010 (polar cen (- an2 an3) rad))
                                        (cons 040 arw)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ an3 4.0)))
                                    )
                                )
                                (   (list
                                        (cons 041 arw)
                                        (cons 042 (tan (/ an3 4.0)))
                                        (cons 010 (polar cen (+ an1 an3) rad))
                                       '(040 . 0.0)
                                       '(041 . 0.0)
                                        (cons 042 (tan (/ (- ang an3) 4.0)))
                                    )
                                )
                            )
                            (list
                                (cons 010 (polar cen an2 rad))
                                (cons 210 (trans '(0.0 0.0 1.0) 1 0 t))
                            )
                        )
                    )
                    (entdel ent)
                )
                (princ "\nArc too short to accommodate arrow(s).")
            )
        )
    )
    (princ)
)

;;----------------------------------------------------------------------;;

(defun c:aarcsettings ( / tmp )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Length <" (cond ((getenv "LMac\\aarcl")) ("1.0")) ">: ")))
        (setenv "LMac\\aarcl" (rtos tmp 2))
    )
    (initget 6)
    (if (setq tmp (getdist (strcat "\nSpecify Arrow Width <" (cond ((getenv "LMac\\aarcw")) ("0.5")) ">: ")))
        (setenv "LMac\\aarcw" (rtos tmp 2))
    )
    (initget "Start End Both")
    (if (setq tmp (getkword (strcat "\nArrows at [Start/End/Both] <" (cond ((getenv "LMac\\aarct")) ("Both")) ">: ")))
        (setenv "LMac\\aarct" tmp)
    )
    (princ)
)    
  
;;----------------------------------------------------------------------;;
  
(defun tan ( x )
    (if (not (equal 0.0 (cos x) 1e-10))
        (/ (sin x) (cos x))
    )
)

;;----------------------------------------------------------------------;;

(princ
    (strcat
        "\n:: ArrowArc.lsp | Version 1.2 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: \"AARC\" for Arrow Arc | \"AARCSETTINGS\" for Settings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;