;;--------------------------------------------------------------------------
;; Ploetzl, Mieling - Vienna                       compuserve id 100115,3172
;;--------------------------------------------------------------------------
;;  DIMARC.LSP
;;
;;  dim length of an arc
;;  uses the autocad command dim angle and replaces the angle measure
;;  with the length of the arc segment
;;
;;  CALL: dima
;;  
;;  0992pf ver11c2/0.1   
;;  0494pf ver12c1/0.2                            
;;--------------------------------------------------------------------------
;
; error, save and restore variables 
;
(defun be:er (s)
   (if (/= s "Function cancelled" )(prompt (strcat "\nError:  " s)))
   (setq *error* be_or)
   (setvar "osmode" oosm)
   (setvar "cmdecho" ocmd)
   (princ)
)
;
; up info
;
(defun be:inf ()
  (prompt (strcat "\nLayer: " (getvar "clayer") 
                  "\tTextheight: " (rtos (getvar "dimtxt"))
                  "\tDimStyle: " (getvar "dimstyle")
          )
  )
)
(princ ".")
;;
;; dima
;;
(defun c:dima (/ e d piu sel elem selnam rad ang lae antw ptr pt1 pt2 rad1 rad2)

;error, save var
  (setq be_or *error* *error* be:er
        oosm (getvar "osmode")
        ocmd (getvar "cmdecho")
        piu (/ 180.0 pi)
        e (getvar "lunits")
        d (getvar "luprec")
  )
  (setvar "cmdecho" 0)
  (setvar "dimse1" 0)
  (setvar "dimse2" 0)

  (prompt "\nDim ArcLength                                                 ver12c1/0.2pf0494") 
  (be:inf)
  (if (setq sel (entsel "\nSelect ARC / RETURN to select points: "))
    (progn
      (setq elem (entget (car sel)))
      (prompt (setq selnam (cdr (assoc 0 elem))))
      (if (= selnam "ARC")
        (progn
          (setq rad (cdr (assoc 40 elem)))      
          (setq ang (- (cdr (assoc 51 elem))(cdr (assoc 50 elem)) ) )
          (if (< ang 0)(setq ang (+ ang (* 2 pi) ) ) )
          (setq ang (* ang piu))
          (setq lae (/ (* pi ang rad) 180.0) )
          (prompt (strcat "\nRadius = " (rtos rad) 
                          "\tAngle = " (rtos ang) 
                          "\tLength = " (rtos lae)
                  )
          );prompt
          (prompt (strcat "\nLength arc: <" (rtos lae) "> "))
          (setq antw (getdist))
          (if (/= antw nil)(setq lae antw))
          (setvar "cmdecho" 1)
          (command "_.dim1" "_ang" sel pause (rtos lae) pause)
        );progn arc
        (prompt "\nNo arc.") 
      );if elem
    );progn
    (progn
      (setq ptr (getpoint "\nMidpoint:        "))
      (setq pt1 (getpoint "\nFirst arc point: ")) 
      (setq pt2 (getpoint "\nSecend arc point: ")) 
      (setq ang1 (angle ptr pt1)
            ang2 (angle ptr pt2)
            rad1 (distance ptr pt1)
            rad2 (distance ptr pt2)
      )
      (if (/= (rtos rad1 2 10) (rtos rad2 2 10))
        (prompt "\nRadius is different!")
        (progn
          (if (> ang2 ang1)
            (setq ang (/ (* (- ang2 ang1) 180.0) pi))
            (setq ang (/ (* (- ang1 ang2) 180.0) pi))
          )
          (setq lae (/ (* ang pi rad1)  180.0))
          (command "_.arc" "_c" ptr pt1 pt2)
          (setq sel (list (ssname (ssget "_L") 0) pt1)) 
          (setvar "cmdecho" 1)
          (command "_.dim1" "_ang" sel pause (rtos lae) pause)
          (command "_.erase" sel "")
        );progn
      );if
    );progn
  );if
;error restore
  (setq *error* be_or)
  (setvar "cmdecho" ocmd)
  (setvar "osmode" oosm)
  (princ)

) ;bemw
;;
(princ ".")
(prompt "\nDimarc.lsp loaded. Call command with DIMA.                               pf0494")
(princ)
;;end dimarc.lsp pf

