;Simplified dimensioning (CAD Forum - www.cadforum.cz)

;DIMension line, single click, fixed dim offset
(defun C:DIMFIXL ( / ss e p1 p2 p3 txdist)
 (setq txdist (getvar "DIMTXT")) ; offset
 (princ "\nSelect a line: ")
 (setq ss (ssget "_:E:S" '((0 . "LINE"))))
 (if ss (progn
  (setq p1 (cdr (assoc 10 (entget (ssname ss 0)))))
  (setq p2 (cdr (assoc 11 (entget (ssname ss 0)))))
  (setq p3 (polar p2 (+ (angle p1 p2)(/ pi 2.0)) txdist))
  (command "_.dimaligned" "_non" p1 "_non" p2 "_m" "" "_non" p3)
 ))
 (princ)
)

;DIMension 2 points, 2-clicks, fixed dim offset
(defun C:DIMFIXT ( / p1 p2 p3 txdist)
 (setq txdist (getvar "DIMTXT")) ; offset
 (setq p1 (getpoint "\nFirst dimension point: "))
 (setq p2 (getpoint p1 "\nSecond dimension origin:"))
 (setq p3 (polar p2 (+ (angle p1 p2)(/ pi 2.0)) txdist))
 (command "_.dimaligned" "_non" p1 "_non" p2 "_m" "" "_non" p3)
 (princ)
)