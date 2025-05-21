;rozdílová kóta - www.cadforum.cz, 2022
(defun C:DIMDIFF ( / p1 p2 p3 dx dy txdist)

 (defun dif0 (a b / d dt)
  (setq d (- b a))
  (setq dt (rtos d 2 (getvar "DIMDEC")))
  (if (> d 0.0)(setq dt (strcat "+" dt)))
  dt
 )
 (setq txdist (getvar "DIMTXT")) ; offset
 (setq p1 (getpoint "\nFirst dimension point: "))
 (setq p2 (getpoint p1 "\nSecond dimension point:"))
 ;(setq p3 (polar p2 (+ (angle p1 p2)(/ pi 2.0)) txdist))
 (setq dx (dif0 (car p1)(car p2)))
 (setq dy (dif0 (cadr p1)(cadr p2)))
 (command "_.dimaligned" "_non" p1 "_non" p2 "_t" (strcat "X: " dx "\\PY: " dy))
 (princ)
)