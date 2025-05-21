(defun c:LPL (/ e ss l p i)
  (if
    (setq l 0.0 ss (ssget '((0 . "LINE,SPLINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))
              l (+ l (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
        )
      )
      (if
        (setq p (getpoint "\nSpecify a point to insert text: "))
        (entmake
          (list
            '(0 . "TEXT")
            '(100 . "AcDbText")
            (cons 10 (trans p 1 0))
            (cons 40 (/ 5.0 (getvar 'cannoscalevalue)))
            (cons 1 (rtos l))
          )
        )
        (princ (strcat "\nTotal length = " (rtos l)))
      )
    )
  )
  (princ)
)