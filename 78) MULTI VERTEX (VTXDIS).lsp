;Add polyline vertices in intervals
;CAD Studio, www.cadstudio.cz  www.cadforum.cz  parts: Lee Mac
(defun C:VTXDIST ( / pline plineo pt ptpar stan int seg)

(defun addvertex 
       (ATPT / pt p1 p2 bu1 bu2 center ent ew iang nw obj old_bulge radius sw vi vr)

  (defun getbulge (st end cw / ang) ; by CAB
    (setq ang
           (if cw
             (if (> st end)
               (- st end)
               (+ (- (* 2 pi) end) st)
             )
             (if (< end st)
               (+ (- (* 2 pi) st) end)
               (- end st)
             )
           )
    )
    (setq ang (/ ang 4)
          ang (/ (sin ang) (cos ang)))
    (if cw (-(abs ang)) (abs ang))
  )

  ;(setq ent (entsel "\n Select point on polyline to add vertex: "))
      (setq pt ATPT ;(vlax-curve-getclosestpointto (car ent) (cadr ent)) ; new point
            pt (list (car pt) (cadr pt)) ; 2D point
            vr (vlax-curve-getparamatpoint plineo pt)
            vi (fix vr)
            vr (- vr vi)
      )
      (setq obj plineo ;      (vlax-ename->vla-object (car ent))
            old_bulge (vla-getbulge obj vi)
      )
      (vla-GetWidth obj vi 'sw 'ew)
      (vlax-invoke obj 'AddVertex (1+ vi) pt) ; add new vertex
      (if (equal sw ew 0.0001)
        (vla-SetWidth obj (1+ vi) sw ew)
        (progn
          (setq nw (* (+ sw ew) vr))
          (vla-SetWidth obj vi sw nw)
          (vla-SetWidth obj (1+ vi) nw ew)
        )
      )
      (if (not (zerop old_bulge))
        (progn
          (setq p1     (vlax-curve-getpointatparam obj vi)
                p2     (vlax-curve-getpointatparam obj (+ vi 2))
                iang   (* (atan old_bulge) 4) ; delta angle
                radius (/ (distance p1 p2) (sin (/ iang 2)) 2)
                center (polar p1 (+ (angle p1 p2) (/ (- pi iang) 2)) radius)
          )
          (setq bu1 (getbulge (angle center p1)(angle center pt)(minusp old_bulge))
                bu2 (getbulge (angle center pt)(angle center p2)(minusp old_bulge))
          )
          (vla-setbulge obj vi bu1)
          (vla-setbulge obj (1+ vi) bu2)
        )
      )
 
)




 (vl-load-com)
 (setq pline (entsel "\nSelect a polyline to divide/measure: "))
 (if pline (progn
    (setq pline (car pline))
	(setq plineo (vlax-ename->vla-object pline))
	(if (member (cdr (assoc 0 (entget pline))) '("LWPOLYLINE" "SECTION")) (progn
		(if _lastVTXDIST (initget 6 "Number")(initget 7 "Number"))
		(setq int (getdist (strcat "\nSpecify the segment size or [Number]" (if _lastVTXDIST (strcat " <" (rtos _lastVTXDIST 2) ">") "") ":")))
		 (if (eq int "Number")(progn
		  (initget 7)
		  (setq seg (getint "\nNumber of segments: "))
		  (setq int (/ (vlax-get-property plineo 'Length) seg))
		  (princ (strcat " (size: " (rtos int 2) ")"))
		 ))
		(if (not int)(setq int _lastVTXDIST)(setq _lastVTXDIST int))
		(setq stan int)
		(while (setq pt (vlax-curve-getPointAtDist plineo stan))
		  (setq ptpar (vlax-curve-getParamAtPoint plineo pt))
		  ;(vlax-invoke plineo 'AddVertex (1+ (fix ptpar))  (list (car pt)(cadr pt)))
		  (addvertex pt)
		  (setq stan (+ stan int))
		);
		(princ " added.")
		(sssetfirst nil (ssadd pline))
	) ;else
	(princ " not a lwpolyline ! ")
	) ;if eq
 )) ;if pline
 (prin1)
)