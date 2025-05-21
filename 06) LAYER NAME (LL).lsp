(defun alg-ang	(obj pnt)
  (angle '(0. 0. 0.)
	 (vlax-curve-getfirstderiv
	   obj
	   (vlax-curve-getparamatpoint
	     obj
	     pnt
	     )
	   )
	 )
  )

(defun C:LL (/ *error* acsp adoc ang fld midp mtx rot sset txtpt)
  (defun *error*  (msg)
    (if
      (vl-position
	msg
	'("console break"
	  "Function cancelled"
	  "quit / exit abort"
	  )
	)
       (princ "Error!")
       (princ msg)
       )
       (vla-endundomark      
(vla-get-activedocument
	       (vlax-get-acad-object)
	       )
)
    (princ)
    )

  (or adoc
      (setq adoc
	     (vla-get-activedocument
	       (vlax-get-acad-object)
	       )
	    )
      )
  (if (and
	(= (getvar "tilemode") 0)
	(= (getvar "cvport") 1)
	)
    (setq acsp (vla-get-paperspace adoc))
    (setq acsp (vla-get-modelspace adoc))
    )
  (vla-startundomark      
adoc
)
(if (setq sset (ssget "_:L" (list (cons 0 "*LINE,ARC,CIRCLE,ELLIPSE"))))
  (foreach obj
    (mapcar 'vlax-ename->vla-object
      (vl-remove-if 'listp
        (mapcar 'cadr (ssnamex sset))))
 (if (not (eq "AcDbArc"  (vla-get-objectname obj)))  
(setq midp (vlax-curve-getclosestpointto obj
	     (vlax-curve-getpointatparam obj
   ( / (- (vlax-curve-getEndParam obj)
	  (vlax-curve-getStartParam obj)) 2))
      )
      )
   (setq midp (vlax-curve-getclosestpointto obj
		(vlax-curve-getpointatdist obj
   ( / (vla-get-arclength obj) 2)))
		 )
   )


(setq ang (alg-ang obj midp))
    
(if (> pi ang (/ pi 2))
        (setq ang (+ ang pi))
  )
(if (> (* pi 1.5) ang pi)
  (setq ang (+ ang pi))
  )
      (setq rot (+ ang (/ pi 2)))

    (setq txtpt (polar midp rot
		       (if (zerop (getvar "dimtxt"))
			 0.1
			 (/ (getvar "dimtxt") 2)))
	  )

(setq fld (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			  (itoa (vla-get-objectid obj))
			  ">%).Layer>%")

)

  	 (setq mtx (vlax-invoke
		     acsp 'AddMText midp 0.0 fld)
	       )
	 (vlax-put mtx 'AttachmentPoint
		   8
		   
		   )
    (vlax-put mtx 'InsertionPoint
		   txtpt
		   )
         (vlax-put mtx 'Rotation
		   ang
		   )
    )
  )
(princ)
)
(princ "\n\t\t\tType LL to label curves with layer name\t")
(prin1)

(vl-load-com)