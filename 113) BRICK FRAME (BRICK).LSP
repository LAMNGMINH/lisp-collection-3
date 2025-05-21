;brick.lsp
;version 1.0
;8/7/99 9:56
;  ________________________________________________________________________
;  
;  (C) Copyright 1999 by David Schmidt  
;         
;  web page:  www.jps.net/david001    
;  email:  david001@jps.net
;
;
;This software is liscensed under the provisions of the attached documentation
;and is available for a trial period of 30 days.  After 30 days, you
;must register the software for a charge of
;single user:  $5.00
;site user (unlimited seats):  $20.00
;make check payable to:
;
;David Schmidt
;207 A Street #6
;Boston, MA  02210
;
;see attached documentation for setup information and registration form.
;






     (defun b1 ()

          (command "array" br "" "" course 1 cheight)
          (command "array" htch "" "" course 1 cheight)
          (command "array" j1 "" "" course 1 cheight)
          (command "array" j2 "" "" course 1 cheight)
          (setvar "osmode" oldsnap)
          (setvar "blipmode" oldblip)
      );defun b1


     (defun b2 ()
          (setq pt7 (getpoint "\nPick point for top of wall: "))
          (setq dist (distance  pt1 pt7))
          (setq c1 (/ dist cheight))
          (setq course (fix c1))
          (command "array" br "" "" course 1 cheight)
          (command "array" htch "" "" course 1 cheight)
          (command "array" j1 "" "" course 1 cheight)
          (command "array" j2 "" "" course 1 cheight)
     );defun b2


(defun C:brick()          
          
          (setq oldsnap (getvar "osmode"))
          (setvar "osmode" 0)
          (setq oldblip (getvar "blipmode"))
          (setvar "blipmode" 0)
          (setq width 3.625)
          (setq height 2.2917)
          (setq cheight 2.6667)
          (setq j 0.375)
          (setq pt1 (getpoint "\nPick lower left corner of brick wall: "))
          (setvar "osmode" 0)   
          (setq pt2 (polar pt1 0 width))
          (setq pt3 (polar pt2 (* pi 0.5) height))
          (setq pt4 (polar pt3 pi width))
          (command "pline" pt1 pt2 pt3 pt4 pt1 "")
          (setq br (entlast))
          (command "hatch" "ANSI31" "6" "0" br "")
          (setq htch (entlast))
          (setq pt5 (polar pt2 (* pi 1.5) j))
          (setq pt6 (polar pt1 (* pi 1.5) j))
          (command "arc" pt2 "E" pt5 "R" 0.1875 "")
          (setq j1 (entlast))
          (command "arc" pt6 "E" pt1 "R" 0.1875 "")
          (setq j2 (entlast))         
          (setq course (getint "\nHow many courses (Enter to pick point): "))
          (if (= course nil)
		(b2)
		(b1)
	  );if
          (setvar "osmode" oldsnap)
          (setvar "blipmode" oldblip)         
(princ)
);defun

          