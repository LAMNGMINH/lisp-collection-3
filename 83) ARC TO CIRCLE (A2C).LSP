;*****************************************************************************
;                            ATC.LSP V1.0
;                           by Zoltan Toth
;    ZOTO Technologies,
;    23 Greenhills Dve,
;    Melton 3337.
;    E-MAIL: zoltan.toth@ains.net.au
;       WWW: http://www.ains.net.au/zoto/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  This program takes any number of arcs and converts them into circles.
;  Each circle is a new object with all the properties of the arc it 
;  replaces. Non-arc objects are ignored.
;
;*****************************************************************************
(defun C:ATC (/ CTR2 CTR3 D2 SS2 OBN2 OBD2 OBD3)
 (setq CTR2 0)                             ;initialize CTR2 for object counter
 (prompt "\nSelect arcs to convert to circles: ")
 (setq SS2 (ssget '((0 . "ARC"))))        ;create selection set with arcs only
 (repeat (sslength SS2)                                ;repeat for each object
  (setq OBN2 (ssname SS2 CTR2))                               ;get object name
  (setq OBD2 (entget OBN2))                             ;get object data lists
;substitute CIRCLE in assoc. 0
  (setq OBD2 (subst (cons 0 "CIRCLE")(assoc 0 OBD2) OBD2))
  (setq CTR3 (1- (length OBD2)))         ;set CTR3 to 1 less than size of OBD2
  (repeat (length OBD2)                          ;repeat for each list in OBD2
   (setq D2 (nth CTR3 OBD2))       ;set D2 to an association list from the arc
;;;;
;if association list is neither a start or end angle,
;copy the association list to OBD3
   (if (and (/= 50 (car D2))(/= 51 (car D2)))        ;check for assoc. 50 & 51
    (setq OBD3 (cons D2 OBD3))                       ;copy assoc. list to OBD3
   )
   (setq CTR3 (1- CTR3))                               ;decrement counter CTR3
  )                                                    ;end of second (repeat)
  (entdel OBN2)                                                    ;delete arc
  (entmake OBD3)                                                  ;make circle
  (setq OBD3 nil)                                           ;set OBD3 to (nil)
  (setq CTR2 (1+ CTR2))                              ;increment object counter
 )                                                      ;end of first (repeat)
 (princ)                                                         ;exit quietly
)
