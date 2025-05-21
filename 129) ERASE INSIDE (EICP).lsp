;;;		Erase Inside a Closed Pline 	





;;;---------------------------------------------------------------start of routine----------------------------------------------------------------;;;



(VL-load-com)



(defun c:EICP  ()



  (inivar)



  (princ "\nPick a CLOSED POLYLINE (and everything inside will be erased)...")



  (setq   CLOSEDPOLY



             (vlax-ename->vla-object



               (car (entsel "\nSelect object :"))



             )



  )



  (if (/= CLOSEDPOLY nil)



    (progn



      (setq POLYTRUEFALSE



                 (vlax-get-property CLOSEDPOLY 'objectname)



      )



      (if (= POLYTRUEFALSE "AcDbPolyline")



            (progn



              (setq   CLOSEDTRUEFALSE



                         (vlax-get-property



                           CLOSEDPOLY



                           'closed



                         )



              )



              (if (/= CLOSEDTRUEFALSE :vlax-true)



                (progn



                  (setq CLOSEDPOLY nil)



                  (princ "\nThe POLYLINE isn't CLOSED...")



                )



              )



            )



            (progn



              (setq CLOSEDPOLY nil)



              (princ "\nSelect a POLYLINE...")



            )



      )



    )



  )



 



  (while (= CLOSEDPOLY nil)



    (progn



      (princ "\nNothing Selected...")



      (setq CLOSEDPOLY



                 (vlax-ename->vla-object



                   (car (entsel "\nSelect object :"))



                 )



      )



      (if (/= CLOSEDPOLY nil)



            (progn



              (setq   POLYTRUEFALSE



                         (vlax-get-property CLOSEDPOLY 'objectname)



              )



              (if (= POLYTRUEFALSE "AcDbPolyline")



                (progn



                  (setq CLOSEDTRUEFALSE



                             (vlax-get-property CLOSEDPOLY 'closed)



                  )



                  (if (/= CLOSEDTRUEFALSE :vlax-true)



                        (progn



                          (setq CLOSEDPOLY nil)



                          (princ "\nThe POLYLINE isn't CLOSED...")



                        )



                  )



                )



                (progn



                  (setq CLOSEDPOLY nil)



                  (princ "\nSelect a POLYLINE...")



                )



              )



            )



      )



    )



  )



  (setq ENDPARAM (fix (vlax-curve-getEndParam CLOSEDPOLY)))



  (setq COUNT 0)



  (setq listapt nil)



  (while (<= COUNT ENDPARAM)



    (progn



      (setq xy (vlax-curve-getPointAtParam CLOSEDPOLY COUNT))



      (setq listapt (append listapt (list xy)))



      (setq COUNT (1+ COUNT))



    )



  )



  (setq lengthlista (length listapt))



  (setq   pt1 (nth (- lengthlista 2) listapt)



            pt2 (nth (1- lengthlista) listapt)



  )



  (setq   strpt1 (vl-princ-to-string pt1)



            strpt2 (vl-princ-to-string pt2)



  )



  (if (= strpt1 strpt2)



    (progn



      (setq listapt (vl-remove pt1 listapt))



    )



  )



  (setq selset (ssget "_WP" listapt))



  (if (/= selset nil)



    (progn



      (setq selnumb (sslength selset))



      (setq COUNT 0)



      (while (< COUNT selnumb)



            (progn



              (setq ent (ssname selset count))



              (entdel ent)



              (setq COUNT (1+ COUNT))



            )



      )



    )



    (princ "\nNo objects to erase...")



  )



  (EICP)



  (princ)



)





 



 



;;;-------------------;;;



(defun inivar ()



  (setq   cmd_ini (getvar "cmdecho")



            fla_ini    (getvar "flatland")



            osm_ini (getvar "osmode")



            ort_ini   (getvar "orthomode")



            plt_ini    (getvar "plinetype")



            aup_ini  (getvar "auprec")



            uni_ini   (getvar "unitmode")



            lun_ini   (getvar "lunits")



            diz_ini   (getvar "dimzin")



            edg_ini  (getvar "edgemode")



  )



  (setvar "CMDECHO" 0)



  (setvar "FLATLAND" 0)



  (setvar "OSMODE" 0)



  (setvar "ORTHOMODE" 0)



  (setvar "PLINETYPE" 2)



  (setvar "AUPREC" 0)



  (setvar "UNITMODE" 1)



  (setvar "LUNITS" 2)



  (setvar "DIMZIN" 0)



  (setvar "EDGEMODE" 1)



)



;;;-------------------;;;



(defun EICP ()



  (setvar "CMDECHO" cmd_ini)



  (setvar "FLATLAND" fla_ini)



  (setvar "OSMODE" osm_ini)



  (setvar "ORTHOMODE" ort_ini)



  (setvar "PLINETYPE" plt_ini)



  (setvar "AUPREC" aup_ini)



  (setvar "UNITMODE" uni_ini)



  (setvar "LUNITS" lun_ini)



  (setvar "DIMZIN" diz_ini)



  (setvar "EDGEMODE" edg_ini)



)



 



;;;---------------------------------------------------------------end of routine----------------------------------------------------------------;;;