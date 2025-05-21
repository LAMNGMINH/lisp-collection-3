; DDArea Lisp Routine
;
; Function to list the Square Feet, Square Yards, Acres, and Hectares
; by the selection of a polyline in Architectural, Decimal, and Metric Units
; by using an Alert Dialog Box.
;
; Original by Patrick Slattery			05/07/96
; Modified by Jeff Tippit			07/23/96
; Modified again by Jeff Tippit			01/14/02
;	Added Square Yards, Square Meters, and Hectares
;	Combined 2 Units and added Metric
;
;
(Defun C:DDAREA ()
   (setvar "CMDECHO" 0 )
   (SETQ SS (entsel "Select Object To Get Area of:"))
   (PROGN (COMMAND "AREA" "o" SS))
   (SETQ SQFT (/ (GETVAR "AREA") 144))			;architectural
   (SETQ SQF2 (/ (GETVAR "AREA") 1))			;decimal
   (SETQ SQFM (/ (* (GETVAR "AREA") 0.00155) 144))	;metric
   (setq ACRES (cvunit SQFT "sq feet" "acres"))
   (setq ACRE2 (cvunit SQF2 "sq feet" "acres"))
   (setq ACREM (cvunit SQFM "sq feet" "acres"))
   (setq SQYDS (cvunit SQFT "sq feet" "sq yard"))
   (setq SQYD2 (cvunit SQF2 "sq feet" "sq yard"))
   (setq SQYDM (cvunit SQFM "sq feet" "sq yard"))
   (setq SQMTR (cvunit SQFT "sq feet" "sq meter"))
   (setq SQMT2 (cvunit SQF2 "sq feet" "sq meter"))
   (setq SQMTM (cvunit SQFM "sq feet" "sq meter"))
   (setq HECTA (cvunit SQFT "sq feet" "hectare"))
   (setq HECT2 (cvunit SQF2 "sq feet" "hectare"))
   (setq HECTM (cvunit SQFM "sq feet" "hectare"))
   (alert
      (strcat "\n    Architectural Scale"
              "\n"
              "\n      Square Feet = " (rtos SQFT 2 2)
              "\n      Square Meters = " (rtos SQMTR 2 2)
              "\n      Square Yards = " (rtos SQYDS 2 2)
              "\n      Acres = " (rtos ACRES 2 3)
              "\n      Hectares = " (rtos HECTA 2 4)
              "\n"
              "\n    Decimal Scale"
              "\n"
              "\n      Square Feet = " (rtos SQF2 2 2)
              "\n      Square Meters = " (rtos SQMT2 2 2)
              "\n      Square Yards = " (rtos SQYD2 2 2)
              "\n      Acres = " (rtos ACRE2 2 3)
              "\n      Hectares = " (rtos HECT2 2 4)
              "\n"
              "\n    Metric Scale"
              "\n"
              "\n      Square Feet = " (rtos SQFM 2 2)
              "\n      Square Meters = " (rtos SQMTM 2 2)
              "\n      Square Yards = " (rtos SQYDM 2 2)
              "\n      Acres = " (rtos ACREM 2 3)
              "\n      Hectares = " (rtos HECTM 2 4)
      )
   )	;put up alert box
   (PRINC)
)
