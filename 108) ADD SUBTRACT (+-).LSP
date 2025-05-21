; ADDNUM.LSP - 12 Feb 1987 - Stan H. Bimson - CompuServe 73507,3475
;
;     as printed in CADLINES, the newsletter of
;     the Nashville AutoCAD Users' Group (NAUG)
;     April 1988, Vol. 2, No. 7
;
; CAD/Engineering Services (615)824-4938
; Rt2 Box 293 Forest Harbor Drive
; Hendersonville, TN 37075-9802
;
; This program does not work on dimension that have been
; placed in a drawing with ADIM.

(DeFun FixStr(Str)
 (While (Member (SubStr Str 1 1)
   '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (SetQ Str (SubStr Str 2))))

(DeFun C:Add()
 (Princ "\nAdding machine")
 (SetQ Fun +
      Pmpt "\npick screen +num >() "
       Tot 0.0)
 (While (Not (Null Fun))
  (While (SetQ E
        (EntSel (StrCat (SubStr Pmpt 1 20)
        (RToS Tot) (SubStr Pmpt 21))))
   (SetQ Str (Cdr (Assoc 1 (entget(car e))))
         Tot (Fun Tot
               (ProgN
                (SetQ Num (Float (AToI Str)))
                (SetQ Str (FixStr Str))
                (If (= (SubStr Str 1 2) "'-")
                    (ProgN (SetQ Num (* Num 12.0) Str (SubStr Str 3))
                           (SetQ Num (+ Num (Float (AToI Str))))
                           (SetQ Str (FixStr Str))))
                (If (/= (SetQ F (Float (AToI Str))) 0)
                    (+ (/ F (AToI (SubStr (FixStr (SubStr Str 2)) 2))) Num)
                    Num))))
  ); end while get input
 (SetQ F T)
 (While F
  (SetQ St (GetString "\nEnter + or - (k)eyboard, <q>uit : "))
   (Cond
    ((Member St '("q" "Q" ""))
     (SetQ F Nil Fun Nil))
    ((= St "+")
     (SetQ Fun + Pmpt "\npick screen +num >() " F Nil))
    ((= St "-")
     (SetQ Fun - Pmpt "\npick screen -num >() " F Nil))
    ((Member St '("k" "K"))
     (While (/= "" (SetQ Str
      (GetString (StrCat "\nEnter keyboard num (" (RToS Tot) ") : "))))
        (SetQ Num (AToF Str)
             Sign (If (MinusP Num) -1 1)
              Num (* Num Sign)
               Ft (Fix Num)
              Num (* (- Num Ft) 100.0)
               In (Fix (+ Num 1.0e-12))
              Num (- Num In))
        (If (< Num 1.0e-12) (SetQ Num 0.0))
        (SetQ Num (+ (* Ft 12.0) In (* Num 6.25))
              Tot (+ Tot (* Sign Num))))))))
 (SetQ StrDef (RToS Tot))
 (Princ StrDef)
 (Princ)
)
