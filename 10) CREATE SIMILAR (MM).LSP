;;  MakeMore.LSP [command name: MM]
;;  Concept inspired by CADALYST July 2008  www.cadalyst.com  Tip 2298:
;;    SetAs.lsp -- Make More of the Same by Mosad H. Elewa
;;  Thoroughly rewritten from scratch, expanded & improved by Kent Cooper
;;  Last edited 21 February 2013
;
;;  To Make More of the same kind of entity as a selected object.
;;  Sets all appropriate properties (Layer, Color, Linetype, Linetype Scale, Lineweight, Thickness) to
;;    match selected object.  Warns User if any other than Layer are different from default, suggesting
;;    resetting when done; suggests resetting with MMR command, which is defined, but not invoked
;;    in case User wants to make more than one more matching object.
;;  Sets matching default values where possible [e.g. Text/Dimension/Mline style, Block name, Hatch
;;    pattern, etc.], and/or offers other options, depending on object type [e.g. offers default where entity
;;    data suggests a certain command for LWPolyline].
;;  Offers command choices whether or not entity data alone can suggest which command made object.
;;  Invokes the appropriate command to create the same kind of object.
;;  Notifies User if selected object is not one that routine can replicate, but still sets its properties.
;;  [Note:  Using Enter to recall the last command, immediately after drawing something via MM, will
;;    recall MM itself; it will not recall the command that MM invoked.]
;
(defun C:MMR (); = MakeMore Reset of system variable settings, if MM changed any
  (if mmsvvals (mapcar 'setvar mmsvnames mmsvvals))
  (princ)
); MMReset
;
(defun C:MM
  (/ *error* *esel *pcn *poly *hatch *ed cmde ent data typ P1 P2 P3 parpt
  eq42 cl I1 S1 R1 S2 I2 tj tj2 R2 thov R3 twov E1 tcont E2 tlist tbang pdm)
    ; Initial-capital variables are for Point/Integer/String/Real/Entity, to lessen quantity of
    ; variable names; most used in more than one section, sometimes for different purposes
;
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'cmdecho cmde)
    (princ)
  ); defun - *error*
;
  (defun *esel (); Entity SELection by User [verb-noun operation or multiple pre-selected objects]
    (while
      (not (setq ent (car (entsel "\nSelect object to Make More of the same: "))))
      (prompt "\nNothing selected:")
    ); while
  ); defun - *esel
;
  (defun *pcn (); = Prompt Command Name [display what command was invoked, without double first prompt]
    ; when command name is same as object type or Polyline sub-type
    (prompt
      (strcat
        "\n"
        (cond
          (S2 (strcase S2))
            ; LWPolyline, Leader, PolygonMesh, Region, 3Dsolid, Body subtypes, Divide/Measure for Point
          (typ); none-of-the-above
        ); cond
        "  "
      ); strcat
    ); prompt
  ); defun - *pcn
;
  (defun *poly (); shared between LWPolyline & 2D Polyline types
    (initget "PLine Rectangle POlygon Donut Cloud Boundary Sketch")
    (setq
      S2
        (getkword
          (strcat
            "\nPolyline Type [PLine/Rectangle/POlygon/Donut/Cloud/Boundary/Sketch] <"
            S1
            ">: "
          ); strcat
        ); getkword
      S2 (cond (S2) (S1)); use User entry, or default for Enter
    ); setq
    (cond; variety of Polyline
      ((= S2 "Rectangle")
        (*pcn)
        (command "_.rectangle" "w" (getvar 'plinewid));;;;; other options [Chamfer/Elevation/Fillet/Thickness]?
      ); Rectangle variety
      ((= S2 "POlygon")
        (setvar 'polysides (if (> I1 2) I1 4))
        (setvar 'cmdecho cmde)
        (command "_.polygon")
          (while (> (getvar 'cmdactive) 0) (command pause))
        (vlax-put (vlax-ename->vla-object (entlast)) 'ConstantWidth (getvar 'plinewid))
          ; Polygon doesn't honor width -- assign current [selected item's] width to new Polygon
      ); POlygon variety
      ((= S2 "Donut")
        (setq R1; donut diameter at centerline
          (distance (vlax-curve-getStartPoint ent) (vlax-curve-getPointAtParam ent 1))
        ); setq
        (setvar 'donutid (- R1 (getvar 'plinewid)))
        (setvar 'donutod (+ R1 (getvar 'plinewid)))
        (*pcn)
        (command "_.donut")
      ); Donut variety
      ((= S2 "Cloud")
        (prompt "\nREVCLOUD ")
        (if (= S1 "Cloud"); as determined for default offering
          (progn ; then
            (setq R1 (distance (vlax-curve-getStartPoint ent) (vlax-curve-getPointAtParam ent 1)))
              ; arc min/max lengths calculated from data; adjust multipliers as desired
            (command "_.revcloud" "_arc" (* R1 0.9) (* R1 1.1))
          ); progn
          (command "_.revcloud"); else - Cloud option chosen from other kind of selected Pline
        ); if
      ); Revcloud variety
      ((= S2 "Boundary")
        (setvar 'hpbound 1)
        (*pcn)
        (command "_.boundary")
      ); Boundary variety
      ((= S2 "Sketch")
        (setvar 'skpoly 1); Polyline variety
        (*pcn)
        (command "_.sketch")
      ); Sketch variety
      (T (prompt "\nPLINE ") (command "_.pline")); ordinary-PLine variety
    ); cond - variety of LWPoly
  ); defun - *poly
;
  (defun *hatch (); referred to from Hatch or Insert old-style Hatch object types
    (initget "Bhatch Hatch")
    (setq S1 (getkword "\nBhatch or Hatch [B/H] <B>: "))
    (if (= S1 "Hatch")
      (progn (prompt "\nHATCH ") (command "_.hatch"))
      (progn (initdia) (command "_.bhatch"))
    ); if - type of hatch command
  ); defun - *hatch
;
  (defun *ed (dxf); = Entity Data value associated with DXF code number
    (cdr (assoc dxf data))
  ); defun - *ed
;
  (vl-load-com)
  (setq cmde (getvar 'cmdecho))
  (setvar 'cmdecho 0)
    ; In commands without dialog box and where (command) function ends with command name,
    ; leaving CMDECHO on to display command name also displays initial prompt twice; left off
    ; to prevent duplication of prompt; added (*pcn) [see above] so User knows what command is.
;
  (cond ; OBJECT SELECTION
    ( (and
        (setq ss (ssget "_I")); something pre-selected
        (= (sslength ss) 1); only one object
      ); and
      (setq ent (ssname ss 0))
    ); first condition [single pre-selected object]
    ( (ssget "_I"); more than one object selected
      (sssetfirst nil); un-select multiple objects
      (*esel); User select
    ); second condition
    ((*esel)); User select [nothing pre-selected]
  ); cond
;
  (setq
    data (entget ent)
    typ (*ed 0)
  ); setq
  ; Warning & reset recommendation if any overrides not Bylayer or default:
  (if
    (or
      (vl-remove-if-not '(lambda (x) (member (car x) '(62 6 48 370))) data)
      (and (assoc 39 data) (/= (*ed 39) 0)); LWPolylines have thickness entry even if 0
    ); and
    (progn
      (alert
        (strcat
          "WARNING:  Recommend resetting\n"
          (if (assoc 62 data) "   Color to Bylayer\n" "")
          (if (assoc 6 data) "   Linetype to Bylayer\n" "")
          (if (assoc 48 data) "   Linetype scale to 1\n" "")
          (if (assoc 370 data) "   Lineweight to Bylayer\n" "")
          (if (and (assoc 39 data) (/= (*ed 39) 0)) "   Thickness to 0\n" "")
          "when finished.  MMR [= Make More Reset] command will restore"
          "\nall properties to their settings prior to the last instance of this alert."
        ); strcat
      ); alert
      (setq
        mmsvnames '(clayer cecolor celtype celtscale celweight thickness)
        mmsvvals (mapcar 'getvar mmsvnames)
          ; = System Variable VALueS for reset by MMR if used afterwards [not localized]
      ); setq
    ); progn
  ); if
;
  (setvar 'clayer (*ed 8))
  ; Set these all in case selected object's not same as current, whether override(s) or not:
  (setvar 'cecolor (cond ((assoc 62 data) (itoa (*ed 62))) ("BYLAYER")))
  (setvar 'celtype (cond ((*ed 6)) ("BYLAYER")))
  (setvar 'celtscale (cond ((*ed 48)) (1.0)))
  (setvar 'celweight (cond ((*ed 370)) (-1)))
  (setvar 'thickness (cond ((*ed 39)) (0)))
;
  (cond; begin OVERARCHING TEST for each object type
;
    ((wcmatch typ
      "LINE,XLINE,RAY,SOLID,3DFACE,ARC,CIRCLE,ELLIPSE,SPLINE,IMAGE,WIPEOUT,TOLERANCE")
      (*pcn)
      (command (strcat "_." typ))
    ); CATCH-ALL condition for object types whose command names are the same as their 'typ' values,
      ; and which don't need any other information extracted to set variables, nor have other complications
      ;;;;; pull IMAGE out and offer SUPERHATCH option, perhaps only if part of a Group?
      ;;;;; pull SOLID,3DFACE,WIPEOUT out and offer TEXTMASK option?
;
;|
;;;;;  To offer SKETCH option for Line, if desired, delete "LINE," from catch-all above, and
;;;;;  delete these two lines, semicolon-and-pipe line above, and pipe-and-semicolon line below
    ((= typ "LINE")
      (initget "Line Sketch")
      (if (= (getkword "\nCommand option [Line/Sketch] <L>: ") "Sketch")
        (progn ; then
          (setvar 'skpoly 0); Line variety
          (prompt "\nSKETCH ")
          (command "_.sketch")
        ); progn - Sketch option
        (progn ; else
          (*pcn)
          (command "_.line"); else - ordinary-Line variety
        ); progn
      ); if - variety of Line
    ); cond - Line object type
|;
;
    ((= typ "MLINE")
      (setvar 'cmljust (*ed 70))
      (setvar 'cmlscale (*ed 40))
      (setvar 'cmlstyle (*ed 2))
      (*pcn)
      (command "_.mline")
    ); Mline object type
;
    ((= typ "TRACE")
      (setq P1 (*ed 10) P2 (*ed 11) P3 (*ed 12))
      (setvar 'tracewid; default: perp. distance from 2nd corner to line between 1st and 3rd
        (distance; [if trace has been stretched and is no longer constant width, this will be off]
          P2
          (inters P1 P3 P2 (polar P2 (+ (angle P1 P3) (/ pi 2)) 1) nil)
        ); distance
      ); setvar
      (*pcn)
      (command "_.trace")
    ); Trace object type
;
    ((= typ "LWPOLYLINE")
      (if (= (getvar 'plinetype) 0) (setvar 'plinetype 2))
        ; in case set at 0 [old-style "heavy" 2D type]; value of 1 can remain [new ones still lightweight];
        ; assumes no desire to return it to 0 - add that resetting/option/recommendation, if desired
      (if (assoc 43 data); has global width
        (setvar 'plinewid (*ed 43)); then - match it
        (setvar 'plinewid 0); else - remove current width if non-zero
      ); if
      (defun parpt (par); find Point at Parameter for Polyline type tests
        (vlax-curve-getPointAtParam ent par)
      ); defun - parpt
      (defun eq42 (val / pdata); find whether *all* bulge factors [(assoc 42) entries] have specified value
        (setq pdata data)
        (while (equal (cdr (assoc 42 pdata)) val 1e-6); can be + or -
          (setq pdata (cdr (member (assoc 42 pdata) pdata))); remainder after this 42 entry
        ); while
        (not (assoc 42 pdata)); returns T if they were all equal [none left]
      ); defun - eq42
      (setq
        cl (vlax-curve-isClosed ent)
        I1 (*ed 90); number of vertices for type tests & to set 'polysides
        S1 ; LWPolyline-making command default
          (cond
            ( (and
                (= I1 4)
                cl
                (assoc 43 data); global width
                (eq42 0.0); all straight-line segments
                (equal (distance (parpt 0) (parpt 1)) (distance (parpt 2) (parpt 3)) 1e-8); opposite sides equal lengths
                (equal (distance (parpt 1) (parpt 2)) (distance (parpt 3) (parpt 0)) 1e-8)
                (equal (rem (abs (- (angle (parpt 0) (parpt 1)) (angle (parpt 1) (parpt 2)))) pi) (/ pi 2) 1e-8)
                  ; right angle first corner
              ); and
              "Rectangle"
            ); Rectangle condition
;;;;; Identifies only four-sided square-cornered Rectangles; Rectangle command has options independent
;;;;; of similar options for general drawing: [Chamfer/Elevation/Fillet/Thickness/Width].
;;;;; Polyline with 8 vertices, 2nd & 6th segments same length, 4th & 8th segments same length, odd-numbered
;;;;; ones all same length, could be Rectangle with Fillet/Chamfer option; if odd-numbered segments have
;;;;; (42 . 0.414214), Fillet [90-degree arc bulge factor]; if (eq42 0.0), Chamfer.
;;;;;  ***Don't know where those options are stored, or how to set them as defaults programmatically.***
            ( (and
                (> I1 2)
                cl
                (member '(43 . 0.0) data); global width = 0
                (eq42 0.0); all straight-line segments
                (equal ; first two and last two segments, at least, all same length
                  (setq R1 (distance (parpt 0) (parpt 1))); first segment length
                  (distance (parpt 1) (parpt 2)); second
                  1e-8
                ); equal
                (equal (distance (parpt (- I1 2)) (parpt (1- I1))) R1 1e-8); next-to-last
                (equal (distance (parpt (1- I1)) (parpt 0)) R1 1e-8); last
              ); and
              "POlygon"
            ); POlygon condition [does not check for equal angles]
            ( (and
                (= I1 2)
                cl
                (assoc 43 data); global width
                (or (eq42 1.0) (eq42 -1.0))
                  ; both full-semi-circle arc segments in same direction, CCW or CW
              ); and
              "Donut"
            ); Donut condition
            ( (and
                (assoc 43 data); global width
                (or
                  (and
                    cl
                    (or (eq42 0.520567) (eq42 -0.520567)); all Revcloud-type arc segments
                  ); and
                  (and
                    (not cl)
                    (setq data (reverse (cddr (reverse data))))
                      ; removes last (42) entry, which is 0 for open Revclouds.  Then:
                    (or (eq42 0.520567) (eq42 -0.520567)); all Revcloud-type remaining
                  ); and
                ); or
              ); and
              "Cloud"
            ); Cloud condition
            ("PLine"); none of the above [no default determination for Boundary or Sketch]
          ); cond & S1
      ); setq
      (*poly); subroutine shared with 2D Polyline type
    ); cond - LWPoly object type
;
    ((= typ "POLYLINE")
      (setq S2 (substr (cdr (assoc 100 (reverse data))) 5))
        ; later 100 value minus "AcDb" prefix
      (cond
        ((= S2 "3dPolyline") (prompt "\n3DPOLY ") (command "_.3dpoly"))
        ((= S2 "2dPolyline")
          (initget "Heavy Lightweight")
          (if
            (=
              (getkword
                "\nMatch heavy 2D type (other than for Rectangle/Cloud), or use Lightweight type? [H/L] <L>: "
              )
              "Heavy"
            ); =
            (progn; then - old-style "heavy" 2D type
              (setvar 'plinetype 0)
              (alert "Recommend resetting the PLINETYPE System\nVariable to 1 or 2 when finished.")
                ; but doesn't save it and reset it, in case User needs to make more than one of them
            ); progn
            (if (= (getvar 'plinetype) 0) (setvar 'plinetype 2)); else
              ; in case it was set at 0 [old-style "heavy" 2D type]; keep value if 1 [new ones still lightweight];
              ; assumes no desire to return it to 0 - add that resetting/option/recommendation, if desired
          ); if
          (setq S1 "PLine"); to offer as default in (*poly)
          (*poly); subroutine shared with LWPolyline type
        ); second condition - 2D Polyline type
        ((= S2 "PolygonMesh"); [couldn't find a way to differentiate types from entity data]
          (initget "3D 3DMesh Pface REvsurf RUlesurf Tabsurf")
          (setq S2 (getkword
            "\nPolygon Mesh command [3D/3DMesh/Pface/REvsurf/RUlesurf/Tabsurf] <3D>: "))
          (if (member S2 '(nil "3D")); user hit Enter or typed 3D
            (progn (load "3D") (C:3d)); then [loads in case not used before]
            (progn
              (*pcn)
              (command (strcat "_." S2)); else - other entered option
            ); progn
          ); if
        ); third condition - mesh types
      ); cond - variety of Polyline
    ); 3D/heavy 2D Polyline object type
;
    ((= typ "REGION")
      (initget "Objects Boundary Section")
      (setq S2 (getkword "Region source [Objects/Boundary/Section] <O>: "))
      (cond
        ((= S2 "Boundary")
          (setvar 'hpbound 0)
          (*pcn)
          (command "_.boundary")
        ); Boundary-defined region source
        ((= S2 "Section") (*pcn) (command "_.section"))
        (T (*pcn) (command "_.region"))
      ); cond - region source
    ); Region object type
;
    ((= typ "INSERT"); overall Block/Minsert/Xref/Metafile/Light/old-style Hatch category
      ;;;;; offer SUPERHATCH option, perhaps only if part of a Group?
      (setq S1 (*ed 2)); = insert-object name
      (cond
        ((= (substr S1 1 2) "*X"); identify old-style Hatch pattern
          (setq data (cddadr (assoc -3 (entget ent '("ACAD"))))); extended data [replaces regular entity data]
          (setvar 'hpname (*ed 1000))
          (setvar 'hpscale (*ed 1040))
          (setvar 'hpang (cdr (assoc 1040 (cddddr data)))); second 1040 = rotation in radians
          (if (= (getvar 'hpname) "_U"); User-defined
            (progn
              (setvar 'hpspace (*ed 1040))
              (setvar 'hpdouble (cdr (assoc 1070 (cddddr data)))); double-direction
            ); progn
          ); if
          (*hatch); subroutine shared with Hatch object type
        ); old-style Hatch variety
        ((assoc 1 (tblsearch "block" (*ed 2))); identify Xref
          (initdia)
          (command "_.xref")
        ); Xref variety
          ;;;;; XCLIP option?  identifiable from entity data?
        ((= (substr S1 1 3) "WMF"); identify Windows Metafile [if it hasn't been renamed]
          (command "_.wmfin")
        ); Metafile variety
        ((wcmatch S1 "direct,overhead,sh_spot"); [possibly only in certain overlay program(s), e.g. ADT]
          ; identify Light [assuming names not used otherwise]
          (command "_.light")
        ); Light variety
        (T; Minsert or ordinary Block [both need next line]
          (setvar 'insname S1)
          (if (= (cdr (assoc 100 (reverse data))) "AcDbMInsertBlock")
            ; identify Minsert by later 100 value
            (progn (prompt "\nMINSERT ") (command "_.minsert")); then
            (progn (initdia) (command "_.insert")); else - ordinary Block
              ;;;;; offer Divide & Measure options?
          ); if - Minsert or Block option
        ); Minsert/Block variety
      ); cond - variety of Insert
    ); Insert object type
;
    ((= typ "SHAPE")
      (setvar 'shpname (*ed 2))
      (*pcn)
      (command "_.shape")
    ); Shape object type
;
    ((= typ "HATCH")
      (setvar 'hpname (*ed 2)); hatch pattern
      (if (wcmatch (getvar 'hpname) "U,_USER")
        (progn
          (setvar 'hpspace (*ed 41))
          (setvar 'hpdouble (*ed 77))
        ); progn
      ); if
      (if (/= (getvar 'hpname) "SOLID"); pre-defined patterns other than Solid
        (progn
          (setvar 'hpscale (*ed 41))
          (setvar 'hpang (*ed 52))
        ); progn
      ); if
      (setvar 'hpassoc (*ed 97))
      (*hatch); subroutine shared with Insert old-style Hatch object type
    ); Hatch object type
;
    ((= typ "DIMENSION")
      (command "_.dimstyle" "r" (*ed 3))
      (setvar 'cecolor "bylayer"); Dimensions do not honor color overrides
      (setq
        I1 (boole 2 (*ed 70) 128)
          ; removes 128 caused by relocated text, incl. any DIM & RAD inboard of Circle/Arc
        R1 (*ed 50); dimension line rotation [used in only some types below]
      ); setq
      (setq data (cadr (assoc -3 (entget ent '("ACAD"))))); extended data [replaces regular entity data]
      (setvar 'dimse1 (if (member '(1070 . 75) data) (cdadr (member '(1070 . 75) data)) 0))
      (setvar 'dimse2 (if (member '(1070 . 76) data) (cdadr (member '(1070 . 76) data)) 0))
      (setvar 'dimsd1 (if (member '(1070 . 281) data) (cdadr (member '(1070 . 281) data)) 0))
      (setvar 'dimsd2 (if (member '(1070 . 282) data) (cdadr (member '(1070 . 282) data)) 0))
      ;;;;; matches suppression [if any] of Dimension & Extension lines only; could add more override matches
      ;;;;; match Obliquing?
      ;;;;; offer QDIM option?
      (cond
        ((= I1 35) (prompt "\nDIMDIAMETER\n") (command "_.dimdiameter"))
        ((= I1 36) (prompt "\nDIMRADIUS\n") (command "_.dimradius"))
        (T ; continue and baseline options available for all but diameter and radius
          (initget "New Continue Baseline")
          (setq S1 (getkword "Dimension basis [New/Continue/Baseline] <N>: "))
          (cond
            ((= S1 "Continue") (prompt "\nDIMCONTINUE\n") (command "_.dimcontinue" "s" ent))
            ((= S1 "Baseline") (prompt "\nDIMBASELINE\n") (command "_.dimbaseline" "s" ent))
            ((= I1 33) (prompt "\nDIMALIGNED\n") (command "_.dimaligned"))
            ((member I1 '(34 37)) (prompt "\nDIMANGULAR\n") (command "_.dimangular"))
            ((member I1 '(38 102)) (prompt "\nDIMORDINATE\n") (command "_.dimordinate")); 38 Ydatum, 102 Xdatum
            ((= I1 32)
              (if (/= 0 R1 (/ pi 2)); other than 0 or 90 degrees
                (progn (prompt "\nDIMROTATED\n") (command "_.dimrotated" (* (/ R1 pi) 180))); then
                (progn (prompt "\nDIMLINEAR\n") (command "_.dimlinear")); else
              ); if
            ); Rotated varieties
          ); cond - dimension basis option
        ); continuable types
      ); cond - variety of Dimension
    ); Dimension object type
;
    ((= typ "LEADER")
      (command "_.dimstyle" "r" (*ed 3))
      (setvar 'cecolor "BYLAYER"); leaders don't honor color overrides
      (setvar 'celtscale 1.0); leaders don't honor linetype scale [even though they do honor linetypes]
      (initget "Leader Qleader")
      (setq S2 (getkword "Leader or Qleader [L/Q] <L>: "))
      (if (= S2 "Qleader")
        (progn (*pcn) (command "_.qleader")); then
        (progn ; else
          (setvar 'cmdecho cmde); for point prompts
          (command "_.leader" pause pause "f"
            (if (= (*ed 72) 0) "ST" "S") ; STraight vs. Spline format
          ); command
        ); progn
      ); if
    ); Leader object type
;
    ((= typ "TEXT")
      (setvar 'textstyle (setq S1 (*ed 7)))
      (setvar 'textsize (setq R1 (*ed 40)))
      (setq
        S2 (angtos (*ed 50)); rotation - current angular units
        P1 (*ed 10); left end of baseline [insertion point if Left justified]
        P2 (*ed 11); insertion point [other than Left justified]
        I1 (*ed 72); justification code
        I2 (*ed 73); justification code
        tj (nth I2 '(0 "B" "M" "T"))
        tj2 (nth I1 '("L" "C" "R" "A" "M" "F"))
        tj (if (= I2 0) tj2 (strcat tj tj2)); if single-letter justification, 72 only; otherwise, combination of 73+72
        R2 (cdr (assoc 40 (tblsearch "style" S1))); Style's height, to check for override to fixed-height Style
        thov (and (/= R2 0) (/= R1 R2) (/= tj "A"))
          ; T if Style is fixed-height, but selected text has different Height OVerride, not from Aligned justification
        R3 (*ed 41); width factor
        twov (and (/= R3 (cdr (assoc 41 (tblsearch "style" S1)))) (/= tj "F"))
          ; T if selected text has Width OVerride different from Style's width factor, not from Fit justification
      ); setq
        ; [NOTE: height and/or width overrides must be imposed *after* end of command; new Text entities
        ; will appear at standard height/width during command, and will have overrides imposed afterwards;
        ; if MM is followed by Text or Dtext for more of the same, such overrides will not be imposed -- must
        ; use Match Properties, or use MM again instead of Text/Dtext command.]
      (if (or thov twov) (setq E1 (entlast)))
        ; selected has either non-Aligned-based height override or non-Fit-based width
        ; override -- put marker at last entity, to gather all subsequent entities later
      (*pcn)
      (initget "New Continuation")
      (setq tcont (getkword "\nNew insertion point or Continuation of selected text [N/C] <N>: "))
      (setvar 'cmdecho cmde)
      (cond; - Text insertion-point New-vs.-Continuation choice
        ((member tcont '(nil "New")); NEW user-supplied insertion point [Enter-default or typed]
          (cond; Aligned vs. non-fixed- vs. fixed-height Style
            ((= tj "A") (command "_.dtext" "j" "a" pause pause)); Aligned [no height (fixed or not) or rotation]
            ((= (cdr (assoc 40 (tblsearch "style" S1))) 0); NON-fixed-height Style
              (cond; justification
                ((= tj "F") (command "_.dtext" "j" "f" pause pause "")); Fit [no rotation], uses selected-Text height
                ((= I1 I2 0) (command "_.dtext" pause "" S2)); Plain-left
                ((command "_.dtext" "j" tj pause "" S2)); other justifications
              ); cond - justification sub-category
            ); NON-fixed-height Style Text category
            ( ; FIXED-height Style
              (cond; justification
                ((= tj "F") (command "_.dtext" "j" tj pause pause)); Fit [no rotation]
                ((= I1 I2 0) (command "_.dtext" pause S2)); Plain-left
                ((command "_.dtext" "j" tj pause S2)); other justifications
              ); cond - justification sub-category
            ); FIXED-height Style Text category
          ); cond - non-fixed vs. fixed determination
        ); cond - New user-supplied insertion point option
        ( ; CONTINUATION of selected text;
          (cond; Aligned vs. non-fixed- vs. fixed-height Style
            ((= tj "A"); Aligned
              ; haven't found a way to continue via text Enter to drop down and keep Aligned justification
              (setq
                R2 (* R1 1.6)
                  ; approximate line spacing [varies by font]; Aligned overrides height, so line spacing not critical
                tbang (- (*ed 50) (/ pi 2)); Text-Below ANGle
              ); setq
              (command "_.dtext" "j" "a" (polar P1 tbang R2) (polar P2 tbang R2))
            ); Aligned condition
            ((= (cdr (assoc 40 (tblsearch "style" S1))) 0); NON-fixed-height Style
              (cond; justification
                ((= tj "F") (command "_.text" "j" "f" P1 P2 "" "" "_.dtext" "")); Fit [no rotation], uses selected-Text height
                ((= I1 I2 0) (command "_.text" P1 "" S2 "" "_.dtext" "")); Plain-left
                ((command "_.text" "j" tj P2 "" S2 "" "_.dtext" "")); other justifications
              ); cond - justification sub-category
            ); NON-fixed-height Style Text category
            ( ; FIXED-height Style
              (cond; justification
                ((= tj "F") (command "_.text" "j" tj P1 P2 "" "_.dtext" "")); Fit [no rotation]
                ((= I1 I2 0) (command "_.text" P1 S2 "" "_.dtext" "")); Plain-left
                ((command "_.text" "j" tj P2 S2 "" "_.dtext" "")); other justifications
              ); cond - justification sub-category
            ); FIXED-height Style Text category
          ); cond - non-fixed vs. fixed determination
        ); cond - Continuation of selected Text option
      ); cond - Text insertion-point New-vs.-Continuation choice
      (if E1; if either or both height/width override(s)
        (progn ; make list of new Dtext command's new entities [can be more than one]
          (while (> (getvar 'cmdactive) 0) (command pause)); wait for completion of dtext command
          (setq E2 (entnext E1) tlist (list E2)); start list with first of new Text entities
          (while (setq E2 (entnext E2)) (setq tlist (cons E2 tlist))); put remainder into list
          (foreach x tlist
            (setq data (entget x))
            (if thov (setq data (subst (cons 40 R1) (assoc 40 data) data))); impose height override
              ; [NOTE: if Text of fixed-height Style has height override, must be imposed *after*
              ; command; if doing sequential lines using Enter between, individual Text entity
              ; heights will have height override imposed to match selected Text, but line *spacing*
              ; will be as for height in Style definition, *not* adjusted for imposed override height.]
            (if twov (setq data (subst (cons 41 R3) (assoc 41 data) data))); impose width override
            (entmod data)
            (entupd x)
          ); foreach
        ); progn
      ); if
    ); Text object type
;
    ((= typ "MTEXT")
      (setq
        S1; justification text
          (nth (*ed 71) '(0 "TL" "TC" "TR" "ML" "MC" "MR" "BL" "BC" "BR"))
        S2 (angtos (*ed 50)); rotation - current angular units
        R1 (cdr (assoc 40 (tblsearch "style" (*ed 7)))); Style's height
      ); setq
      (setvar 'textstyle (*ed 7)); must do outside Mtext command, or line spacings revert to defaults
      (setvar 'textsize (*ed 40))
      (setvar 'tspacetype (*ed 73))
      (setvar 'tspacefac (*ed 44))
      (setvar 'cmdecho cmde); for first-corner prompt
      (initdia)
      (if (/= R1 (*ed 40)); Mtext of fixed-height style has different height override
        (command "_.mtext" pause "h" (*ed 40) "j" S1 "r" S2); ask for height
        (command "_.mtext" pause "j" S1 "r" S2); don't
      ); if
    ); Mtext object type
;
    ((= typ "RTEXT")
      (command
        "_.text" "_style" "standard" (getvar 'viewctr) 1 (angtos (*ed 50)) "delete"
          ; Assumes Standard text style has 0 height; rotation is in current angular units.
          ; Draws temporary piece of text to make selected object's angle current -- apparently
          ; no System Variable to set, and seems to need to be set prior to Rtext command.
        "_.erase" "_last" ""
      ); command
      (setvar 'textstyle (*ed 7))
      (setvar 'textsize (*ed 40))
      (C:rtext)
    ); Rtext object type
;
    ((= typ "ARCALIGNEDTEXT")
      (setvar 'textstyle (*ed 7))
      (command "_.arctext")
    ); Arctext object type
;
    ((= typ "POINT")
      (*pcn)
      (initget 1 "Divide Measure")
      (setq S2 (getpoint "\nSpecify a point or [Divide/Measure]: "))
      (cond
        ((listp S2) ; picked a location
          (command "_.point" S2)
          (while (setq P1 (getpoint "\nSpecify a point: ")) (command "_.point" P1))
        ); picked-location condition
        (T (*pcn) (command (strcat "_." S2))); typed D/M
      ); cond
    ); Point object type
;
    ((= typ "3DSOLID"); [couldn't figure a way to distinguish types from entity data]
      (initget "Box Wedge CYlinder COne Sphere Torus Extrude Revolve Union Intersect")
      (setq S2 (getkword
        "\n3D Solid command [Box/Wedge/CYlinder/COne/Sphere/Torus/Extrude/Revolve/Union/Intersect] <B>: "))
      (if (not S2); user hit Enter
        (progn (prompt "\nBOX ") (command "_.box")); then - use default
        (progn (*pcn) (command (strcat "_." S2))); else - entered option
      ); if
    ); 3DSolid object type
;
    ((= typ "ATTDEF") (initdia) (command "_.attdef"))
;
    ((= typ "VIEWPORT") (initdia) (command "_.vports"))
;
    ((= typ "BODY")
      (initget "Acisin Explode")
      (setq S2 (getkword "\nAcisIn/Explode 3D solid [A/E]? "))
      (*pcn)
      (command (strcat "_." S2))
    ); Body object type
;
    ((= typ "PLANT") (C:lsnew)); [possibly only in certain overlay program(s), e.g. ADT]
;
    ; other possible up-to-2004 entity types/commands: 3DSIN?
    ; other newer-than-2004 entity types/commands: 3DDWF? DGNATTACH/DGNIMPORT?
    ; DIMJOGGED/DIMJOGLINE? DISTANTLIGHT? DWFATTACH? FIELD? HELIX? IMPRESSION?
    ; IMPRINT? JOGSECTION? LOFT? MARKUP? MESH? MLEADER? PDFATTACH? PLANESURF?
    ; POINTLIGHT? POLYSOLID? QVDRAWING? SECTIONPLANE? SPOTLIGHT? TABLE? TINSERT?
    ; DYNAMIC BLOCK?
;
    ( (alert ; none of the above object types
        (strcat
          "Routine is not yet set up to Make More of the "
          typ
          " object type,\nbut has set current Properties to match it."
        ); strcat
      ); alert
    ); none-of-the-above condition
;
  ); cond - OVERARCHING TEST for each object type
;
  (setvar 'cmdecho cmde)
  (princ)
); defun - C:MM
(prompt "\nType MM to Make More the same as an existing object.")
; MakeMore.lsp
