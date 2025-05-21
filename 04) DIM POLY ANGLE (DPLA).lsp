;|
DimPolyAngles.lsp [command name: DPLA]
to Dimension all Angles between adjacent Polyline line segments.
Works on Polylines of any variety, open or closed.
Dimensions only angles between adjacent line segments, ignoring arc segments.
Uses all current Dimension settings.  Position of each Dimension is six times
  Dimension text height from vertex, which may give undesirable results with
  too-short segments [see EDIT instruction below to modify].
**** SO FAR [most can be dealt with if needed]:
  1) does not modify UCS to Dimension those not lying in or parallel to the
      current Coordinate System;
  2) does not adjust for non-planar 3D Polylines [dimensions as if flat];
  3) does not work around coincident vertices;
  4) does not work in Paper Space on Polylines in Model Space Viewports;
  5) Dimensions smaller angle, i.e. outside Polyline at concave bends;
  6) Dimension the start/end-vertex angle of a Polyline that starts and ends
      at the same place but is not "closed" by AutoCAD's definition;
  7) does not force appropriate Layer, Dimension Style or any dimension
      variables such as suppression state of extension lines, etc.;
  8) if changes as in 7) are made, needs *error* handler to reset;
  9) does not turn off command echoing, blips, etc.;
 10) does not create Undo begin/end "wrapper." ****
Kent Cooper, 14 July 2016
|;
(vl-load-com)
(defun C:DPLA ; = Dimension PolyLine Angles
  (/ plsel pl pldata endpar styht ref pt1 pt2 pt3 pt4 pt5)
  (while
    (not
      (and
        (setq plsel (entsel "\nSelect Polyline: ")); nil on Enter or miss
        (setq
          pl (car plsel); entity name
          pldata (entget pl)
        ); setq
        (wcmatch (cdr (assoc 0 pldata)) "*POLYLINE")
        (= (logand (cdr (assoc 70 pldata)) 80) 0)
          ;; not mesh [16 (polygon mesh) + 64 (polyface mesh)]
      ); and
    ); not
    (prompt "\nNothing selected, or not a Polyline.")
  ); while
  (if plsel
    (progn ; then
      (setq
        endpar (vlax-curve-getEndParam pl)
        ref (1+ endpar)
        styht (cdr (assoc 40 (tblsearch "style" (getvar 'dimtxsty))))
          ; height of text style in current dimension style
      ); setq
      (if (= styht 0.0) (setq styht (* (getvar 'dimtxt) (getvar 'dimscale))))
        ; if above is non-fixed-height
      (repeat
        (- (fix endpar) (if (vlax-curve-isClosed pl) 0 1)); number of [possible] angles
        (setq
          pt1 (vlax-curve-getPointAtParam pl (setq ref (1- ref))); [starts at downstream end]
          pt2 (vlax-curve-getPointAtParam pl (- ref 0.5)); preceding segment midpoint
          pt3 (vlax-curve-getPointAtParam pl (1- ref)); preceding vertex [angle to be Dimensioned]
          pt4 (vlax-curve-getPointAtParam pl (if (> ref 1) (- ref 1.5) (- endpar 0.5)))
            ; two midpoints before, or when Dimensioning start/end-vertex angle, last midpoint
          pt5 (vlax-curve-getPointAtParam pl (if (> ref 1) (- ref 2) (1- endpar)))
            ; two vertices before, or when Dimensioning start/end-vertex angle, back one from end
        ); setq
        (if
          (and ; both line segments?
            (equal (angle pt1 pt2) (angle pt2 pt3) 1e-8); segment ending at 'ref' location
            (equal (angle pt3 pt4) (angle pt4 pt5) 1e-8); segment before that
          ); and
          (command "_.dimangular" ; then
            "" "_none" pt3 "_none" pt2 "_none" pt4 ; points
            "_none"
              (polar
                pt3
                (angle pt3 (mapcar '/ (mapcar '+ pt2 pt4) '(2 2 2)))
                  ; tried obvious (angle pt3 pt2), but that sometimes puts it on the "wrong" side,
                  ;   and thus sometimes wrong angle measure [reason for having it do smaller
                  ;   angle, for consistency -- at least always internal at convex bends this way]
                (* styht 6); <---EDIT multiplier as desired [smaller = closer to corner, but results
                  ; greatly affected by text width factor, units/precision, particular angles, etc.]
              ); polar
          ); command
        ); if
      ); repeat
    ); progn [then]
  ); if
  (princ)
); defun -- DPLA

(prompt "\nType DPLA to Dimension Polyline Angles.")