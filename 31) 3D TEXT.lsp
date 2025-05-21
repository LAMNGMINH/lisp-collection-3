;;;CADALYST 02/10  Tip 2081-update  3DText.lsp  3DText  (c) 2010 Aaron Werning


;3DText.LSP     Routine to create and modify 3D Text                     
  ;3DText.LSP Version 1.4                                                
  ;(C) Copyright 2005 by Aaron Werning                                   
  ;E-mail: aaron_werning@yahoo.com                                       
;;----------------------------------------------------------------------;
  ;Rev. 1.1 - Jan 16 2006 - Fixed base letter blocks to be created and   
  ;                         inserted on layer "0" so that the 3D Text    
  ;                         block can be set to different layers after   
  ;                         initial creation.                            
  ;Rev. 1.2 - Mar 15 2006 - Fixed text block purge to account for 3D text
  ;                         nested inside other blocks                   
  ;                       - Added solid hatch to top surface of 3D Text  
  ;                         to make it more visible.			 
  ;Rev. 1.3 - Mar 15 2007 - Fixed base letter blocks to be color by      
  ;                         "ByBlock" so that the text can be different  
  ;                         colors independant of the layer.             
  ;Rev. 1.4 - Aug 11 2008 - Added functionality in the set_TrueColor     
  ;                         subroutine to work with all versions of ACAD 
  ;                       - Added text edit dialog so that later versions
  ;                         of ACAD function the same as earlier versions
;;----------------------------------------------------------------------;
  ;THIS PROGRAM IS PROVIDED "AS IS" AND WITH ALL FAULTS.  AARON WERNING  
  ;SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR     
  ;FITNESS FOR A PARTICULAR USE.  AARON WERNING DOES NOT WARRANT THAT    
  ;THE OPERATION OF THIS PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.    
;;----------------------------------------------------------------------;
  ;Program Notes:                                                        
  ; - 3DText.lsp makes a block of every letter (on demand) that is 1 unit
  ;   tall, 1 unit wide and 1 unit deep.  When creating the 3D text      
  ;   string each letter is inserted with the appropriate X,Y,Z scale.   
  ;   This minimizes file size due to extruded blocks.  If every letter  
  ;   (small and uppercase), special characters & fractions are inserted 
  ;   then the file size will only go up by a maximum of 2.5 MB for      
  ;   AutoCAD ver. 2000 file format (likely smaller for later versions)  
  ;                                                                      
  ; - 3DText.lsp has the ability to convert a DTEXT string into 3D Text  
  ;                                                                      
  ; - 3DText.lsp has the ability to edit/modify an existing piece of 3D  
  ;   text (i.e text string, justification, width, depth, etc.)          
  ;                                                                      
  ; - 3DText.lsp has the ability to fit the 3D text to a curved surface  
  ;   such as the face of a verticle cylinder (handy for labeling        
  ;   equipment with rounded surfaces such as tanks and vessels)         
  ;                                                                      
  ; - This routine can be easily called from a double-click reactor in   
  ;   order to assist easy 3D text editing.  Due to the necessity of     
  ;   redefining AutoCADs double-click reactor in order to obtain this   
  ;   functionality it is up to you to contact Aaron Werning so that he  
  ;   can provide you with the correct code for your version of AutoCAD  
  ;                                                                      
  ; - %% Codes can be used for special characters and fractions:         
  ;      %%D --> Degree Symbol (temperature)                             
  ;      %%P --> Plus/Minus Symbol                                       
  ;      %%C --> Diameter Symbol                                         
  ;      %%130 --> Centerline Symbol                                     
  ;      %%141 --> 1/16                                                  
  ;      %%142 --> 1/8                                                   
  ;      %%143 --> 3/16                                                  
  ;      %%144 --> 1/4                                                   
  ;      %%145 --> 5/16                                                  
  ;      %%146 --> 3/8                                                   
  ;      %%147 --> 7/16                                                  
  ;      %%148 --> 1/2                                                   
  ;      %%149 --> 9/16                                                  
  ;      %%150 --> 5/8                                                   
  ;      %%151 --> 11/16                                                 
  ;      %%152 --> 3/4                                                   
  ;      %%153 --> 13/16                                                 
  ;      %%154 --> 7/8                                                   
  ;      %%155 --> 15/16                                                 
;;----------------------------------------------------------------------;
(vl-load-com)


(defun C:3DText (/ option)
  (princ "\n")
  (while (not option)
    (initget 1 "New Convert")
    (setq option (entsel "\rSelect 3D Text to modify or [New/Convert]: "));select text to modify
    (if (and option
             (or (= 'LIST (type option))
                 (= 'STR (type option))))
      (if (and (= 'LIST (type option))
               (= 'ENAME (type (setq option (car option)))))
        (progn
          (setq option (vlax-ename->vla-object option))
          (if (not (and (= "AcDbBlockReference" (vla-get-objectname option))
                        (= "3D_TEXT_" (substr (vla-get-name option) 1 8))))
            (setq option nil)
          )
        )
      )
      (setq option nil)
    )
  )
  (make_3d_txt option nil)
)

;function to make 3D text based on the options selected
(defun make_3d_txt (option1	;option to 1)modify existing 3D test 2)create new 3D text or 3)convert text to 3D Text
                    reactor	;used for double-click reactor to double-click on text to edit (Ask Aaron Werning for code)
                            / olderr oldecho document utility activespace zeropoint cnt 3dt_blks
                              small_case option1 option2 origin txtht xvec width justify dwgname
                              depth textobj 3d_trash font font_name scale letter_ip letter yvec
                              letter_lst letter_width letter_plines case block space curve mstr_blk_name
                              last_letter test temp i 3dtxt layer text blkname chr_name osmode cur_ltr_rev letter_rev)

  ;--Begin Set-Up-----------------------------------------------------------------------
  (setq olderr *error*						;Store old error handler
        oldecho (getvar "cmdecho")
        osmode (getvar "osmode")
        document (vla-get-activedocument (vlax-get-acad-object))
        utility (vla-get-utility document)
        activespace (vla-get-block (vla-get-activelayout document))
        zeropoint (vlax-3d-point '(0.0 0.0 0.0))
        dwgname (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))
        mstr_blk_name (strcat "3D_TEXT_" dwgname "_")
        font_name "ARIAL"
        cur_ltr_rev "1.3"	;used to redefind letter blocks in existing drawings
        cnt 1
        small_case '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                     "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (vl-cmdf "._ucs" "d" "$3D_Text_UCS$" "._ucs" "s" "$3D_Text_UCS$")
  (setvar "cmdecho" 0)						;Run Program silently
  (vla-StartUndoMark document)					;Begin undo grouping
  ;--End Set-Up
  
  ;--Begin Error Handler------------------------------------------------------------------------
  (defun *error* (msg)
    (setq *error* olderr)               			;Restore old *error* handler
    (if (/= msg "Function cancelled")
      (prompt (strcat "\nError: " msg))				;Display error and message
    )
    (vla-EndUndoMark document)					;end undo grouping and undo everything
    (vl-cmdf "UNDO" "")
    (princ)
  );--End Error Handler

  ;--Begin Main Programming---------------------------------------------------------------------------
  (cond
    ;IF CREATING NEW 3D TEXT
    ((= option1 "New")
      (while (not (and origin (listp origin)))
        (initget 15 "Justify Curve")
        (setq origin (getpoint "\nSpecify start point of text or [Justify/Curve]: "))
        (if (= origin "Justify")
          (progn
            (initget 1 "TL TC TR ML MC MR BL BC BR")
            (setq justify (getkword "\nEnter an option [TL/TC/TR/ML/MC/MR/BL/BC/BR]: ")
                  origin nil)
          )
        )
        (if (= origin "Curve")
          (progn
            (initget 1)
            (setq curve_rad (getdist "\nEnter radius of curved surface for molded text: ")
                  origin nil)
            (if (equal 0.0 curve_rad 0.001)(setq curve_rad nil))
          )
        )
      )
      (initget 5)
      (setq layer (GetVar "clayer")
            justify (if justify (get_justify justify)(get_justify "BL"))
            txtht (getdist origin "\nSpecify height: ")
            txtrt (getangle origin "\nSpecify rotation angle of text <0>: ")
            width 0.9)
      (if (not txtrt)(setq txtrt 0.0))				;if not rotation default to
      (setq xvec (trans (polar origin txtrt 1) 1 0)
            yvec (trans (polar origin (+ txtrt (/ pi 2)) 1) 1 0)
            origin (trans origin 1 0))
      (vl-cmdf "ucs" "3" (trans origin 0 1)(trans xvec 0 1)(trans yvec 0 1) "ucs" "y" "90")
      (initget 1)
      (setq depth (getdist '(0 0 0) "\nSpecify Depth of 3D Text: "))
      (setq text (custom_Text_edit nil))
    )
    ;IF CONVERTING TEXT TO 3D TEXT
    ((= option1 "Convert")
      (princ "\n")
      (while (not textobj)
        (initget 1)
        (setq textobj (entsel "\rSelect Text to Convert to 3d: "));select text to modify
        (if (and textobj
                 (= "AcDbText" (vla-get-objectname (vlax-ename->vla-object (car textobj)))))
          (setq textobj (vlax-ename->vla-object (car textobj)))
          (setq textobj nil)
        )
      )
      (setq layer (vla-get-layer textobj)
            txtht (vla-get-height textobj)			;obtain text height
            txtrt (vla-get-rotation textobj)			;obtain text rotation
            width (vla-get-scalefactor textobj)			;obtain text width
            text (vla-get-textstring textobj)
            justify (get_justify textobj)
            origin (if (= 12 (car justify))
                     (vla-get-InsertionPoint textobj)
                     (vla-get-TextAlignmentPoint textobj))
            origin (vlax-safearray->list
                     (vlax-variant-value origin)))	;obtain text insertion point
      (vl-cmdf "ucs" "ob" (vlax-vla-object->ename textobj))
      (setq temp (trans origin 0 1)
            xvec (trans (cons (1+ (car temp))(cdr temp)) 1 0)
            yvec (trans (list (car temp)(1+ (cadr temp))(caddr temp)) 1 0))
      (vl-cmdf "ucs" "y" "90")
      (initget 1)
      (setq depth (getdist '(0 0 0) "\nSpecify Depth of 3D Text: "))
      (vla-delete textobj)
    )
    ;IF EDITING 3D TEXT
    (T								;if modifying text
      (setq layer (vla-get-layer option1))			;read layer of text
      (setq origin (xdata "get" option1 "origin" nil "$3D_TEXT_APP$")	;obtain text insertion point
            txtht (xdata "get" option1 "height" nil "$3D_TEXT_APP$")	;obtain text height
            xvec (xdata "get" option1 "xvec" nil "$3D_TEXT_APP$")	;obtain text rotation
            yvec  (xdata "get" option1 "yvec" nil "$3D_TEXT_APP$")
            width (xdata "get" option1 "width" nil "$3D_TEXT_APP$")	;obtain text width
            text  (xdata "get" option1 "text" nil "$3D_TEXT_APP$")
            depth (xdata "get" option1 "depth" nil "$3D_TEXT_APP$")
            curve_rad (xdata "get" option1 "curve" nil "$3D_TEXT_APP$")
            justify (get_justify (xdata "get" option1 "justify" nil "$3D_TEXT_APP$")))
      (initget "Height Width Depth Justify Text Curve")
      (if (not reactor)
        (setq option2 (getkword "\nHeight/Width/Depth/Justify/Curve/<Text>: "));what do you want to modify
      )
      (cond
        ((= option2 "Height")	 				;if height
          (setq temp txtht)
          (initget 5)
          (setq txtht (getdist (strcat "\nEnter New Height <" (rtos txtht 4 5) ">: ")))
          (if (not txtht)(setq txtht temp))
        )							;get new height
        ((= option2 "Width")	 				;if width
          (setq temp width)
          (initget 5)
          (setq width (getdist (strcat "\nEnter New Width <" (rtos width 2 3) ">: ")))
          (if (not width)(setq width temp))
        )							;get new width
        ((= option2 "Depth")	 				;if depth
          (setq temp depth)
          (vl-cmdf "ucs" "3" (trans origin 0 1)(trans xvec 0 1)(trans yvec 0 1) "ucs" "y" "90")
          (initget 1)
          (setq depth (getdist '(0 0 0) (strcat "\nEnter New Depth <" (rtos depth 4 5) ">: ")))
          (if (not depth)(setq depth temp))
        )							;get new depth
        ((= option2 "Justify")
          (initget 1 "TL TC TR ML MC MR BL BC BR")
          (setq justify (get_justify (getkword "\nEnter an option [TL/TC/TR/ML/MC/MR/BL/BC/BR]: ")))
        )
        ((= option2 "Curve")
          (initget 1)
          (setq temp curve_rad
                curve_rad (getdist (strcat "\nEnter new radius of curved text <" (if curve_rad
                                                                                   (rtos curve_rad 4 5)
                                                                                   "0"
                                                                                 )">: ")))
          (if (not curve_rad)(setq curve_rad temp))
          (if (equal 0.0 curve_rad 0.001)(setq curve_rad nil))
        )
        (T			 				;if Text
	  (setq temp (custom_Text_edit text))
          (if (= temp text)
            (setq text nil)
            (setq text temp)
          )
        )
      )
    )
  )
  (if (and text
           (/= text ""))
    (progn
      (if (= 'VLA-OBJECT (type option1))
        (vla-delete option1)
      )
      ;CHECK FOR UNUSED BLOCK NAMES AND PURGE THEM
      (vlax-for temp (vla-get-blocks document)
        (if (= mstr_blk_name (substr (vla-get-name temp) 1 8))
          (setq 3dt_blks (append 3dt_blks (list (vla-get-name temp))))))
      (foreach temp (ss->lst (ssget "X" (list (cons 2 (strcat mstr_blk_name "*")))))
        (if (member (cdr (assoc 2 (entget temp))) 3dt_blks)
          (setq 3dt_blks (vl-remove (cdr (assoc 2 (entget temp))) 3dt_blks))))
      (if 3dt_blks
        (vlax-for block (vla-get-blocks document)
          (if (and (= ':vlax-false (vla-get-islayout block))
                   (= ':vlax-false (vla-get-isxref block)))
            (vlax-for temp block
              (if (and (= "ACDBBLOCKREFERENCE" (strcase (vla-get-objectname temp)))
                       (= mstr_blk_name (substr (vla-get-name temp) 1 8)))
                (setq 3dt_blks (vl-remove (vla-get-name temp) 3dt_blks)))))))
      (if 3dt_blks
        (vlax-for temp (vla-get-blocks document)
          (if (member (vla-get-name temp) 3dt_blks)
            (vla-delete temp))))
      ;OBTAIN NEXT AVAILIABLE SEQUENTIAL BLOCK NAME
      (setq i 0 test nil)
      (while (= test nil)					;while a new unique ID# has not be found
        (if (tblsearch "BLOCK" (strcat mstr_blk_name (itoa i)))	;step through each number until a unique ID# has been found
          (setq i (1+ i))
          (setq blkname (strcat mstr_blk_name (itoa i))		;save block name with new unique ID#
                test t)
        )
      )
      (vl-cmdf "ucs" "")
      ;CREATE NEW TEXT BLOCK
      (setq 3dt_block (vla-add (vla-get-blocks document) zeropoint blkname)
            font (load_font font_name)
            scale (* txtht width)
            letter_pos 0
            letter_lst '((" " 0.1 nil)))
      ;BUILD A DATA LIST OF EACH LETTER
      (while (/= "" (substr text cnt 1))
        (setq letter_lst
               (append letter_lst
                       (list (assoc (fix_chr (substr text cnt 1)) font)))
              cnt (1+ cnt))
      )
      ;COUNT THE LENGTH OF THE TEXT STRING
      (setq text_len (- (apply '+ (mapcar '(lambda (x)(* scale (cadr x))) letter_lst)) 0.2))
      ;SPECIFY STARTING POINT FOR TEXT INSERTION
      (if curve_rad
        (setq letter_ip (list 0.0
                              (* txtht (cadadr justify))
                              depth)
              start_ang (/ (* text_len (caadr justify)) curve_rad))
        (setq letter_ip (list (* text_len (caadr justify))
                              (* txtht (cadadr justify))
                              depth))
      )
      ;STEP THROUGH EACH LETTER IN THE TEXT
      (foreach letter letter_lst
        (setq letter_width (cadr letter)
              space (* scale letter_width)
              letter_plines (caddr letter)
              case (if (member (car letter) small_case) "_" "_CAP_")
              chr_name (strcat "3D_LETR_" font_name case (car letter)))
        ;GET THE NEXT LETTER INSERTION POINT
        (if curve_rad
          (setq letter_ip (list (* -1 (/ space 2))
                                (cadr letter_ip)
                                depth))
        )
        (if (/= " " (car letter))
          (progn
            ;IF THE LETTER BLOCK HAS NOT BEEN CREATED THEN DO IT
            (if (not (tblsearch "BLOCK" chr_name))
              (if (= 'STR (type (caar letter_plines)))
                (setq block (make_frac letter_plines chr_name))
                (setq block (make_block letter_plines chr_name nil))
              )
              (setq block (collect_search (vla-get-blocks document) 'vla-get-name chr_name)
                    letter_rev (xdata "get" block "letter_rev" nil "$3D_TEXT_APP$"))
            )
            (if (/= letter_rev cur_ltr_rev)
              (make_block letter_plines chr_name block)
            )
            ;INSERT THE LETTER
            (setq letter
              (vla-insertblock
                3dt_block
                (list->safearray letter_ip)
                chr_name scale txtht
                (if (equal 0.0 depth 0.0001)
                  0.001
                  depth
                )
                0
              )
            )
            (vla-put-layer letter "0")
            (set_TrueColor letter "0")
            ;IF CURVING TEXT TO SURFACE THEN 3D ROTATE LETTER INTO PLACE
            (if curve_rad
              (vla-Rotate3D
                letter
                (vlax-3d-point (list 0 0 (* -1 curve_rad)))
                (vlax-3d-point (list 0 1 (* -1 curve_rad)))
                (+ start_ang (/ (+ letter_pos (/ space 2)) curve_rad))
              )
            )
          )
        )
        ;GET NEXT LETTER INSERTION POINT
        (if curve_rad
          (setq letter_pos (+ space letter_pos))
          (setq letter_ip (polar letter_ip 0 space))
        )
      )
      ;INSERT THE 3D TEXT BLOCK
      (vl-cmdf "._ucs" "3" (trans origin 0 1)(trans xvec 0 1)(trans yvec 0 1)
               "._insert" (vla-get-name 3dt_block) '(0 0 0) "" "" "")
      (setq 3dt_block (vlax-ename->vla-object (entlast)))
      (vla-put-layer 3dt_block layer)
      ;STORE XDATA IN THE BLOCK
      (xdata "put" 3dt_block "text" (list (cons 1000 text)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "depth" (list (cons 1041 depth)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "height" (list (cons 1041 txtht)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "width" (list (cons 1040 width)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "origin" (list (cons 1011 origin)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "xvec" (list (cons 1011 xvec)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "yvec" (list (cons 1011 yvec)) "$3D_TEXT_APP$")
      (xdata "put" 3dt_block "justify" (list (cons 1070 (car justify))) "$3D_TEXT_APP$")
      (if curve_rad
        (xdata "put" 3dt_block "curve" (list (cons 1041 curve_rad)) "$3D_TEXT_APP$")
      )
    )
  )
  (vl-cmdf ".ucs" "r" "$3D_Text_UCS$")
  (setvar "cmdecho" oldecho)
  (setvar "osmode" osmode)
  (setq *error* olderr)
  (vla-EndUndoMark document)					;End undo grouping
  (princ);exit quitely
)
;--End Main Program-----------------------------------------------------------------------------------

(defun collect_search (collection
                       properity
                       value
                       / i return)
  (setq i (1- (vla-get-count collection)))
  (while (<= 0 i)
    (if (equal value ((eval properity)(vla-item collection i)))
      (setq return (vla-item collection i)
            i -1)
      (setq i (1- i))
    )
  )
  return
)

;FUNCTION USED TO GET JUSTIFICATION DATA
(defun get_justify (arg / value)
  (cond
    ((= 'ENAME (type arg))
      (setq value (vla-get-Alignment
                    (vlax-ename->vla-object arg)))
    )
    ((= 'VLA-OBJECT (type arg))
      (setq value (vla-get-Alignment arg))
    )
    (T (setq value arg))
  )
  (cond
    ((or (= "TL" value)
         (= 6 value))
      (list 6 '(0 -1))
    )
    ((or (= "TC" value)
         (= 7 value))
      (list 7 '(-0.5 -1))
    )
    ((or (= "TR" value)
         (= 8 value))
      (list 8 '(-1 -1))
    )
    ((or (= "ML" value)
         (= 9 value))
      (list 9 '(0 -0.5))
    )
    ((or (= "MC" value)
         (= 4 value)
         (= 10 value))
      (list 10 '(-0.5 -0.5))
    )
    ((or (= "MR" value)
         (= 11 value))
      (list 11 '(-1 -0.5))
    )
    ((or (= "BC" value)
         (= 1 value)
         (= 3 value)
         (= 5 value)
         (= 13 value))
      (list 13 '(-0.5 0))
    )
    ((or (= "BR" value)
         (= 2 value)
         (= 14 value))
      (list 14 '(-1 0))
    )
    (T (list 12 '(0 0)))	;BL
  )
)

;FUNCTION USED TO CONVERT A SELECTION SET TO A LIST OF ENAMES
(defun ss->lst (ss / num i lst)
  (if ss (setq i 0 num (sslength ss)))				;store length of selection set
  (while (and ss (<= i (1- num)))				;Rebuild selection set into list
    (setq lst (append lst (list (ssname ss i)))
          i (1+ i))
  )
  lst								;return the list
)

;Function used to convert a list of like elements into a safearray
(defun list->safearray (lst /)
  (vlax-safearray-fill
    (vlax-make-safearray
      (vlax-variant-type (vlax-make-variant (car lst)))		;read the "type" of the first element of the list
      (cons 0 (1- (length lst)))				;construct a safearray of the correct length
    )
    lst								;fill it with the list
  )
)

;SUBROUTINE USED TO MAKE THE FRACTION BLOCKS
(defun make_frac (char_list					;list representing the characters and their positions
                  block_name					;name of the block
                  / frac_block char)
  (setq frac_block (vla-add (vla-get-blocks document) zeropoint block_name))
  (foreach char char_list					;create each character in the supplied position
    (setq letter (car char)
          char_pos (cadr char)
          letter_data (assoc letter font)
          letter_width (cadr letter_data)
          letter_plines (caddr letter_data)
          case (if (member letter small_case) "" "CAP_"))
    (if (not (tblsearch "BLOCK" (strcat "3D_LETR_" case letter)))
      (setq block (make_block letter_plines (strcat "3D_LETR_" case letter) nil))
      (setq block (collect_search (vla-get-blocks document) 'vla-get-name chr_name)
            letter_rev (xdata "get" block "letter_rev" nil "$3D_TEXT_APP$"))
    )
    (if (/= letter_rev cur_ltr_rev)
      (make_block letter_plines chr_name block)
    )
    (vla-insertblock
      frac_block
      (list->safearray (append char_pos '(0.0)))
      (strcat "3D_LETR_" case letter)
      0.7 0.7 1 0
    )
  )
  frac_block
)

;Subroutine to make a block and add the extruded letter to the block
(defun make_block (plines					;plines representing the shape of the letter
                   block_name					;name of the block
                   rebuild					;supply block object to redefine/rebuild
                   / block pline pline_lst region_lst region temp)
  (if rebuild
    (progn
      (setq block rebuild)
      (vlax-for temp block
        (vla-delete temp)
      )
    )
    (setq block (vla-add (vla-get-blocks document) zeropoint block_name));define new block collection
  )
  (foreach pline plines						;create plines to represent the shape of the letter
    (vla-put-closed
      (vla-AddLightweightPolyline
        activespace
        (list->safearray (apply 'append pline))
      )
      :vlax-true
    )
    (setq pline_lst (append pline_lst (list (vlax-ename->vla-object (entlast)))))
  )								;add each pline to a list
  (setq region_lst (vlax-safearray->list
                     (vlax-variant-value
                       (vla-addregion
                         activespace
                         (list->safearray pline_lst)))))	;convert all plines in list into regions
  (foreach temp pline_lst (vla-delete temp))			;delete the left over plines
  (foreach temp region_lst
    (setq hatch (vla-addhatch block acHatchPatternTypePredefined "SOLID" :vlax-false))
    (vla-appendouterloop hatch (list->safearray (list temp)))	;add solid fill to top layer of text
    (vla-put-layer hatch "0")
    (set_TrueColor hatch "0")
  )
  (setq region (car region_lst))
  (foreach temp (cdr region_lst)
    (vla-Boolean region acUnion temp)				;union all the regions together
  )
  (setq temp (vla-addextrudedsolid block region -1 0))		;extrude unioned region and add to block
  (vla-put-layer temp "0")
  (set_TrueColor temp "0")
  (entdel (entlast))						;delete left over region in active space
  (xdata "put" block "letter_rev" (list (cons 1000 cur_ltr_rev)) "$3D_TEXT_APP$")
  block								;return the block object
)

(defun set_TrueColor (object color / vla_truecolor acad_ver)
  (if (and (setq acad_ver (getvar "acadver"))
           (setq acad_ver (atoi (substr (getvar "acadver") 1 2)))
           (< 0 acad_ver))
    (setq acad_ver (itoa acad_ver))
    (if (<= 2000 (atoi (setq acad_ver (substr (ver) 13 4))))
      (if (member acad_ver '("2000" "2002"))
        (setq acad_ver "15")
        (setq acad_ver (itoa (+ 16 (fix (/ (- (atoi acad_ver) 2003.1) 3))))) ; every 3 years, add one unit to the version number
      )
      (setq acad_ver nil)
    )
  )
  (if acad_ver
    (progn
      (setq vla_truecolor (vla-getinterfaceobject (vlax-get-acad-object)(strcat "AutoCAD.AcCmColor." acad_ver)))
      (cond
        ((and (= 'INT (type color))
              (and (<= 0 color)
                   (>= 256 color)))
          (vla-put-colorindex vla_truecolor color)
        )
        ((or (= color "0")
             (and (= 'STR (type color))
                  (> 0 (atoi color))))
          (vla-put-colorindex vla_truecolor (atoi color))
        )
        ((and (= 'STR (type color))
              (= 0 (atoi color)))
          (vla-put-colorindex vla_truecolor (eval (read (strcat "ac" color))))
        )
        ((listp color)
          (vla-setrgb vla_truecolor (car color)(cadr color)(caddr color))
        )
      )
      (vla-put-TrueColor object vla_truecolor)
    )
    (progn
      (alert (strcat "The function \"set_TrueColor\" in this LISP routine does not work in this version of AutoCAD.\n"
                     "Please contact the developer for an update of this LISP routine (aaron_werning@yahoo.com)."))
      (exit)
    )
  )
)

;FUCTION USED TO "GET" OR "PUT" XDATA INTO AN ENTITY
(defun xdata (get_put	;specify "GET" or "PUT"
              ent	;entity to add xdata to
              key	;key of xdata value to get or put (must be a string name)
              conslst	;value of data associated with "key" (must be a list of construction lists)
              		;	e.g. '((1011 3.5 5.75 9.375)(1040 . 0.75)(1041 . 3.5))
              app	;name of registered application to store xdata under
              / elist xdata application part appdata olddata newdata
                newappdata newxdata return xcontainer xcontainers temp)
  (if (= 'VLA-OBJECT (type ent))
    (setq ent (vlax-vla-object->ename ent))
  )
  (setq elist (entget ent '("*"))
        xdata (assoc -3 elist)
        application (if xdata (assoc app (cdr xdata)))
        appdata (if application (cdr application))
        get_put (strcase get_put)
        key (strcase key)
        temp appdata)
  (if appdata
    (while temp
      (if (equal (car temp) '(1002 . "}"))
        (setq xcontainer (append xcontainer (list (car temp)))
              xcontainers (append xcontainers (list xcontainer))
              xcontainer nil test nil)
        (setq xcontainer (append xcontainer (list (car temp))))
      )
      (setq temp (cdr temp))
    )
  )
  (if xcontainers
    (foreach xcontainer xcontainers
      (if (= key (cdadr xcontainer))
        (setq olddata xcontainer)
      )
    )
  )
  (if (and olddata (= "GET" get_put))
    (progn
      (foreach temp (cddr olddata)
        (if (not (equal temp '(1002 . "}")))
          (setq return (append return (list (cdr temp))))
        )
      )
      (if (= 1 (length return))
        (setq return (car return))
      )
    )
    (if (= "PUT" get_put)
      (progn
        (if (not (tblsearch "appid" app))		;check and see if <app> is a registered xdata application
          (regapp app)					;if not register it so that we can store xdata
        )
        (setq newdata (list '(1002 . "{")
                             (cons 1000 key)))
        (foreach part conslst
          (setq newdata (append newdata (list part)))
        )
        (setq newdata (append newdata (list '(1002 . "}"))))
        (if olddata
          (setq appdata (apply 'append (subst newdata olddata xcontainers)))
          (if appdata
            (setq appdata (append appdata newdata))
            (setq appdata newdata)
          )
        )
        (setq newappdata (cons app appdata))
        (if application
          (setq newxdata (cons -3 (subst newappdata application (cdr xdata)))
                return (entmod (subst newxdata xdata elist)))
          (if xdata
            (setq newxdata (cons -3 (append (cdr xdata)(list newappdata))))
                  ;return (entmod (subst newxdata xdata elist)))
            (setq newxdata (cons -3 (list newappdata))
                  return (entmod (append elist (list newxdata))))
          )
        )
      )
    )
  )
  return
)

;--Define custom text edit dialog
(defun custom_Text_edit (text / temp_dcl write_dcl dclfile dcl_test temp header)
  (setq temp_file (strcat (getvar "tempprefix") "3D_Text.dcl"))
  (setq write_dcl (open temp_file "w"))
  (write-line " " write_dcl)
  (write-line " " write_dcl)
  (write-line "TEMP    : dialog {" write_dcl)
  (write-line "          label = \"3D Text Editor\";" write_dcl)
  (write-line "          children_alignment = centered;" write_dcl)
  (write-line "          initial_focus = \"TEXT\";" write_dcl)
  (write-line "          : spacer {height = 1;}" write_dcl)
  (write-line "          : row {" write_dcl)
  (write-line "            : edit_box {" write_dcl)
  (write-line "              width = 50;" write_dcl)
  (write-line "              allow_accept = true;" write_dcl)
  (write-line "              label = \"Text:  \";" write_dcl)
  (write-line "              key = \"TEXT\";" write_dcl)
  (write-line "            }" write_dcl)
  (write-line "            : spacer {width = 1;}" write_dcl)
  (write-line "          }" write_dcl)
  (write-line "          : spacer {height = 1;}" write_dcl)
  (write-line "          ok_cancel;" write_dcl)
  (write-line "        }" write_dcl)
  (close write_dcl)
  (setq dclfile (load_dialog temp_file))				;load the dialog file
  (if (not (new_dialog "TEMP" dclfile ""))(exit))			;load dialog-key
  (if text
    (set_tile "TEXT" text)
  )
  (action_tile "TEXT" "(setq temp $value)")
  (setq dcl_test (start_dialog))					;run dialog and store returned value
  (unload_dialog dclfile)						;unload the dialog file
  (cond
    ((= 0 dcl_test)
      (setq text nil)
    )
    ((and (= 1 dcl_test)
          temp
          (/= temp text))
      (setq text temp)
    )
  )
  text
)

;define routine to fix special characters into %% codes
; - routine is also needed to read %% codes
(defun fix_chr (char)
  (cond
    ((= "*" char) (setq char "%%A"))
    ((= "\\" char)(setq char "%%B"))
    ((= "=" char) (setq char "%%E"))
    ((= "/" char) (setq char "%%F"))
    ((= ">" char) (setq char "%%G"))
    ((= "<" char) (setq char "%%L"))
    ((= "?" char) (setq char "%%M"))
    ((= "\"" char)(setq char "%%Q"))
    ((= ";" char) (setq char "%%S"))
    ((= "|" char) (setq char "%%V"))
    ((= "`" char) (setq char "%%X"))
    ((= ":" char) (setq char "%%Y"))
    ((= "," char) (setq char "%%Z"))
    ((and (= "%" char)
          (= "%" (substr text (1+ cnt) 1)))
      (if (< 0 (atoi (substr text (+ 2 cnt) 1)))
        (setq char (strcase (substr text cnt 5))
              cnt (+ 4 cnt))
        (setq char (strcase (substr text cnt 3))
              cnt (+ 2 cnt))
      )
    )
  )
  (if (and (/= "" char)
           (/= " " char)
           (not (assoc char font)))
    (setq char "%%M")
  )
  char
)

;FUNCTION USED TO LOAD SPECIFIED FONT (CURRENTLY ONLY "ARIAL" FONT IS AVAILIABLE)
(defun load_font (font)
  (cond
    ((= "ARIAL" font)
     '((" " 0.4 nil)
       ("A" 0.927 (((-0.1138 0.0) (0.0272 0.0) (0.3384 0.8948) (0.6707 0.0) (0.8215 0.0) (0.413 1.0001) (0.2703 1.0001))((0.5681 0.3031) (0.5278 0.4117) (0.1604 0.4117) (0.1226 0.3031))))
       ("B" 0.933 (((0.4701 0.1232) (0.3809 0.1183) (0.3809 0.0) (0.4603 0.0032) (0.5754 0.0276) (0.6727 0.0859) (0.7328 0.1746) (0.7537 0.2901) (0.744 0.3647) (0.7051 0.4425) (0.65 0.4944) (0.5657 0.5333) (0.6127 0.5608) (0.6794 0.6327) (0.7029 0.7175) (0.7002 0.7943) (0.6662 0.8769) (0.6322 0.9207) (0.5592 0.9709) (0.4733 0.9936) (0.3744 1.0001) (0.3323 0.8818) (0.4101 0.8802) (0.483 0.8688) (0.556 0.8084) (0.5754 0.7294) (0.5479 0.6386) (0.4733 0.5916) (0.3485 0.5803) (0.3631 0.462) (0.4312 0.4603) (0.5203 0.4409) (0.5965 0.3761) (0.6176 0.2901) (0.5981 0.2059) (0.5463 0.1507))((0.1313 0.1183) (0.1313 0.462) (0.3631 0.462) (0.3485 0.5803) (0.1313 0.5803) (0.1313 0.8818) (0.3323 0.8818) (0.3744 1.0001) (0.0 1.0001) (0.0 0.0) (0.3809 0.0) (0.3809 0.1183))))
       ("C" 1.000 (((0.8413 0.3177) (0.7084 0.3517) (0.6581 0.2221) (0.6046 0.1605) (0.5058 0.1086) (0.415 0.0973) (0.3275 0.1086) (0.2383 0.1491) (0.1751 0.2075) (0.1167 0.3274) (0.1005 0.4133) (0.094 0.509) (0.1038 0.6143) (0.1297 0.7116) (0.1719 0.7878) (0.2561 0.8623) (0.3339 0.8931) (0.4263 0.9045) (0.428 0.9045) (0.5187 0.8915) (0.5933 0.8542) (0.6549 0.7845) (0.6938 0.6954) (0.8235 0.7262) (0.791 0.8105) (0.744 0.8834) (0.6792 0.9434) (0.6063 0.9839) (0.5236 1.0098) (0.4296 1.0179) (0.3534 1.0131) (0.2675 0.992) (0.1881 0.958) (0.1281 0.9174) (0.0665 0.8558) (0.0179 0.7813) (-0.0178 0.6889) (-0.0356 0.603) (-0.0421 0.5009) (-0.0356 0.4101) (-0.0178 0.3226) (0.0114 0.2399) (0.0454 0.1734) (0.0989 0.1037) (0.1654 0.0502) (0.2448 0.0113) (0.3291 -0.0097) (0.428 -0.0162) (0.5382 -0.0049) (0.6208 0.0243) (0.7132 0.0875) (0.7684 0.1507) (0.8105 0.2269))))
       ("D" 1.005 (((0.5641 0.0292) (0.6711 0.0908) (0.7327 0.1589) (0.7667 0.2156) (0.8072 0.3258) (0.8218 0.4117) (0.8267 0.5057) (0.8234 0.5738) (0.8121 0.663) (0.7748 0.7748) (0.7294 0.851) (0.6694 0.9142) (0.6078 0.9547) (0.5236 0.9855) (0.4474 0.9969) (0.3453 1.0001) (0.3436 0.8818) (0.4425 0.8769) (0.5138 0.8623) (0.5803 0.8218) (0.6403 0.7489) (0.6646 0.6938) (0.684 0.6111) (0.6905 0.5073) (0.6824 0.3955) (0.6646 0.3161) (0.6403 0.2577) (0.5916 0.188) (0.5025 0.1362) (0.4458 0.1248) (0.3469 0.1183) (0.3615 0.0) (0.4377 0.0032))((0.1329 0.1183) (0.1329 0.8818) (0.3436 0.8818) (0.3453 1.0001) (0.0 1.0001) (0.0 0.0) (0.3615 0.0) (0.3469 0.1183))))
       ("E" 0.933 (((0.6857 0.4587) (0.6857 0.5754) (0.1329 0.5754) (0.1329 0.8818) (0.7229 0.8818) (0.7229 1.0001) (0.0 1.0001) (0.0 0.0) (0.7456 0.0) (0.7456 0.1183) (0.1329 0.1183) (0.1329 0.4587))))
       ("F" 0.848 (((0.6014 0.5722) (0.1329 0.5722) (0.1329 0.8818) (0.6743 0.8818) (0.6743 1.0001) (0.0 1.0001) (0.0 0.0) (0.1329 0.0) (0.1329 0.4555) (0.6014 0.4555))))
       ("G" 1.080 (((0.7583 0.2059) (0.6335 0.1362) (0.5541 0.1118) (0.4666 0.1021) (0.3628 0.1151) (0.2656 0.154) (0.1959 0.2075) (0.1397 0.292) (0.1067 0.4069) (0.0986 0.5041) (0.1116 0.6224) (0.1489 0.731) (0.2283 0.8299) (0.3077 0.8769) (0.3709 0.8964) (0.4633 0.9045) (0.5409 0.8978) (0.6229 0.8677) (0.6862 0.8204) (0.7178 0.7764) (0.7535 0.6889) (0.8718 0.7213) (0.8394 0.8137) (0.7972 0.885) (0.7405 0.9385) (0.64 0.9904) (0.5557 1.0115) (0.4552 1.0179) (0.3628 1.0098) (0.2769 0.9888) (0.1991 0.9563) (0.131 0.9093) (0.0711 0.8461) (0.0241 0.7683) (-0.0132 0.6727) (-0.0311 0.5868) (-0.0375 0.4863) (-0.0294 0.3939) (-0.0084 0.3096) (0.0241 0.2286) (0.0711 0.1572) (0.131 0.0956) (0.2153 0.0421) (0.2964 0.0097) (0.3823 -0.0097) (0.4909 -0.0163) (0.5995 -0.0032) (0.6854 0.0227) (0.734 0.0438) (0.8864 0.1394) (0.8864 0.5106) (0.4633 0.5106) (0.4633 0.3923) (0.7583 0.3923))))
       ("H" 1.005 (((0.7851 0.0) (0.785 1.0001) (0.6516 1.0001) (0.6516 0.59) (0.1329 0.59) (0.1329 1.0001) (0.0 1.0001) (0.0 0.0) (0.1329 0.0) (0.1329 0.4717) (0.6516 0.4717) (0.6516 0.0))))
       ("I" 0.356 (((0.136 0.0) (0.0 0.0) (0.0 1.0001) (0.136 1.0001))))
       ("J" 0.683 (((-0.0606 0.1248) (-0.0056 0.0466) (0.0886 -0.0049) (0.1826 -0.0162) (0.2571 -0.0097) (0.3366 0.0195) (0.3836 0.0535) (0.4336 0.1264) (0.4565 0.2172) (0.463 0.3193) (0.463 1.0001) (0.3317 1.0001) (0.3317 0.3112) (0.3171 0.1864) (0.2669 0.1248) (0.2669 0.1232) (0.1633 0.103) (0.0756 0.1443) (0.048 0.1994) (0.0335 0.2999) (-0.0865 0.2837) (-0.0816 0.2091))))
       ("K" 0.927 (((0.8267 0.0) (0.389 0.5949) (0.8072 1.0001) (0.6289 1.0001) (0.1329 0.5041) (0.1329 1.0001) (0.0 1.0001) (0.0 0.0) (0.1329 0.0) (0.1329 0.3469) (0.2966 0.5073) (0.6516 0.0))))
       ("L" 0.772 (((0.6241 0.1183) (0.1313 0.1183) (0.1313 1.0001) (0.0 1.0001) (0.0 0.0) (0.6241 0.0))))
       ("M" 1.161 (((0.7627 1.0001) (0.5113 0.2912) (0.4696 0.1576) (0.4357 0.2918) (0.1994 1.0001) (0.0 1.0001) (0.0 0.0) (0.1281 0.0) (0.1281 0.851) (0.4166 0.0) (0.5228 0.0) (0.8129 0.838) (0.8129 0.0) (0.941 0.0) (0.941 1.0001))))
       ("N" 1.005 (((0.785 1.0001) (0.6613 1.0001) (0.6613 0.2156) (0.1362 1.0001) (0.0 1.0001) (0.0 0.0) (0.1281 0.0) (0.1281 0.7861) (0.6516 0.0) (0.785 0.0))))
       ("O" 1.080 (((0.5108 -0.0113) (0.5967 0.0097) (0.6777 0.0454) (0.7409 0.0892) (0.8025 0.1507) (0.8512 0.2269) (0.8884 0.3193) (0.9063 0.4069) (0.9127 0.4992) (0.9046 0.603) (0.8852 0.6889) (0.8544 0.7683) (0.8139 0.8348) (0.7555 0.8996) (0.6842 0.9515) (0.6113 0.9871) (0.5253 1.0098) (0.4153 1.0174) (0.4362 0.9045) (0.5351 0.8899) (0.6145 0.8542) (0.6874 0.7894) (0.7458 0.6888) (0.7701 0.5949) (0.7766 0.4992) (0.7733 0.4344) (0.7571 0.3436) (0.7247 0.2658) (0.6696 0.1913) (0.6015 0.1394) (0.5237 0.107) (0.4216 0.0973) (0.4346 -0.0162))((0.1817 0.0519) (0.2595 0.0146) (0.3438 -0.0081) (0.4346 -0.0162) (0.4216 0.0973) (0.3341 0.1102) (0.2579 0.1443) (0.1845 0.2084) (0.125 0.3047) (0.1007 0.389) (0.0926 0.4863) (0.099 0.5884) (0.1185 0.6775) (0.1493 0.7505) (0.1947 0.8088) (0.2692 0.8623) (0.3487 0.8931) (0.4362 0.9045) (0.4153 1.0174) (0.3098 1.0034) (0.2287 0.9758) (0.1558 0.9337) (0.0909 0.8769) (0.0488 0.8267) (0.0083 0.7554) (-0.0209 0.6759) (-0.0371 0.5868) (-0.0436 0.4879) (-0.0371 0.4069) (-0.0193 0.3209) (0.0148 0.2383) (0.0537 0.1702) (0.112 0.1054))))
       ("P" 0.933 (((0.1329 0.5252) (0.1329 0.8818) (0.4328 0.8818) (0.3761 1.0001) (0.0 1.0001) (0.0 0.0) (0.1329 0.0) (0.1329 0.4069) (0.389 0.4069) (0.3906 0.5252))((0.389 0.4069) (0.4555 0.4101) (0.5511 0.4247) (0.6257 0.4539) (0.6808 0.496) (0.7181 0.543) (0.7521 0.6224) (0.7618 0.7116) (0.7491 0.8067) (0.7116 0.8834) (0.6516 0.945) (0.5809 0.9774) (0.4782 0.9969) (0.3761 1.0001) (0.4328 0.8818) (0.5106 0.8737) (0.5705 0.8415) (0.6078 0.7943) (0.6257 0.7067) (0.6176 0.6419) (0.5656 0.5661) (0.4928 0.5349) (0.3906 0.5252))))
       ("Q" 1.080 (((0.9218 0.0146) (0.8343 0.0567) (0.7532 0.107) (0.6495 0.1815) (0.5506 0.2448) (0.4615 0.2772) (0.429 0.1799) (0.4809 0.1637) (0.5603 0.1232) (0.6689 0.0454) (0.8051 -0.0438) (0.8829 -0.0778))((0.7727 0.1281) (0.8262 0.1994) (0.8715 0.2967) (0.8975 0.4069) (0.9024 0.5009) (0.8959 0.603) (0.8764 0.6889) (0.8456 0.7683) (0.8051 0.8348) (0.7468 0.8996) (0.6754 0.9531) (0.6009 0.9888) (0.5166 1.0115) (0.4258 1.0179) (0.3431 1.0115) (0.2572 0.9904) (0.1778 0.9547) (0.1146 0.911) (0.0546 0.8477) (0.0027 0.7602) (-0.0281 0.6792) (-0.0459 0.5916) (-0.0524 0.5009) (-0.0443 0.3987) (-0.0248 0.3128) (0.0076 0.2334) (0.0481 0.167) (0.1065 0.1005) (0.1778 0.0486) (0.2491 0.0146) (0.3334 -0.0081) (0.4226 -0.0162) (0.5036 -0.0113) (0.5895 0.0097) (0.6689 0.0454) (0.5603 0.1232) (0.515 0.1086) (0.4274 0.0973) (0.3237 0.1102) (0.2475 0.1459) (0.181 0.201) (0.1486 0.2431) (0.113 0.3145) (0.0903 0.4004) (0.0838 0.5009) (0.087 0.5754) (0.1049 0.6646) (0.1357 0.7408) (0.181 0.804) (0.2589 0.8623) (0.3367 0.8948) (0.4274 0.9045) (0.5263 0.8899) (0.6057 0.8542) (0.6787 0.7894) (0.7343 0.6913) (0.7597 0.5949) (0.7662 0.5009) (0.7662 0.4992) (0.7597 0.4004) (0.737 0.3145) (0.7014 0.2415) (0.6495 0.1815) (0.7532 0.107))((0.7532 0.107) (0.7727 0.1281))))
       ("R" 1.005 (((0.7083 0.2723) (0.6565 0.3436) (0.5949 0.4085) (0.5187 0.4555) (0.4166 0.5592) (0.1329 0.5592) (0.1329 0.8899) (0.449 0.8899) (0.4441 1.0001) (0.0 1.0001) (0.0 0.0) (0.1329 0.0) (0.1329 0.4441) (0.2869 0.4441) (0.3598 0.4393) (0.4247 0.4101) (0.4879 0.3469) (0.5268 0.2934) (0.5835 0.2091) (0.7148 0.0) (0.8818 0.0))((0.5138 0.5673) (0.4166 0.5592) (0.5187 0.4555) (0.5868 0.4684) (0.6694 0.5009) (0.7332 0.551) (0.7861 0.6403) (0.7991 0.7278) (0.791 0.7975) (0.7586 0.8785) (0.7197 0.9272) (0.6467 0.9742) (0.5479 0.9952) (0.4441 1.0001) (0.449 0.8899) (0.5446 0.8785) (0.6127 0.8445) (0.6495 0.7887) (0.663 0.7213) (0.637 0.6386) (0.5852 0.5925))))
       ("S" 0.927 (((0.4632 0.1118) (0.3708 0.1021) (0.3011 0.1086) (0.2185 0.1329) (0.1623 0.1645) (0.118 0.2107) (0.0899 0.2717) (0.0758 0.3323) (-0.049 0.3226) (-0.0312 0.2204) (0.0142 0.1313) (0.0742 0.0681) (0.1504 0.0227) (0.2148 0.0013) (0.2957 -0.0129) (0.4 -0.0162) (0.4892 -0.0049) (0.5702 0.0227) (0.6448 0.0681) (0.7015 0.1329) (0.7323 0.1978) (0.7469 0.2837) (0.7404 0.3469) (0.7047 0.4247) (0.6561 0.4782) (0.5767 0.5268) (0.4665 0.5641) (0.2087 0.6322) (0.1471 0.6711) (0.1214 0.7276) (0.1228 0.7861) (0.1623 0.8486) (0.246 0.8915) (0.3465 0.9012) (0.4519 0.8883) (0.5216 0.8526) (0.5653 0.7975) (0.5897 0.71) (0.7177 0.7181) (0.7047 0.7975) (0.6675 0.8769) (0.6156 0.9353) (0.5378 0.9823) (0.4357 1.0098) (0.3417 1.0179) (0.2412 1.0082) (0.1585 0.9839) (0.0904 0.945) (0.0321 0.8834) (0.0045 0.8283) (-0.0084 0.7642) (-0.0009 0.6791) (0.0256 0.6176) (0.0564 0.5787) (0.1309 0.5252) (0.2185 0.4911) (0.4405 0.4328) (0.5021 0.4133) (0.556 0.3856) (0.6062 0.3292) (0.6157 0.2501) (0.5913 0.1864) (0.534 0.1403))))
       ("T" 0.848 (((0.3832 0.0) (0.3832 0.8818) (0.7139 0.8818) (0.7139 1.0001) (-0.0788 1.0001) (-0.0788 0.8818) (0.2503 0.8818) (0.2503 0.0))))
       ("U" 1.005 (((0.653 1.0001) (0.653 0.4182) (0.6465 0.308) (0.6254 0.2253) (0.6006 0.1792) (0.559 0.1426) (0.4844 0.1135) (0.402 0.1023) (0.3191 0.1086) (0.2397 0.1362) (0.1962 0.1698) (0.157 0.2318) (0.1376 0.3177) (0.1311 0.4231) (0.1311 1.0001) (0.0 1.0001) (0.0 0.4231) (0.003 0.3339) (0.016 0.2448) (0.0452 0.1556) (0.0938 0.0843) (0.1651 0.0324) (0.2137 0.0113) (0.2964 -0.0097) (0.3953 -0.0162) (0.4698 -0.013) (0.5557 0.0065) (0.6433 0.0486) (0.7081 0.1086) (0.7519 0.1832) (0.7665 0.235) (0.7801 0.3209) (0.785 0.4231) (0.785 1.0001))))
       ("V" 0.927 (((0.3507 0.1102) (0.2972 0.2739) (0.0379 1.0001) (-0.1048 1.0001) (0.2826 0.0) (0.4172 0.0) (0.8095 1.0001) (0.6733 1.0001) (0.4042 0.2739))))
       ("W" 1.316 (((0.8513 0.141) (0.8113 0.3518) (0.6309 1.0001) (0.472 1.0001) (0.2808 0.3258) (0.2354 0.141) (0.1932 0.3453) (0.0425 1.0001) (-0.0937 1.0001) (0.1722 0.0) (0.3067 0.0) (0.519 0.7618) (0.5482 0.8802) (0.579 0.7618) (0.7897 0.0) (0.9178 0.0) (1.1917 1.0001) (1.0588 1.0001) (0.9016 0.3582))))
       ("X" 0.927 (((0.3511 0.4247) (0.3949 0.3566) (0.6461 0.0) (0.8098 0.0) (0.4322 0.5284) (0.7823 1.0001) (0.6397 1.0001) (0.4387 0.7359) (0.3592 0.6208) (0.2782 0.744) (0.0966 1.0001) (-0.0606 1.0001) (0.2798 0.5219) (-0.106 0.0) (0.0529 0.0) (0.3041 0.3485))))
       ("Y" 0.927 (((0.4089 0.4247) (0.8076 1.0001) (0.6537 1.0001) (0.4608 0.7067) (0.3522 0.53) (0.25 0.6986) (0.0523 1.0001) (-0.1082 1.0001) (0.2776 0.4247) (0.2776 0.0) (0.4089 0.0))))
       ("Z" 0.848 (((-0.0844 0.0) (0.7066 0.0) (0.7066 0.1183) (0.068 0.1183) (0.1279 0.188) (0.6904 0.8818) (0.6904 1.0001) (-0.0261 1.0001) (-0.0261 0.8818) (0.5315 0.8818) (0.4278 0.7635) (-0.0844 0.1232))))
       ("1" 0.772 (((0.0846 0.6532) (0.2192 0.7294) (0.284 0.7829) (0.284 0.0) (0.4072 0.0) (0.4072 1.005) (0.3278 1.005) (0.2856 0.9401) (0.2192 0.8704) (0.1203 0.7975) (0.0393 0.7537) (0.0393 0.6354))))
       ("2" 0.772 (((0.0792 0.7019) (0.0906 0.7797) (0.1327 0.8494) (0.1862 0.8866) (0.2754 0.9029) (0.3386 0.8948) (0.399 0.8639) (0.4423 0.8137) (0.4634 0.7294) (0.4521 0.6646) (0.4067 0.5819) (0.3402 0.5073) (0.0792 0.2756) (0.0257 0.2188) (-0.0164 0.1621) (-0.0553 0.0859) (-0.0699 0.0) (0.5915 0.0) (0.5915 0.1183) (0.1003 0.1183) (0.1554 0.1896) (0.2073 0.2367) (0.4358 0.436) (0.4845 0.4863) (0.5234 0.5365) (0.5673 0.6079) (0.5858 0.6908) (0.585 0.7748) (0.559 0.8558) (0.5055 0.9255) (0.4521 0.9645) (0.3726 0.9952) (0.2786 1.005) (0.2024 0.9985) (0.1198 0.9742) (0.0517 0.9304) (0.0095 0.885) (-0.0277 0.8088) (-0.0472 0.7148))))
       ("3" 0.772 (((-0.0089 0.1281) (0.0478 0.0616) (0.0948 0.0259) (0.1742 -0.0065) (0.2666 -0.0178) (0.3574 -0.0065) (0.4385 0.0243) (0.5065 0.0746) (0.5552 0.1313) (0.5908 0.2091) (0.6022 0.2966) (0.5908 0.3825) (0.5535 0.4571) (0.4984 0.5073) (0.4158 0.5414) (0.4579 0.5657) (0.5066 0.6113) (0.5415 0.678) (0.5535 0.744) (0.5487 0.791) (0.5259 0.8561) (0.4838 0.9174) (0.4109 0.9693) (0.3493 0.9936) (0.2796 1.0042) (0.2037 0.9996) (0.1337 0.9807) (0.0624 0.9369) (0.02 0.8908) (-0.0138 0.8348) (-0.0397 0.7456) (0.0819 0.7229) (0.0997 0.791) (0.1434 0.8591) (0.2004 0.8923) (0.2545 0.9019) (0.3149 0.8971) (0.3675 0.8696) (0.4044 0.8315) (0.4264 0.7629) (0.4158 0.684) (0.3639 0.6208) (0.3104 0.5949) (0.2034 0.5819) (0.1888 0.4733) (0.2764 0.4879) (0.3444 0.4766) (0.4158 0.4328) (0.4547 0.3793) (0.4725 0.2934) (0.4595 0.2172) (0.4125 0.1443) (0.3509 0.1021) (0.2666 0.0843) (0.2011 0.0969) (0.1418 0.1313) (0.0997 0.188) (0.0705 0.2804) (-0.0527 0.2642))))
       ("4" 0.772 (((0.5966 0.3517) (0.4621 0.3517) (0.4621 1.0001) (0.3616 1.0001) (0.3389 0.804) (0.3389 0.3517) (0.3389 0.2399) (0.3389 0.0) (0.4621 0.0) (0.4621 0.2399) (0.5966 0.2399))((0.3616 1.0001) (-0.0939 0.3517) (-0.0939 0.2399) (0.3389 0.2399) (0.3389 0.3517) (0.026 0.3517) (0.3389 0.804))))
       ("5" 0.772 (((0.075 0.2739) (-0.053 0.2626) (-0.0092 0.1248) (0.0459 0.06) (0.0945 0.0243) (0.1739 -0.0065) (0.2896 -0.0164) (0.396 0.0049) (0.4689 0.0454) (0.5305 0.1054) (0.5711 0.1686) (0.5986 0.2496) (0.6083 0.3388) (0.6002 0.4247) (0.5694 0.5057) (0.5192 0.5722) (0.4641 0.6176) (0.3863 0.6516) (0.2801 0.6635) (0.1901 0.6435) (0.1091 0.6014) (0.1626 0.8688) (0.5613 0.8688) (0.5613 0.9871) (0.0653 0.9871) (-0.0319 0.4733) (0.0832 0.4587) (0.1342 0.5098) (0.1788 0.5381) (0.2647 0.5544) (0.3506 0.5398) (0.4203 0.4944) (0.4657 0.4198) (0.4787 0.329) (0.4662 0.2363) (0.4171 0.1507) (0.3522 0.1021) (0.2679 0.0843) (0.1999 0.0974) (0.1415 0.1329) (0.1026 0.1832))))
       ("6" 0.772 (((0.4195 0.1475) (0.3617 0.1021) (0.2866 0.0843) (0.2882 -0.0162) (0.3692 -0.0065) (0.4486 0.0259) (0.5102 0.0762) (0.5605 0.1507) (0.5621 0.1524) (0.5913 0.2367) (0.601 0.3258) (0.5929 0.4101) (0.5637 0.4911) (0.5151 0.5592) (0.4697 0.5997) (0.3919 0.637) (0.306 0.65) (0.2817 0.5414) (0.3498 0.53) (0.4211 0.4814) (0.4211 0.4798) (0.4632 0.4101) (0.4757 0.3054) (0.4616 0.2172))((0.2358 0.6408) (0.1715 0.616) (0.1228 0.5835) (0.0629 0.5155) (0.0661 0.5852) (0.0775 0.6759) (0.0985 0.7472) (0.1309 0.8105) (0.1702 0.8579) (0.2347 0.8924) (0.293 0.9029) (0.3433 0.8964) (0.3924 0.8683) (0.4372 0.8167) (0.4616 0.7456) (0.5832 0.7554) (0.5459 0.8737) (0.494 0.9385) (0.4454 0.9708) (0.3669 0.9997) (0.2685 1.0038) (0.1861 0.9871) (0.1115 0.9482) (0.0483 0.8899) (0.0483 0.8883) (0.0094 0.8299) (-0.0198 0.7586) (-0.0409 0.6743) (-0.0538 0.577) (-0.0575 0.4278) (-0.0474 0.3145) (-0.0295 0.2269) (-0.0003 0.154) (0.0491 0.0855) (0.1212 0.0259) (0.2006 -0.0065) (0.2882 -0.0162) (0.2866 0.0843) (0.2286 0.0974) (0.1617 0.1313) (0.1192 0.1863) (0.0969 0.2367) (0.0823 0.3242) (0.0969 0.4101) (0.1407 0.4814) (0.1974 0.5219) (0.2817 0.5414) (0.306 0.65))))
       ("7" 0.772 (((0.0931 0.0) (0.2196 0.0) (0.2293 0.1037) (0.2524 0.2236) (0.299 0.3761) (0.3687 0.5414) (0.4465 0.6824) (0.5486 0.8315) (0.6005 0.8915) (0.6005 0.9871) (-0.0463 0.9871) (-0.0463 0.8688) (0.4416 0.8688) (0.3638 0.7683) (0.2666 0.6078) (0.1953 0.4555) (0.1385 0.2853) (0.111 0.1653))))
       ("8" 0.772 (((0.1014 0.0227) (0.1808 -0.0065) (0.2748 -0.0162) (0.3213 0.0896) (0.2229 0.0923) (0.1344 0.136) (0.0884 0.2026) (0.0706 0.2885) (0.0835 0.3598) (0.1289 0.4328) (0.1857 0.4717) (0.2716 0.4895) (0.2393 0.5933) (0.1773 0.6207) (0.1322 0.6662) (0.1127 0.7229) (0.1231 0.807) (0.1565 0.8591) (0.21 0.8936) (0.2732 0.9029) (0.2911 1.005) (0.2148 1.0001) (0.1322 0.9758) (0.0641 0.9304) (0.0097 0.8594) (-0.0115 0.7829) (-0.0105 0.7019) (0.0219 0.6224) (0.0544 0.5868) (0.1354 0.543) (0.0576 0.5106) (-0.0056 0.4539) (-0.0413 0.3809) (-0.0542 0.2901) (-0.0478 0.2188) (-0.017 0.1394) (0.0365 0.0713))((0.3213 0.0896) (0.2748 -0.0162) (0.3656 -0.0065) (0.445 0.0227) (0.5131 0.0713) (0.5552 0.1216) (0.5909 0.1994) (0.6039 0.2869) (0.5925 0.3696) (0.5552 0.4458) (0.4985 0.5025) (0.4158 0.543) (0.4645 0.5657) (0.526 0.6224) (0.5559 0.6948) (0.5601 0.778) (0.5358 0.8591) (0.4823 0.9288) (0.4418 0.9612) (0.3656 0.9936) (0.2911 1.005) (0.2732 0.9029) (0.35 0.8841) (0.4083 0.8381) (0.4336 0.776) (0.4337 0.71) (0.3915 0.6354) (0.3607 0.6111) (0.3074 0.5936) (0.2393 0.5933) (0.2716 0.4895) (0.3477 0.4766) (0.4191 0.4312) (0.4612 0.3712) (0.4774 0.2853) (0.4661 0.2123) (0.4025 0.1242))))
       ("9" 0.772 (((0.5049 0.8964) (0.4401 0.9515) (0.3493 0.9936) (0.2618 1.005) (0.278 0.9029) (0.3593 0.8825) (0.4157 0.8364) (0.4563 0.7651) (0.4692 0.6727) (0.4546 0.577) (0.4141 0.5073) (0.359 0.4668) (0.2731 0.4474) (0.299 0.3453) (0.3817 0.3761) (0.4238 0.4052) (0.4822 0.4717) (0.4806 0.3842) (0.4644 0.2966) (0.4341 0.2091) (0.3709 0.1243) (0.2827 0.0888) (0.218 0.0875) (0.1555 0.1124) (0.1159 0.154) (0.0851 0.2431) (-0.0333 0.2318) (-0.0042 0.1295) (0.0337 0.0739) (0.0927 0.0218) (0.1548 -0.0049) (0.2306 -0.016) (0.3012 -0.0121) (0.3639 0.0032) (0.4414 0.0444) (0.5025 0.1018) (0.5426 0.1702) (0.573 0.248) (0.5908 0.3258) (0.6021 0.4166) (0.6054 0.5446) (0.5989 0.6467) (0.5859 0.7343) (0.5619 0.8086))((0.2439 0.3388) (0.299 0.3453) (0.2731 0.4474) (0.2034 0.4603) (0.1321 0.5073) (0.0899 0.5738) (0.0734 0.666) (0.0916 0.7651) (0.1353 0.8364) (0.1953 0.8834) (0.278 0.9029) (0.2618 1.005) (0.1856 0.9969) (0.1061 0.9661) (0.0381 0.9126) (-0.0138 0.8413) (-0.043 0.7602) (-0.0527 0.6662) (-0.043 0.577) (-0.0138 0.4976) (0.0332 0.4295) (0.0802 0.389) (0.1564 0.3517))))
       ("0" 0.772 (((0.5598 0.201) (0.5855 0.3015) (0.5952 0.3906) (0.5985 0.4944) (0.5985 0.5641) (0.5904 0.6549) (0.5758 0.731) (0.5531 0.804) (0.5224 0.8668) (0.4729 0.93) (0.4137 0.9726) (0.3634 0.992) (0.2727 1.005) (0.2727 0.9029) (0.3505 0.8834) (0.4153 0.8218) (0.438 0.7797) (0.4575 0.7067) (0.4688 0.6127) (0.4737 0.4944) (0.4704 0.3971) (0.4607 0.2983) (0.4412 0.2204) (0.4153 0.167) (0.3521 0.1054) (0.2727 0.0843) (0.2876 -0.0152) (0.3829 0.0016) (0.4619 0.0491) (0.5239 0.1248))((0.2876 -0.0152) (0.2727 0.0843) (0.1965 0.1037) (0.1433 0.1519) (0.109 0.2075) (0.0895 0.2804) (0.0782 0.3761) (0.0733 0.4944) (0.0765 0.6014) (0.0895 0.6986) (0.109 0.7748) (0.1365 0.8315) (0.19 0.8818) (0.2727 0.9029) (0.2727 1.005) (0.1657 0.9871) (0.0927 0.9466) (0.023 0.8656) (-0.0159 0.7797) (-0.0386 0.6873) (-0.0483 0.5965) (-0.0531 0.4944) (-0.0499 0.402) (-0.0386 0.3031) (-0.0191 0.2172) (0.0101 0.1459) (0.0457 0.0875) (0.1073 0.0308) (0.1835 -0.0049))))
       ("-" 0.460 (((0.3081 0.2999) (-0.0679 0.2999) (-0.0679 0.4247) (0.3081 0.4247))))
       ("%%E" 0.812 (((0.6265 0.5884) (0.6265 0.7035) (-0.0333 0.7035) (-0.0333 0.5884))((0.6265 0.2853) (0.6265 0.4004) (-0.0333 0.4004) (-0.0333 0.2853))))
       ("[" 0.384 (((0.1056 -0.1751) (0.1056 0.8996) (0.2547 0.8996) (0.2547 1.0001) (-0.016 1.0001) (-0.016 -0.2772) (0.2547 -0.2772) (0.2547 -0.1751))))
       ("]" 0.384 (((-0.0907 -0.1751) (-0.0907 -0.2772) (0.18 -0.2772) (0.18 1.0001) (-0.0907 1.0001) (-0.0907 0.8996) (0.0568 0.8996) (0.0568 -0.1751))))
       ("%%B" 0.384 (((0.2757 -0.0162) (0.1784 -0.0162) (-0.1117 1.0179) (-0.0128 1.0179))))
       ("%%S" 0.384 (((0.0041 -0.1443) (0.0382 -0.1978) (0.1098 -0.1398) (0.1396 -0.0835) (0.1516 0.0) (0.1516 0.141) (0.0122 0.141) (0.0122 0.0) (0.0803 0.0) (0.0651 -0.0852))((0.1516 0.5852) (0.1516 0.7246) (0.0122 0.7246) (0.0122 0.5852))))
       ("'" 0.262 (((0.0889 0.8332) (0.0565 0.6467) (-0.018 0.6467) (-0.0505 0.8332) (-0.0505 1.0001) (0.0889 1.0001))))
       ("%%Z" 0.384 (((0.0041 -0.1443) (0.0382 -0.1978) (0.1125 -0.1402) (0.14 -0.0853) (0.1516 0.0) (0.1516 0.141) (0.0122 0.141) (0.0122 0.0) (0.0819 0.0) (0.0665 -0.0862))))
       ("." 0.384 (((0.1517 0.0) (0.0123 0.0) (0.0123 0.141) (0.1517 0.141))))
       ("%%F" 0.384 (((-0.013 -0.0162) (-0.1119 -0.0162) (0.1782 1.0179) (0.2771 1.0179))))
       ("%%X" 0.460 (((0.1131 1.0066) (0.2055 0.8153) (0.1066 0.8153) (-0.0506 1.0066))))
       ("!" 0.384 (((0.1525 0.0) (0.1525 0.141) (0.0115 0.141) (0.0115 0.0))((0.0058 1.0001) (0.0058 0.778) (0.0431 0.2496) (0.1225 0.2496) (0.1582 0.778) (0.1582 1.0001))))
       ("@" 1.415 (((0.6804 0.1118) (0.6934 0.0535) (0.7355 0.0113) (0.8117 -0.0016) (0.9175 0.0154) (1.0127 0.0697) (1.097 0.1485) (1.157 0.2383) (1.1926 0.3161) (1.2153 0.3987) (1.2218 0.4863) (1.2137 0.577) (1.1926 0.663) (1.1484 0.757) (1.0824 0.8445) (1.0176 0.9029) (0.929 0.957) (0.8198 0.9969) (0.7323 1.0131) (0.6181 1.0193) (0.5086 1.0082) (0.4211 0.9904) (0.3416 0.9596) (0.2577 0.9132) (0.186 0.8558) (0.1277 0.7926) (0.0758 0.7197) (0.032 0.637) (0.0025 0.5617) (-0.0196 0.4759) (-0.0341 0.3798) (-0.0356 0.295) (-0.0279 0.2107) (-0.0085 0.1248) (0.035 0.0156) (0.0936 -0.0729) (0.1568 -0.1378) (0.212 -0.1783) (0.2881 -0.2221) (0.3767 -0.2546) (0.4713 -0.2772) (0.5605 -0.2901) (0.6691 -0.2934) (0.7712 -0.2869) (0.8571 -0.2723) (0.9682 -0.2369) (1.0662 -0.1848) (1.1456 -0.1232) (1.2558 0.013) (1.1326 0.013) (1.0338 -0.0827) (0.9386 -0.1386) (0.8393 -0.1718) (0.7501 -0.188) (0.6331 -0.1926) (0.5151 -0.1815) (0.4292 -0.1637) (0.3327 -0.1273) (0.2557 -0.0827) (0.1909 -0.0259) (0.133 0.053) (0.0904 0.1507) (0.0709 0.2367) (0.0647 0.361) (0.079 0.4652) (0.1017 0.5495) (0.1431 0.6402) (0.2071 0.7343) (0.2703 0.7959) (0.364 0.8558) (0.4567 0.8931) (0.5426 0.9126) (0.6334 0.9191) (0.7226 0.9126) (0.8085 0.8931) (0.8895 0.8607) (0.9446 0.8267) (1.0111 0.7699) (1.0727 0.685) (1.1116 0.577) (1.1213 0.4713) (1.1083 0.3842) (1.0704 0.2895) (1.0176 0.2107) (0.9349 0.141) (0.8676 0.1117) (0.8255 0.1226) (0.8101 0.1621) (0.8312 0.2626) (0.9284 0.7197) (0.8068 0.7197) (0.7841 0.6143) (0.743 0.6662) (0.6766 0.718) (0.5913 0.7376) (0.5864 0.6386) (0.6502 0.6241) (0.6999 0.5868) (0.736 0.5176) (0.7452 0.4425) (0.7342 0.3524) (0.7055 0.2697) (0.647 0.1787) (0.5892 0.1285) (0.518 0.0999) (0.5084 0.0002) (0.572 0.0227) (0.6139 0.0502))((0.3951 0.1507) (0.3643 0.2042) (0.3516 0.2706) (0.3546 0.3482) (0.3724 0.4247) (0.3999 0.4946) (0.4405 0.5592) (0.5053 0.616) (0.5864 0.6386) (0.5913 0.7376) (0.5021 0.7278) (0.4068 0.6785) (0.3303 0.603) (0.2682 0.4977) (0.233 0.3906) (0.2233 0.3047) (0.2314 0.2253) (0.2606 0.1426) (0.2914 0.0908) (0.353 0.034) (0.4182 0.0068) (0.5084 0.0002) (0.518 0.0999) (0.4464 0.1126))))
       ("#" 0.770 (((0.1195 0.2739) (0.139 0.3761) (0.1892 0.6241) (0.2103 0.7246) (0.2703 1.0179) (0.1682 1.0179) (0.1082 0.7246) (-0.0977 0.7246) (-0.0977 0.6241) (0.0871 0.6241) (0.0369 0.3761) (-0.0977 0.3761) (-0.0977 0.2739) (0.0174 0.2739) (-0.0425 -0.0162) (0.0596 -0.0162))((0.6463 0.2739) (0.6463 0.3761) (0.4567 0.3761) (0.507 0.6241) (0.6463 0.6241) (0.6463 0.7246) (0.528 0.7246) (0.588 1.0179) (0.4843 1.0179) (0.4259 0.7246) (0.2103 0.7246) (0.1892 0.6241) (0.4048 0.6241) (0.353 0.3761) (0.139 0.3761) (0.1195 0.2739) (0.3335 0.2739) (0.2752 -0.0162) (0.3773 -0.0162) (0.4356 0.2739))))
       ("$" 0.772 (((0.3828 -0.0084) (0.4526 0.0195) (0.5077 0.0614) (0.5548 0.1167) (0.5888 0.1945) (0.5995 0.2639) (0.595 0.3407) (0.5758 0.4069) (0.5256 0.4782) (0.4765 0.5137) (0.4089 0.5495) (0.31 0.5754) (0.31 0.4571) (0.3813 0.4279) (0.4413 0.3842) (0.4708 0.3113) (0.4672 0.2204) (0.434 0.153) (0.3762 0.1048) (0.31 0.0827) (0.31 -0.0195))((0.2371 1.0925) (0.2371 0.0827) (0.1819 0.0989) (0.1219 0.1459) (0.0863 0.2059) (0.0636 0.3015) (-0.0612 0.2788) (-0.026 0.1462) (0.0215 0.0729) (0.0879 0.0211) (0.1619 -0.0072) (0.2371 -0.0211) (0.2371 -0.1426) (0.31 -0.1426) (0.31 0.9304) (0.3831 0.896) (0.4218 0.8558) (0.4494 0.7683) (0.5758 0.7878) (0.5353 0.9077) (0.4753 0.9758) (0.4024 1.0147) (0.31 1.0342) (0.31 1.0925))((0.1544 0.4976) (0.2371 0.4733) (0.2371 0.5916) (0.1836 0.6127) (0.1187 0.6597) (0.0931 0.7166) (0.0863 0.7975) (0.1144 0.8589) (0.1699 0.9099) (0.2371 0.932) (0.2371 1.0342) (0.1406 1.0145) (0.0539 0.9677) (0.0101 0.9191) (-0.0272 0.8445) (-0.0401 0.7554) (-0.0226 0.6628) (0.015 0.5884) (0.0831 0.5333))))
       ("%" 1.237 (((0.8977 0.0787) (0.8289 0.0486) (0.8273 -0.0357) (0.9116 -0.0195) (0.9721 0.0226) (1.0109 0.0779) (1.0332 0.1313) (1.0429 0.2269) (1.0385 0.2897) (1.0218 0.3582) (0.9813 0.4263) (0.9116 0.4766) (0.8257 0.4928) (0.8322 0.4085) (0.9051 0.3696) (0.9321 0.2999) (0.9358 0.2243) (0.9221 0.1248))((0.7652 0.0797) (0.7349 0.1248) (0.7236 0.2237) (0.7284 0.2999) (0.756 0.3696) (0.8322 0.4085) (0.8257 0.4928) (0.7333 0.4717) (0.6701 0.4166) (0.6279 0.3258) (0.6166 0.235) (0.6211 0.1628) (0.6377 0.0973) (0.6766 0.0308) (0.743 -0.0178) (0.8273 -0.0357) (0.8289 0.0486))((0.7301 1.0179) (0.1838 -0.0357) (0.2843 -0.0357) (0.8289 1.0179))((0.1806 0.4895) (0.1822 0.5738) (0.1184 0.6048) (0.0882 0.65) (0.0768 0.7489) (0.0817 0.8251) (0.1093 0.8948) (0.1854 0.9337) (0.179 1.0179) (0.0866 0.9969) (0.0233 0.9418) (-0.0188 0.851) (-0.0301 0.7602) (-0.0256 0.688) (-0.0091 0.6224) (0.0298 0.556) (0.0963 0.5073))((0.3962 0.7521) (0.3917 0.8148) (0.3751 0.8834) (0.3346 0.9515) (0.2649 1.0017) (0.179 1.0179) (0.1854 0.9337) (0.2584 0.8948) (0.2853 0.8251) (0.2891 0.7494) (0.2754 0.65) (0.251 0.6039) (0.1822 0.5738) (0.1806 0.4895) (0.2649 0.5057) (0.3254 0.5478) (0.3641 0.6031) (0.3864 0.6565))))
       ("^" 0.683 (((0.1818 1.0179) (-0.0597 0.4717) (0.0667 0.4717) (0.2305 0.8785) (0.399 0.4717) (0.5222 0.4717) (0.2807 1.0179))))
       ("&" 0.927 (((0.1919 0.1054) (0.1271 0.1572) (0.0979 0.2042) (0.0784 0.2837) (0.1141 0.3842) (0.1627 0.436) (0.247 0.4944) (0.1838 0.5754) (0.0638 0.5009) (0.0023 0.4393) (-0.0399 0.3615) (-0.0525 0.2647) (-0.0302 0.1621) (0.0185 0.0875) (0.093 0.0259) (0.1708 -0.0065) (0.2632 -0.0162) (0.3345 -0.0097) (0.4172 0.0178) (0.482 0.0567) (0.5501 0.1183) (0.4723 0.2156) (0.4124 0.1541) (0.3475 0.1102) (0.2632 0.0924))((0.1222 0.6549) (0.1838 0.5754) (0.247 0.4944) (0.4723 0.2156) (0.5501 0.1183) (0.6393 0.0259) (0.7025 -0.0227) (0.7852 0.0746) (0.6879 0.1589) (0.6296 0.2286) (0.6895 0.3501) (0.7187 0.4441) (0.5906 0.4717) (0.5744 0.4117) (0.542 0.3323) (0.3637 0.5608) (0.3005 0.6403) (0.2454 0.7035) (0.2097 0.757) (0.2011 0.8222) (0.234 0.8866) (0.3167 0.9158) (0.32 1.0179) (0.213 0.9969) (0.14 0.9499) (0.0882 0.8769) (0.072 0.7943) (0.0875 0.7185))((0.3005 0.6403) (0.3637 0.5608) (0.4334 0.6046) (0.4999 0.6646) (0.5388 0.731) (0.5517 0.804) (0.5372 0.8834) (0.482 0.9612) (0.4091 1.0034) (0.32 1.0179) (0.3167 0.9158) (0.3945 0.885) (0.4269 0.8007) (0.4122 0.7402) (0.3815 0.697))))
       ("%%A" 0.539 (((0.2012 0.6581) (0.2466 0.5916) (0.3276 0.65) (0.2628 0.7262) (0.2109 0.778) (0.2919 0.7926) (0.3811 0.8169) (0.3487 0.9142) (0.266 0.8834) (0.1882 0.8477) (0.1995 0.9369) (0.2044 1.0179) (0.1055 1.0179) (0.1088 0.9563) (0.1185 0.8477) (0.0585 0.8769) (-0.0387 0.9142) (-0.0695 0.8169) (0.0083 0.7975) (0.0974 0.778) (0.0537 0.7343) (-0.016 0.65) (0.065 0.5916) (0.1039 0.65) (0.1525 0.7391))))
       ("(" 0.460 (((0.2853 -0.2934) (0.2432 -0.2221) (0.201 -0.1394) (0.1638 -0.0551) (0.1346 0.0276) (0.1103 0.1118) (0.0941 0.1945) (0.0843 0.2788) (0.0811 0.3615) (0.0843 0.4409) (0.0941 0.53) (0.1103 0.6192) (0.1427 0.7327) (0.1897 0.8461) (0.2286 0.9191) (0.2853 1.0179) (0.1962 1.0179) (0.1216 0.9077) (0.0762 0.8299) (0.039 0.7521) (-0.0081 0.6273) (-0.0291 0.5414) (-0.0405 0.4539) (-0.0453 0.3631) (-0.0405 0.2658) (-0.0275 0.1783) (-0.0048 0.0908) (0.0244 0.0065) (0.0584 -0.0681) (0.1005 -0.1491) (0.1459 -0.2237) (0.1962 -0.2934))))
       (")" 0.460 (((0.1751 -0.0794) (0.2173 0.0146) (0.2464 0.0989) (0.2675 0.1864) (0.2833 0.3052) (0.2837 0.4117) (0.274 0.5025) (0.2578 0.5884) (0.2318 0.6743) (0.1865 0.7829) (0.1459 0.8623) (0.0973 0.9401) (0.0422 1.0179) (-0.0453 1.0179) (-0.0113 0.9563) (0.0357 0.8721) (0.0746 0.7878) (0.1054 0.7035) (0.1297 0.616) (0.1443 0.5398) (0.154 0.4506) (0.1581 0.3276) (0.1476 0.2075) (0.1313 0.1248) (0.1087 0.0405) (0.0811 -0.0438) (0.0454 -0.1264) (0.0033 -0.2107) (-0.0453 -0.2934) (0.0422 -0.2934) (0.086 -0.2334) (0.133 -0.1589))))
       ("_" 0.772 (((0.6797 -0.2772) (-0.1324 -0.2772) (-0.1324 -0.188) (0.6797 -0.188))))
       ("+" 0.811 (((0.3543 0.1621) (0.3543 0.436) (0.6266 0.436) (0.6266 0.5511) (0.3543 0.5511) (0.3543 0.8234) (0.2376 0.8234) (0.2376 0.5511) (-0.0348 0.5511) (-0.0348 0.436) (0.2376 0.436) (0.2376 0.1621))))
       ("{" 0.460 (((0.3169 -0.188) (0.3169 -0.295) (0.2008 -0.2891) (0.1337 -0.2567) (0.0945 -0.2049) (0.0722 -0.1443) (0.0657 -0.0713) (0.0608 0.141) (0.0527 0.2026) (0.0073 0.2756) (-0.0769 0.3047) (-0.0769 0.4182) (0.0073 0.4474) (0.0527 0.5203) (0.0608 0.5819) (0.0657 0.7943) (0.0722 0.8672) (0.0945 0.9278) (0.1337 0.9796) (0.2008 1.012) (0.3169 1.0179) (0.3169 0.911) (0.2715 0.9091) (0.2153 0.8974) (0.1878 0.8508) (0.1792 0.7861) (0.1775 0.6241) (0.1678 0.5284) (0.1467 0.462) (0.1127 0.4133) (0.0365 0.3615) (0.1127 0.3096) (0.1467 0.261) (0.1678 0.1945) (0.1775 0.0989) (0.1792 -0.0632) (0.1878 -0.1279) (0.2153 -0.1744) (0.2715 -0.1862))))
       ("}" 0.460 (((-0.0769 0.911) (-0.0769 1.0179) (0.0392 1.012) (0.1063 0.9796) (0.1455 0.9278) (0.1678 0.8672) (0.1743 0.7943) (0.1792 0.5819) (0.1873 0.5203) (0.2327 0.4474) (0.3169 0.4182) (0.3169 0.3047) (0.2327 0.2756) (0.1873 0.2026) (0.1792 0.141) (0.1743 -0.0713) (0.1678 -0.1443) (0.1455 -0.2049) (0.1063 -0.2567) (0.0392 -0.2891) (-0.0769 -0.295) (-0.0769 -0.188) (-0.0315 -0.1862) (0.0247 -0.1744) (0.0522 -0.1279) (0.0608 -0.0632) (0.0625 0.0989) (0.0722 0.1945) (0.0933 0.261) (0.1273 0.3096) (0.2035 0.3615) (0.1273 0.4133) (0.0933 0.462) (0.0722 0.5284) (0.0625 0.6241) (0.0608 0.7861) (0.0522 0.8508) (0.0247 0.8974) (-0.0315 0.9091))))
       ("%%V" 0.356 (((0.1215 -0.2934) (0.0146 -0.2934) (0.0146 1.0179) (0.1215 1.0179))))
       ("%%Y" 0.384 (((0.1528 0.0) (0.1528 0.141) (0.0134 0.141) (0.0134 0.0))((0.1528 0.5852) (0.1528 0.7246) (0.0134 0.7246) (0.0134 0.5852))))
       ("%%Q" 0.490 (((0.0598 0.6467) (0.0906 0.8364) (0.0906 1.0001) (-0.0488 1.0001) (-0.0488 0.8364) (-0.0148 0.6467))((0.2851 0.6467) (0.3175 0.8364) (0.3175 1.0001) (0.1781 1.0001) (0.1781 0.8364) (0.2105 0.6467))))
       ("%%L" 0.811 (((0.1013 0.4944) (0.6265 0.71) (0.6265 0.8315) (-0.0365 0.5527) (-0.0365 0.4377) (0.6265 0.154) (0.6265 0.2772))))
       ("%%G" 0.811 (((-0.0363 0.154) (0.6267 0.4377) (0.6267 0.5527) (-0.0363 0.8315) (-0.0363 0.71) (0.4889 0.4944) (-0.0363 0.2772))))
       ("%%M" 0.848 (((0.3417 0.0) (0.3417 0.141) (0.2023 0.141) (0.2023 0.0))((0.1894 0.898) (0.2753 0.9158) (0.3385 0.9077) (0.413 0.8623) (0.4471 0.8186) (0.466 0.7623) (0.4643 0.6986) (0.4357 0.6435) (0.2753 0.4847) (0.2355 0.4241) (0.2147 0.3549) (0.2104 0.2464) (0.3271 0.2464) (0.3385 0.3534) (0.3741 0.4214) (0.5184 0.5641) (0.5638 0.6241) (0.5877 0.695) (0.5913 0.7894) (0.5622 0.8688) (0.5071 0.9385) (0.4487 0.979) (0.3677 1.0082) (0.2736 1.0179) (0.2039 1.0115) (0.1197 0.9871) (0.0516 0.9434) (0.0094 0.898) (-0.0295 0.8218) (-0.0505 0.7294) (0.0759 0.7132) (0.1002 0.8024) (0.144 0.8672))))
       ("~" 0.811 (((0.0443 0.4611) (0.1132 0.4784) (0.1765 0.4719) (0.3796 0.394) (0.455 0.3856) (0.5556 0.4024) (0.6479 0.4611) (0.6479 0.6036) (0.564 0.5449) (0.4916 0.5146) (0.4215 0.5198) (0.2203 0.5952) (0.1365 0.6036) (0.0443 0.5952) (-0.0563 0.5198) (-0.0563 0.3856))))
       ("%%D" 0.550 (((-0.0197 0.7954) (-0.0218 0.8376) (0.0167 0.9461) (0.0333 0.9627) (0.1411 1.0125) (0.1885 1.0126) (0.166 0.9461) (0.0831 0.9129) (0.0499 0.8299) (0.0831 0.7469) (0.166 0.7137) (0.1504 0.6417) (0.0499 0.6805) (0.0333 0.6971))((0.3487 0.806) (0.3486 0.8548) (0.2988 0.9627) (0.2822 0.9793) (0.1885 1.0126) (0.166 0.9461) (0.249 0.9129) (0.2822 0.8299) (0.249 0.7469) (0.166 0.7137) (0.1504 0.6417) (0.1991 0.6426) (0.2988 0.6971) (0.3154 0.7137))))
       ("%%C" 1.080 (((0.212 0.0322) (0.3055 0.0) (0.43 -0.0166) (0.5545 0.0) (0.6708 0.045) (0.7703 0.1245) (0.8532 0.249) (0.8947 0.3734) (0.903 0.4979) (0.8917 0.6244) (0.8615 0.7303) (0.7951 0.8465) (0.7122 0.7469) (0.7562 0.6224) (0.7703 0.4979) (0.7645 0.4055) (0.7296 0.2899) (0.6541 0.1826) (0.5452 0.1154) (0.4217 0.0996) (0.2972 0.1162) (0.2059 0.1826) (0.1229 0.0913))((0.1478 0.249) (0.0649 0.1494) (-0.0596 0.0166) (0.0068 -0.0415) (0.1229 0.0913) (0.2059 0.1826) (0.7122 0.7469) (0.7951 0.8465) (0.9196 0.9793) (0.8532 1.0373) (0.7371 0.9046) (0.6541 0.8133))((-0.0038 0.2657) (0.0649 0.1494) (0.1478 0.249) (0.098 0.3568) (0.0815 0.4979) (0.0897 0.5892) (0.1229 0.7137) (0.181 0.805) (0.3055 0.8797) (0.43 0.9046) (0.552 0.8834) (0.6541 0.8133) (0.7371 0.9046) (0.6626 0.9589) (0.5545 1.0042) (0.43 1.0208) (0.2972 1.0042) (0.181 0.9544) (0.0815 0.8797) (0.0068 0.7718) (-0.0347 0.6307) (-0.0491 0.5039) (-0.04 0.3859))))
       ("%%P" 0.772 (((0.6038 0.0) (0.6038 0.1162) (-0.0518 0.1162) (-0.0518 0.0))((0.3382 0.8382) (0.2221 0.8382) (0.2221 0.5643) (-0.0518 0.5643) (-0.0518 0.4481) (0.2221 0.4481) (0.2221 0.1743) (0.3382 0.1743) (0.3382 0.4481) (0.6038 0.4481) (0.6038 0.5643) (0.3382 0.5643))))
       ("a" 0.776 (((-0.0424 0.2081) (-0.0291 0.1194) (0.02 0.043) (0.0961 0.0015) (0.2206 -0.0143) (0.3424 0.0072) (0.4032 0.0349) (0.4714 0.086) (0.4427 0.1935) (0.3639 0.1075) (0.2349 0.0788) (0.1274 0.1146) (0.0916 0.1935) (0.1059 0.2508) (0.1561 0.2938) (0.2636 0.3153) (0.3567 0.3296) (0.4642 0.3654) (0.4642 0.4586) (0.3854 0.4371) (0.2421 0.4156) (0.1346 0.4012) (0.0486 0.3583) (-0.0159 0.2866))((0.0005 0.5811) (-0.023 0.5159) (0.0988 0.5016) (0.1237 0.5557) (0.1633 0.609) (0.2285 0.6314) (0.2991 0.6376) (0.387 0.6153) (0.4434 0.5731) (0.4612 0.519) (0.4642 0.4586) (0.4642 0.3654) (0.4642 0.3153) (0.4427 0.1935) (0.4714 0.086) (0.5 0.0) (0.6218 0.0) (0.5932 0.086) (0.586 0.1505) (0.586 0.4657) (0.5788 0.5804) (0.5556 0.6447) (0.4866 0.7017) (0.4033 0.7294) (0.3066 0.738) (0.2046 0.7309) (0.1078 0.7035) (0.0343 0.6449))))
       ("b" 0.776 (((0.129 0.6449) (0.1623 0.5583) (0.2078 0.6019) (0.2816 0.6321) (0.3747 0.6307) (0.4514 0.5732) (0.4872 0.4944) (0.5087 0.3654) (0.4944 0.2508) (0.4597 0.1762) (0.3947 0.1098) (0.3081 0.086) (0.2364 0.1003) (0.1669 0.157) (0.1146 0.0931) (0.2006 0.0072) (0.3296 -0.0143) (0.4442 0.0072) (0.517 0.0637) (0.5732 0.129) (0.6162 0.2364) (0.6305 0.3726) (0.609 0.5302) (0.5517 0.6377) (0.4514 0.7165) (0.3224 0.738) (0.2221 0.7165))((0.1623 0.5583) (0.129 0.6449) (0.129 0.996) (0.0 0.996) (0.0 0.0) (0.1146 0.0) (0.1146 0.0931) (0.1669 0.157) (0.1227 0.2641) (0.1146 0.3654) (0.129 0.4729))))
       ("c" 0.690 (((0.3344 0.6295) (0.4116 0.5777) (0.4518 0.4944) (0.5736 0.5087) (0.5131 0.6488) (0.4271 0.7111) (0.3275 0.7344) (0.2297 0.738) (0.1393 0.7084) (0.0512 0.6521) (-0.0068 0.566) (-0.0283 0.4872) (-0.0407 0.3961) (-0.0426 0.2938) (-0.0139 0.172) (0.0434 0.0788) (0.158 0.0) (0.287 -0.0143) (0.3801 0.0) (0.4876 0.0573) (0.5449 0.129) (0.5879 0.2508) (0.4661 0.2651) (0.4285 0.1635) (0.3628 0.1047) (0.2798 0.086) (0.2026 0.0982) (0.1365 0.1505) (0.0924 0.2517) (0.0792 0.3654) (0.0935 0.4801) (0.1365 0.5732) (0.2362 0.6309))))
       ("d" 0.776 (((0.114 0.7035) (0.0454 0.6403) (-0.0087 0.5589) (-0.0302 0.4872) (-0.041 0.3873) (-0.0374 0.2794) (-0.0121 0.1873) (0.0271 0.1146) (0.0815 0.048) (0.176 -0.002) (0.2707 -0.0143) (0.3854 0.0072) (0.4714 0.0931) (0.414 0.1505) (0.3628 0.1087) (0.2779 0.086) (0.2068 0.0987) (0.1418 0.1505) (0.0988 0.2293) (0.0844 0.3583) (0.0916 0.4729) (0.1229 0.5534) (0.1857 0.6133) (0.2509 0.636) (0.3385 0.6235) (0.408 0.5731) (0.4642 0.6377) (0.3782 0.7165) (0.288 0.7367) (0.1816 0.73))((0.4642 0.996) (0.4642 0.6377) (0.408 0.5731) (0.457 0.4872) (0.4714 0.3511) (0.4642 0.2508) (0.414 0.1505) (0.4714 0.0931) (0.4714 0.0) (0.586 0.0) (0.586 0.996))))
       ("e" 0.776 (((0.0888 0.3296) (0.096 0.4299) (0.1055 0.4915) (0.1407 0.5594) (0.2004 0.6156) (0.2709 0.6374) (0.3539 0.6305) (0.4244 0.5933) (0.478 0.5113) (0.4972 0.4299) (0.6262 0.4299) (0.5975 0.5445) (0.5579 0.6147) (0.4924 0.6776) (0.4184 0.7165) (0.3396 0.7349) (0.2502 0.7367) (0.1644 0.7182) (0.0699 0.6553) (0.0069 0.5757) (-0.0258 0.4872) (-0.0402 0.3995) (-0.0402 0.2938) (-0.0219 0.1974) (0.032 0.1018) (0.0917 0.0448) (0.1748 0.0) (0.284 -0.017) (0.4041 -0.0072) (0.5115 0.043) (0.5807 0.1206) (0.619 0.215) (0.4972 0.2293) (0.4685 0.176) (0.4184 0.1218) (0.3216 0.0919) (0.2192 0.1027) (0.1533 0.1505) (0.1089 0.2243))((0.6262 0.4299) (0.6262 0.3296) (0.0888 0.3296) (0.096 0.4299) (0.4972 0.4299))))
       ("f" 0.390 (((0.1163 0.9888) (0.0518 0.91) (0.0375 0.8025) (0.0375 0.7237) (-0.07 0.7237) (-0.07 0.6305) (0.0375 0.6305) (0.0375 0.0) (0.1593 0.0) (0.1593 0.6305) (0.3026 0.6305) (0.3026 0.7237) (0.1593 0.7237) (0.1616 0.8099) (0.1808 0.8813) (0.2596 0.9028) (0.3312 0.8956) (0.3527 1.0031) (0.2381 1.0174))))
       ("g" 0.776 (((0.1003 0.701) (0.0319 0.6408) (-0.0159 0.5589) (-0.0455 0.4492) (-0.0517 0.3296) (-0.0302 0.215) (0.0271 0.1075) (0.1346 0.0215) (0.2564 0.0) (0.3639 0.0215) (0.457 0.0931) (0.414 0.1648) (0.3187 0.1109) (0.221 0.1099) (0.1274 0.1648) (0.0916 0.2364) (0.0701 0.3726) (0.0844 0.4729) (0.1169 0.5566) (0.1809 0.6164) (0.2548 0.637) (0.3393 0.6243) (0.4069 0.5732) (0.4714 0.6377) (0.371 0.7165) (0.291 0.7348) (0.1861 0.734))((0.3854 -0.1576) (0.2963 -0.1886) (0.2072 -0.1886) (0.1274 -0.1576) (0.0916 -0.0788) (-0.0302 -0.0573) (-0.0159 -0.1433) (0.0486 -0.2364) (0.1274 -0.2794) (0.2325 -0.2939) (0.3647 -0.2807) (0.4714 -0.2364) (0.5502 -0.1433) (0.5717 -0.0502) (0.586 0.1003) (0.586 0.7237) (0.4714 0.7237) (0.4714 0.6377) (0.4069 0.5732) (0.4499 0.5016) (0.4714 0.3726) (0.457 0.2651) (0.414 0.1648) (0.457 0.0931) (0.4499 -0.0645))))
       ("h" 0.776 (((0.4642 0.4586) (0.4642 0.0) (0.586 0.0) (0.586 0.5087) (0.5573 0.6234) (0.4714 0.7093) (0.3439 0.738) (0.2221 0.7165) (0.129 0.6377) (0.129 0.996) (0.0 0.996) (0.0 0.0) (0.129 0.0) (0.129 0.3941) (0.1433 0.5302) (0.215 0.609) (0.3224 0.6377) (0.3963 0.612) (0.4483 0.5492))))
       ("i" 0.315 (((0.125 0.7237) (0.0 0.7237) (0.0 0.0) (0.125 0.0))((0.125 0.996) (0.0 0.996) (0.0 0.8598) (0.125 0.8598))))
       ("j" 0.315 (((-0.0207 -0.1648) (-0.0685 -0.1863) (-0.1258 -0.1791) (-0.1544 -0.2794) (-0.0541 -0.2938) (0.021 -0.2776) (0.0892 -0.2293) (0.1188 -0.1371) (0.125 -0.043) (0.125 0.7237) (0.0 0.7237) (0.0 -0.0358) (-0.0025 -0.107))((0.125 0.996) (0.0 0.996) (0.0 0.8598) (0.125 0.8598))))
       ("k" 0.690 (((0.6019 0.0) (0.3009 0.4586) (0.5732 0.7237) (0.4156 0.7237) (0.129 0.4299) (0.129 0.996) (0.0 0.996) (0.0 0.0) (0.129 0.0) (0.129 0.2866) (0.215 0.3726) (0.4514 0.0))))
       ("l" 0.315 (((0.125 0.0) (0.0 0.0) (0.0 0.996) (0.125 0.996))))
       ("m" 1.172 (((0.1075 0.6234) (0.1075 0.7237) (0.0 0.7237) (0.0 0.0) (0.1218 0.0) (0.1218 0.3726) (0.1249 0.4553) (0.1433 0.5302) (0.2078 0.609) (0.3009 0.6305) (0.3721 0.6104) (0.4108 0.5644) (0.4299 0.4729) (0.4299 0.0) (0.5517 0.0) (0.5517 0.4227) (0.5659 0.5097) (0.5995 0.5718) (0.6592 0.6163) (0.7308 0.6305) (0.8025 0.6162) (0.8455 0.5589) (0.8598 0.4586) (0.8598 0.0) (0.982 0.0) (0.982 0.4944) (0.9745 0.5804) (0.9449 0.6521) (0.8813 0.7165) (0.7991 0.735) (0.7034 0.7331) (0.6234 0.7022) (0.5374 0.6162) (0.4586 0.7093) (0.3296 0.738) (0.2006 0.7093))))
       ("n" 0.776 (((0.2386 0.6174) (0.3153 0.6305) (0.4012 0.609) (0.4514 0.5517) (0.4657 0.4371) (0.4657 0.0) (0.586 0.0) (0.586 0.4442) (0.586 0.5514) (0.5445 0.652) (0.4657 0.7165) (0.3614 0.7387) (0.2552 0.7261) (0.1809 0.6921) (0.1146 0.6234) (0.1146 0.7237) (0.0 0.7237) (0.0 0.0) (0.1218 0.0) (0.1218 0.3941) (0.1361 0.4944) (0.1641 0.566))))
       ("o" 0.776 (((0.1785 -0.0005) (0.2702 -0.0137) (0.3099 0.0867) (0.2159 0.1009) (0.1388 0.1505) (0.0975 0.2448) (0.0815 0.3583) (0.0958 0.4729) (0.1277 0.5576) (0.1937 0.6096) (0.2615 0.6322) (0.2964 0.738) (0.1675 0.7165) (0.0671 0.6592) (0.017 0.6019) (-0.0332 0.4944) (-0.0471 0.3879) (-0.0403 0.2938) (-0.0239 0.2069) (0.0226 0.111) (0.0903 0.0466))((0.4592 0.1637) (0.4039 0.1146) (0.3099 0.0867) (0.2702 -0.0137) (0.3802 -0.0089) (0.4684 0.0287) (0.5293 0.0733) (0.5902 0.1505) (0.6189 0.2364) (0.633 0.3244) (0.6273 0.4583) (0.5974 0.5517) (0.54 0.6449) (0.4182 0.7237) (0.2964 0.738) (0.2615 0.6322) (0.3466 0.6305) (0.4469 0.5732) (0.4899 0.4944) (0.5042 0.3654) (0.4971 0.2508))))
       ("p" 0.776 (((0.3949 0.1062) (0.3081 0.086) (0.2372 0.0999) (0.1648 0.1505) (0.1218 0.0717) (0.2006 0.0072) (0.2901 -0.0132) (0.382 -0.0094) (0.4542 0.0178) (0.5236 0.0702) (0.5875 0.1648) (0.6162 0.2436) (0.6305 0.3654) (0.6234 0.4442) (0.6056 0.5308) (0.566 0.609) (0.5112 0.6753) (0.4196 0.7237) (0.3224 0.738) (0.2006 0.7165) (0.1146 0.6305) (0.1586 0.5556) (0.2142 0.6119) (0.2829 0.6418) (0.3566 0.6344) (0.4279 0.5904) (0.4872 0.5016) (0.5016 0.3654) (0.4944 0.2508) (0.4595 0.1681))((0.1218 -0.2794) (0.1218 0.0717) (0.1648 0.1505) (0.129 0.2221) (0.1146 0.3583) (0.1218 0.4729) (0.1586 0.5556) (0.1146 0.6305) (0.1146 0.7237) (0.0 0.7237) (0.0 -0.2794))))
       ("q" 0.776 (((0.3983 0.589) (0.457 0.4872) (0.4785 0.3511) (0.4642 0.2508) (0.432 0.1657) (0.4642 0.0788) (0.4642 -0.2794) (0.586 -0.2794) (0.586 0.7237) (0.4785 0.7237) (0.4785 0.6305))((-0.0302 0.4944) (-0.0445 0.3654) (-0.0334 0.2639) (0.0019 0.1524) (0.0558 0.0788) (0.1561 0.0072) (0.2707 -0.0143) (0.3854 0.0072) (0.4642 0.0788) (0.432 0.1657) (0.368 0.107) (0.2851 0.086) (0.211 0.0974) (0.1418 0.1576) (0.0979 0.2546) (0.0844 0.3654) (0.0988 0.4801) (0.1294 0.5568) (0.18 0.6148) (0.246 0.6413) (0.3137 0.6377) (0.3983 0.589) (0.4785 0.6305) (0.4047 0.7017) (0.319 0.7305) (0.2308 0.7376) (0.1371 0.7115) (0.0701 0.6664) (-0.0015 0.566))))
       ("r" 0.466 (((0.1075 0.6162) (0.1075 0.7237) (0.0 0.7237) (0.0 0.0) (0.1218 0.0) (0.1218 0.3798) (0.1309 0.4767) (0.1665 0.5664) (0.2323 0.6151) (0.2988 0.6097) (0.3511 0.5875) (0.3869 0.7022) (0.3395 0.7232) (0.2651 0.738) (0.2016 0.724) (0.1525 0.6862))))
       ("s" 0.690 (((0.2548 0.4371) (0.1473 0.4729) (0.0972 0.5016) (0.0859 0.5588) (0.1201 0.6129) (0.2152 0.6357) (0.298 0.6305) (0.3702 0.595) (0.3981 0.5231) (0.5199 0.5374) (0.4769 0.652) (0.4123 0.7026) (0.3173 0.7308) (0.2333 0.738) (0.1258 0.7237) (0.047 0.6879) (-0.0103 0.6234) (-0.0318 0.5302) (-0.0103 0.4371) (0.0685 0.3654) (0.2691 0.3009) (0.3838 0.2651) (0.4196 0.2006) (0.3984 0.1504) (0.3542 0.1068) (0.2917 0.0902) (0.2188 0.088) (0.1458 0.1101) (0.0933 0.1708) (0.0685 0.2364) (-0.0533 0.215) (-0.0318 0.1361) (0.0234 0.0574) (0.1029 0.0052) (0.2075 -0.0144) (0.2906 -0.0143) (0.3764 0.0019) (0.4515 0.0367) (0.5234 0.1162) (0.5486 0.215) (0.5199 0.3153) (0.4602 0.3647) (0.3909 0.4012))))
       ("t" 0.390 (((0.0952 0.0143) (0.1955 -0.0072) (0.2887 0.0) (0.2672 0.1075) (0.217 0.1075) (0.1741 0.1146) (0.1478 0.1494) (0.1454 0.2078) (0.1454 0.6305) (0.2672 0.6305) (0.2672 0.7237) (0.1454 0.7237) (0.1454 0.9745) (0.0236 0.9028) (0.0236 0.7237) (-0.0624 0.7237) (-0.0624 0.6305) (0.0236 0.6305) (0.0236 0.215) (0.0271 0.1195) (0.0459 0.0543))))
       ("u" 0.776 (((0.0072 0.1576) (0.043 0.0717) (0.129 0.0072) (0.2116 -0.0104) (0.3178 -0.0068) (0.3854 0.0215) (0.4714 0.1075) (0.4714 0.0) (0.586 0.0) (0.586 0.7237) (0.4642 0.7237) (0.4642 0.3368) (0.4427 0.1935) (0.371 0.1146) (0.2707 0.086) (0.1791 0.1146) (0.129 0.1935) (0.1218 0.3224) (0.1218 0.7237) (0.0 0.7237) (0.0 0.2723))))
       ("v" 0.690 (((0.3109 0.0) (0.5832 0.7237) (0.4542 0.7237) (0.2464 0.1433) (0.0458 0.7237) (-0.0832 0.7237) (0.1963 0.0))))
       ("w" 1.011 (((0.5538 0.0) (0.6828 0.0) (0.9049 0.7237) (0.7903 0.7237) (0.6183 0.1648) (0.475 0.7237) (0.3532 0.7237) (0.1955 0.1505) (0.0379 0.7237) (-0.0839 0.7237) (0.1382 0.0) (0.26 0.0) (0.4105 0.5589))))
       ("x" 0.690 (((0.2464 0.2866) (0.4398 0.0) (0.5903 0.0) (0.318 0.3798) (0.5688 0.7237) (0.4255 0.7237) (0.2464 0.4729) (0.0816 0.7237) (-0.0689 0.7237) (0.1747 0.3726) (-0.0904 0.0) (0.0601 0.0))))
       ("y" 0.690 (((0.0386 -0.172) (-0.033 -0.1648) (-0.0187 -0.2794) (0.0698 -0.2911) (0.1604 -0.2651) (0.2195 -0.2097) (0.2536 -0.1433) (0.5832 0.7237) (0.4613 0.7237) (0.2536 0.1361) (0.0529 0.7237) (-0.0832 0.7237) (0.1962 0.0) (0.1461 -0.1218) (0.1031 -0.1648))))
       ("z" 0.690 (((0.5748 0.0) (0.5748 0.1075) (0.238 0.1075) (0.0875 0.1003) (0.5533 0.6449) (0.5533 0.7237) (-0.0414 0.7237) (-0.0414 0.6234) (0.2523 0.6234) (0.3956 0.6305) (-0.0701 0.1003) (-0.0701 0.0))))
       ("%%H" 1.356 (((-0.073 -0.104) (0.0605 -0.104) (1.2087 1.5309) (1.0752 1.5309))))
       ("%%130" 1.000 (((0.4813 -0.0107) (0.5382 -0.0049) (0.6208 0.0243) (0.7132 0.0875) (0.7684 0.1507) (0.8105 0.2269) (0.8413 0.3177) (0.7084 0.3517) (0.6581 0.2221) (0.6046 0.1605) (0.5362 0.1224) (0.4813 0.1055) (0.4813 0.3981) (0.35 0.3981) (0.35 0.1057) (0.299 0.1177) (0.2383 0.1491) (0.1751 0.2075) (0.1167 0.3274) (0.1005 0.4133) (0.094 0.509) (0.1038 0.6143) (0.1297 0.7116) (0.1719 0.7878) (0.2561 0.8623) (0.3339 0.8931) (0.4263 0.9045) (0.428 0.9045) (0.5187 0.8915) (0.5933 0.8542) (0.6549 0.7845) (0.6938 0.6954) (0.8235 0.7262) (0.791 0.8105) (0.744 0.8834) (0.6792 0.9434) (0.6063 0.9839) (0.5236 1.0098) (0.4296 1.0179) (0.3534 1.0131) (0.2675 0.992) (0.1881 0.958) (0.1281 0.9174) (0.0665 0.8558) (0.0179 0.7813) (-0.0178 0.6889) (-0.0356 0.603) (-0.0421 0.5009) (-0.0356 0.4101) (-0.0178 0.3226) (0.0114 0.2399) (0.0454 0.1734) (0.0989 0.1037) (0.1654 0.0502) (0.2448 0.0113) (0.3018 -0.0046) (0.35 -0.0111) (0.35 -0.602) (0.8388 -0.602) (0.8388 -0.4837) (0.4813 -0.4837))))
       ("%%141" 1.714 (("%%H" (0.0236 0.0000))("1" (-0.0272 0.4992))("1" (0.4620 -0.2172))("6" (1.0057 -0.2142))))
       ("%%142" 1.239 (("%%H" (0.0265 0.0000))("1" (-0.0243 0.4992))("8" (0.5308 -0.2145))))
       ("%%143" 1.915 (("%%H" (0.2245 0.0000))("3" (0.0381 0.5002))("1" (0.6659 -0.2142))("6" (1.2066 -0.2142))))
       ("%%144" 1.262 (("%%H" (0.0256 0.0000))("1" (-0.0272 0.4992))("4" (0.5576 -0.2021))))
       ("%%145" 1.918 (("%%H" (0.2303 0.0000))("5" (0.0367 0.5000))("1" (0.6688 -0.2142))("6" (1.2096 -0.2142))))
       ("%%146" 1.440 (("%%H" (0.2266 0.0000))("3" (0.0381 0.4973))("8" (0.7317 -0.2174))))
       ("%%147" 1.909 (("%%H" (0.2187 0.0000))("7" (0.0340 0.5000))("1" (0.6600 -0.2142))("6" (1.2008 -0.2142))))
       ("%%148" 1.239 (("%%H" (0.0256 0.0000))("1" (-0.0272 0.4992))("2" (0.5398 -0.2038))))
       ("%%149" 1.918 (("%%H" (0.2295 0.0000))("9" (0.0365 0.4988))("1" (0.6659 -0.2172))("6" (1.2096 -0.2172))))
       ("%%150" 1.446 (("%%H" (0.2324 0.0000))("5" (0.0396 0.4971))("8" (0.7346 -0.2174))))
       ("%%151" 2.255 (("%%H" (0.5681 0.0000))("1" (-0.0272 0.4992))("1" (0.5144 0.4992))("1" (1.0066 -0.2142))("6" (1.5474 -0.2142))))
       ("%%152" 1.463 (("%%H" (0.2274 0.0000))("3" (0.0381 0.5002))("4" (0.7585 -0.1992))))
       ("%%153" 2.392 (("%%H" (0.7021 0.0000))("1" (-0.0272 0.4992))("3" (0.5157 0.5002))("1" (1.1435 -0.2172))("6" (1.6842 -0.2142))))
       ("%%154" 1.434 (("%%H" (0.2187 0.0000))("7" (0.0310 0.5000))("8" (0.7230 -0.2145))))
       ("%%155" 2.398 (("%%H" (0.7108 0.0000))("1" (-0.0243 0.4992))("5" (0.5172 0.5000))("1" (1.1493 -0.2142))("6" (1.6901 -0.2142))))
      )
    )
  )
)

(princ "\n3D Text LISP Routine Loaded. Type \"3DText\" to Run.")
(princ)

