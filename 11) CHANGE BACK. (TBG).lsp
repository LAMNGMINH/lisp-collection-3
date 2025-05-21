(defun c:tbg ( / col )
    (if tbg:flg
        (setq col 16777215 tbg:flg nil)
        (setq col 0        tbg:flg  t )
    )
    (foreach prp '(graphicswinmodelbackgrndcolor modelcrosshaircolor)
        (vlax-put-property (acdisp) prp (setq col (- 16777215 col)))
    )
    (princ)
)
(defun acdisp nil
    (eval
        (list 'defun 'acdisp 'nil
            (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
        )
    )
    (acdisp)
)
(vl-load-com) (princ)