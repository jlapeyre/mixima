(defvar mixima-limit-option-hash  (make-hash-table :test #'equal ))
; should be factored out again

(defun limit-set-option-args ( &aux direction args)
  (setq direction (gethash '|$Direction|  mixima-limit-option-hash))
  (setq args (list direction))
  args)

#|> Function Limit |#
(defmfun |$Limit| (expr limitargrule &rest inargs &aux optargs limitvar limitarg direction args) 
  (clrhash mixima-limit-option-hash)
  (setq args (mixima-collect-rules mixima-limit-option-hash inargs)) ; filter rules from args
  (setq optargs (limit-set-option-args))
  (setq args (rest (first args)))
  (cond ( (rulep limitargrule)
          (setf limitvar (second limitargrule))
          (setf limitarg (third limitargrule))))
  (setq direction (first optargs))
  (cond ( (eq nil direction)
          (mapply '$limit (list expr limitvar limitarg)))
        ( (eq 1 direction )
          (mapply '$limit (list expr limitvar limitarg '$minus)))
        ( t
          (mapply '$limit (list expr limitvar limitarg '$plus)))))
