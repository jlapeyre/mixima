;; These are called by mixpart and smixpart
(defun mset-part (expr repl inds &aux (i (first inds)))
  (cond ( (listp expr)
          (when (< i 0) (setf i (+ (length expr) i)))
          (cond ( (eq 1 (length inds))
                  (cond( (> i (- (length expr) 1))
                         ($error "get-part: indices out of bounds."))
                       ( (> i 0)
                          (setf (elt expr  i) repl))
                        ( t
                          (cond ( (symbolp repl)
                                  (setf (elt expr  i) (list repl) ))
                                (t
                                 ($error "smixpart can't set  op to non-symbol."))))))
                (t
                 (mset-part (elt expr  i) repl (rest inds)))))
        (t  ($error "smixpart: part specification longer than depth of object."))))

(defun mget-part (expr inds &aux (i (elt inds 0)))
  (cond ( (listp expr)
          (when (< i 0) (setf i (+ (length expr) i)))
          (cond ((eq 1 (length inds))
                 (cond ( (> i (- (length expr) 1))
                         ($error "get-part: indices out of bounds."))
                       ( (> i 0)
                         (elt expr i))
                       (t (first (elt expr i)))))
                (t
                 (mget-part (elt expr i) (rest inds)))))
        (t  ($error "get-part: part specification longer than depth of object."))))


#|> AuxFunction mixpart |#
(defmspec $mixpart (x)
  (setf x (cdr x))
  (setf x (mapcar 'meval x))
  (mget-part (first x) (rest x)))

#|> AuxFunction smixpart |#
(defmspec $smixpart (x &aux expr repl)
  (setf x (cdr x))
  (setf x (mapcar 'meval x))
  (setf repl (first x))
  (setf expr (copy-tree (second x)))
  (mset-part expr repl (rest (rest x)))
  (meval expr))

#|> Function Part |#
(defmspec |$Part| (x)
  (mapply '$mixpart (rest x)))

#|

  Negative part numbers now supported. But using new not well tested mixpart function
  Probably should quote args in smixpart call to prevent another meval.
  MapAt(f,[a,b,c],0) is different here than in Mma; we should fix this.
  Not much to do...
   MapAt[f,{a,b,c,d},0]
    f[List][a, b, c, d]

  But f("[")(a,b,c,d) is not allowed in maxima, we could do
  f["["](a,b,c,d) but I am not sure that is better than
   f[a,b,c,d], which is what we do now, but is wrong.
  
|#

#|> Function MapAt |#
(defmix |$MapAt| ( func expr (pos-specs :pos-spec) &aux subexpr newexpr )
  (setq pos-specs (mixima-normalize-position-spec  pos-specs))
  (dolist (one-spec (rest pos-specs))
    (setf subexpr (mapply '$mixpart  (cons expr (rest one-spec))))
    (cond ( (equal 0 (first (last one-spec)))
            (setf newexpr (apply 'mfuncall (cons '$smixpart
                                                 (cons  func  (cons expr (rest one-spec)))))))
          (t 
           (setf newexpr (apply 'mfuncall (cons '$smixpart
                                                (cons (list (list func) subexpr) (cons expr (rest one-spec))))))))
    (setf expr newexpr))
  expr)


#| before defmix, but worked
(defmspec |$MapAt| (x &aux body  func efunc inexpr expr pos-specs subexpr newexpr )
  (cond ( (mixima-check-args x '(:nargs-spec 3 :pos-spec-n 3))
          (setq body (rest x))
          (setq func (first body))
          (setq inexpr (second body))
;;          (setq pos-specs (getf newargs ':pos-spec))
          (setq pos-specs (mixima-normalize-position-spec (meval (third body))))
          (cond ( pos-specs
                  (setf expr (meval inexpr))
                  (setf efunc (meval func))
                  (dolist (one-spec (rest pos-specs))
                    (setf subexpr (mapply '$mixpart  (cons expr (rest one-spec))))
                    (cond ( (equal 0 (first (last one-spec)))
                            (setf newexpr (apply 'mfuncall (cons '$smixpart
                                                                 (cons  efunc  (cons expr (rest one-spec)))))))
                          (t 
                           (setf newexpr (apply 'mfuncall (cons '$smixpart
                                      (cons (list (list efunc) subexpr) (cons expr (rest one-spec))))))))
                    (setf expr newexpr))
                  expr)
                (t x)))
        (t x)))

|#

#|

  This works in a limited set of cases.
  ReplaceAll(expr,[Rule(x,y),Rule(a,b)]
  subst([x=y,a=b], expr)
  Thats all.

|#

;; This is wrong. We must quit after first success.
#|> Function ReplaceAll |#
(defmix |$ReplaceAll| (expr rules &aux eqs res)
  (if (not ($listp rules) ) (setf rules (list '(mlist simp) rules)))
  (setf eqs (cons '(mlist simp) (mapcar #'(lambda (r) (list '(mequal simp) (second r) (third r))) (rest rules) )))
  (setq res (mfuncall '$substitute eqs expr)))


#| remove this one soon
(defmspec |$ReplaceAll| (x &aux body expr rules eqs res)
  (setq body (rest x))
  (setf expr (first body))
  (setf rules (second body))
  (cond ( (mixima-check-args x '(:nargs-spec 2))
          (if (not ($listp rules) ) (setf rules (list '(mlist simp) rules)))
          (setf eqs (cons '(mlist simp) (mapcar #'(lambda (r) (list '(mequal simp) (second r) (third r))) (rest rules) )))
          (setq res (mfuncall '$substitute eqs expr))
          res)
        (t
         x)))
|#
