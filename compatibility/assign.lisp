;; experiment with assignment
;; unless var is a call to inpart, mixSetQ(var,val) tries to assign val to  var
;; if var is a call to inpart(var1,partnum) , call substinpart to substitute val for
;; part partnum of var1 and finally set var1 to the result (inplace might be better.
;; return val.   If we use infix operator => to call this function, then we can do
;; x => 3 for normal assignment, or
;; inpart(a,n) => 3 to try to assign 3 to part number n of a
;;  Note:  =~ caused a bug somewhere in a format statement.

;; If assigning to a part, we make a copy and change the part and
;; call msetq to rebind the variable. I think mma does this destructively.
;; We could change smixpart to  do a destructive change instead. I don't
;; know what that might break.

(defmspec $=> (x &aux var val varop partex partspec (subres nil) newvar )
  (setq x (cdr x))
  (setq var (first x))
  (setq val (meval (second x)))
  (cond ( (listp var)
          (setq varop (first (first var)))
          (cond
           ( (equalp varop '$inpart)
             (setq partex (second var))
             (setq partspec (rest (rest var)))
             (setq subres (apply 'mfuncall (append (list '$substinpart val partex ) partspec)))
             (if subres (meval (cons '(msetq simp) (list partex subres)))))
           ( (or (equalp varop '$mixpart) (equalp varop '|$Part|))
             (setq partex (second var))
             (setq partspec (rest (rest var)))
             (setq subres (apply 'mfuncall (append (list '$smixpart val partex ) partspec)))
             (if subres (meval (cons '(msetq simp) (list partex subres)))))
           ( (equalp varop '$part)
             (setq partex (second var))
             (setq partspec (rest (rest var)))
             (setq subres (apply 'mfuncall (append (list '$substpart val partex ) partspec)))
             (if subres (meval (cons '(msetq simp) (list partex subres)))))
           ( (equalp (last (first var)) '(ARRAY))
             (meval (cons '(msetq simp) (list var val)))) ; try if its an array
           ( t
             (setq newvar (cons (append (first var) '(array)) (rest var)))
             (meval (cons '(msetq simp) (list newvar val)))))) ; coerce to array
        ( t
          (meval (cons '(msetq simp) (list var val)))))
  val)
