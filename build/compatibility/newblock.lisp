;; This is modified block code.
;; A bit dangerous I suppose.
;; So far, all we have changed  is to allow both  :  and => in the variable list rather than just :
;; In mixima_block.lisp, this is called by Block, etc.

(defmspec mixprog (prog)
  (setq prog (cdr prog))
  (let (vars vals (mlocp t))
    (if ($listp (car prog)) (setq vars (cdar prog) prog (cdr prog)))
    (setq loclist (cons nil loclist))
    (do ((l vars (cdr l))) ((null l) (setq vals vars))
      (if (not (atom (car l))) (return (setq vals t))))
    (if (eq vals t)
	(setq vals (mapcar #'(lambda (v)
			       (cond ((atom v) v)
				     ((or (eq (caar v) 'msetq) (eq (caar v) '$=>) ) (meval (caddr v)))
				     (t (merror
					 (intl:gettext "myblock: variable list must comprise only atoms and assignment expressions; found: ~M")
					 v))))
			   vars)
	      vars (mapcar #'(lambda (v) (if (atom v) v (cadr v))) vars)))
    (mbinding (vars vals)
	      (do ((prog prog (cdr prog)) (mprogp prog)
		   (bindl bindlist) (val '$done) (retp) (x) ($%% '$%%))
		  ((null prog) (munlocal) val)
		(cond ((atom (car prog))
		       (if (null (cdr prog))
			   (setq retp t val (meval (car prog)))))
		      ((null (setq x (catch 'mprog
				       (prog2 (setq val (setq $%% (meval (car prog))))
					   nil)))))
		      ((not (eq bindl bindlist))
		       (if (not (atom x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'return': ~M") (car x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'go': ~M") x)))
		      ((not (atom x)) (setq retp t val (car x)))
		      ((not (setq prog (member x mprogp :test #'equal)))
		       (merror (intl:gettext "block: no such tag: ~:M") x)))
		(if retp (setq prog '(nil)))))))

#| This is how return is done

(defmfun mreturn (&optional (x nil) &rest args)
  (cond 
    ((not (null args))
       (merror (intl:gettext "return: too many arguments; found: ~M") `((mlist) ,x ,@args) ))
    ((and (not mprogp) (not mdop))
       (merror (intl:gettext "return: not within 'block'")))
    (t (throw 'mprog (ncons x)) ) ))

|#

#|

Perhaps this is the better way to do what I have in mixima_block.lisp

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'alias)
		(putprop (cadr x) (car x) 'reversealias))
      '(($block mprog block) ($lambda lambda lambda)
	($subst $substitute subst)
	($go mgo go) ($signum %signum signum)
	($return mreturn return) ($factorial mfactorial factorial)
	($nouuo nouuo nouuo) ($rset *rset rset)
        ($ibase *read-base* *read-base*) ($obase *print-base* obase)
        ($nopoint *nopoint nopoint)
	($modulus modulus modulus) ($zunderflow zunderflow zunderflow)
	($ttyoff #.ttyoff ttyoff) ($writefile_on #.writefilep writefile_on)
	($mode_declare $modedeclare mode_declare)))


|#

#|
;; This is an untouched copy of the original maxima block (mprogn) code.
;; It is renamed, so it is not used.
(defmspec originalmprog (prog)
  (setq prog (cdr prog))
  (let (vars vals (mlocp t))
    (if ($listp (car prog)) (setq vars (cdar prog) prog (cdr prog)))
    (setq loclist (cons nil loclist))
    (do ((l vars (cdr l))) ((null l) (setq vals vars))
      (if (not (atom (car l))) (return (setq vals t))))
    (if (eq vals t)
	(setq vals (mapcar #'(lambda (v)
			       (cond ((atom v) v)
				     ((eq (caar v) 'msetq) (meval (caddr v)))
				     (t (merror
					 (intl:gettext "block: variable list must comprise only atoms and assignment expressions; found: ~M")
					 v))))
			   vars)
	      vars (mapcar #'(lambda (v) (if (atom v) v (cadr v))) vars)))
    (mbinding (vars vals)
	      (do ((prog prog (cdr prog)) (mprogp prog)
		   (bindl bindlist) (val '$done) (retp) (x) ($%% '$%%))
		  ((null prog) (munlocal) val)
		(cond ((atom (car prog))
		       (if (null (cdr prog))
			   (setq retp t val (meval (car prog)))))
		      ((null (setq x (catch 'mprog
				       (prog2 (setq val (setq $%% (meval (car prog))))
					   nil)))))
		      ((not (eq bindl bindlist))
		       (if (not (atom x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'return': ~M") (car x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'go': ~M") x)))
		      ((not (atom x)) (setq retp t val (car x)))
		      ((not (setq prog (member x mprogp :test #'equal)))
		       (merror (intl:gettext "block: no such tag: ~:M") x)))
		(if retp (setq prog '(nil)))))))

|#
