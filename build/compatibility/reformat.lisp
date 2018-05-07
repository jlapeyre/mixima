#|

 This file contains routines that reformat the output of the Mma to Macsyma
 translator.

 This one sets grind to true below!

|#

; This is the same as stringout, except that
; newstringout(filename,[n,m], nl) prints nl newlines after every input line
(defmspec $newstringout (x)
  (setq x (cdr x))
  (let*
    ((file (namestring (maxima-string (meval (car x)))))
     (filespec (if (or (eq $file_output_append '$true) (eq $file_output_append t))
	`(savefile ,file :direction :output :if-exists :append :if-does-not-exist :create)
	`(savefile ,file :direction :output :if-exists :supersede :if-does-not-exist :create))))
    (setq x (cdr x))
    (eval
      `(let (maxima-error l1 truename (newlines 
				   (if (and ($listp (first ',x)) (eq (length ',x) 2))
	                       (second ',x) 0)))
	(declare (special $grind $strdisp))
           (setf $grind t)
           (with-open-file ,filespec
	      (cond ((null
		      (errset
		       (do ((l ',x (cdr l)))( (null l))
			 (cond ((member (car l) '($all $input) :test #'equal)
				(setq l (nconc (getlabels* $inchar t) (cdr l))))
			       ((eq (car l) '$values)
				(setq l (nconc (mapcan
						#'(lambda (x)
						    (if (boundp x)
							(ncons (list '(msetq) x (symbol-value x)))))
						(cdr $values))
					       (cdr l))))
			       ((eq (car l) '$functions)
				(setq l (nconc (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $functions))
					       (mapcan
						#'(lambda (x)
						    (if (mget x 'aexpr)
							(ncons (consfundef x t nil))))
						(cdr $arrays))
					       (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $macros))
					       (cdr l))))
			       ((setq l1 (listargp (car l)))
				(setq l (nconc (getlabels (car l1) (cdr l1) t) (cdr l)))))
			 (if (null l) (return nil))
			 (terpri savefile)
			 (if $grind (mgrind (strmeval (car l)) savefile)
			     (princ (print-invert-case (maknam (mstring (strmeval (car l)))))
					    savefile))
			 (if (or (and (symbolp (car l)) (get (car l) 'nodisp)) (not $strdisp))
			     (write-char #\$ savefile)
			     (progn (write-char #\; savefile)
				    (dotimes (n newlines) (write-char #\Newline savefile)))))))
		     (setq maxima-error t)))
	      (setq truename (truename savefile))
	      (terpri savefile))
	    (if maxima-error (let ((errset 'errbreak1)) (merror "Error in `stringout' attempt")))
	    (cl:namestring truename)))))
