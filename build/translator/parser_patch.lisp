;; copyright (c) 2009 Richard J. Fateman
;; RJF 6/30/09
;; GJL: This code pulled out of maxima.lisp . It allows parser to
;; work with gcl, which does not allow (setf (readtable-case mathrt) :preserve) 

(defun mread1()
  ;;  (format t "~% next char = ~s" (pc))
  (cond ((member (pc)'( #\space #\tab #\page) :test #'char=)
	 (rc)(mread1))  ;; fix - 2x bug
	((digit-char-p (pc));; next character is a digit 0-9
	 (collect-integer 
	  (char-to-int(read-char stream)) 10)) ;radix 10 default
	(t (or	;;(read-preserving-whitespace stream nil 'e-o-l)
	    (read stream nil 'e-o-l)
	      '|False|)
	   ;; nil reads as False
	   )))
;;; an attempt to get (read) to do both cases.

(defun _()(setf *readtable* #.(copy-readtable))) ;restore old readtable program

(loop for C across
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      do
      (set-macro-character 
       C
       #'(lambda(stream char)
	   (values (intern 
		    (format nil "~A~A" char 
			    (if (alphanumericp; terminate?
				 (peek-char nil stream #\% t))
				(read stream t nil t)
			      ""))
		    :mma)))
       t				; non-terminating macro character
       mma::mathrt))
