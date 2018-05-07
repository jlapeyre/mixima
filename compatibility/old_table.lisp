(in-package :maxima)

#|

  VERSION VERSIONNUMBER

  Copyright (C) 2008, John Lapeyre  All rights reserved.

  This program is free software; you can redistribute it
  and/or modify it only under the terms of the GNU General
  Public License version 2 as published by the Free Software
  Foundation.

  Mathematica is a registered trademark of Wolfram Research,
  Inc. (2008)

  
   Table. 
   This implementation tries to reproduce current (v 6) documented Mathematica functionality.
   It is only tested with Mma 3.0,  so more recent functionality is only compared to 
   documented behavior.  
   In particular: 
    1)  the  error messages, etc. mimic Mma 3.0.
    2)  explicit lists in the iterator are not supported in 3.0 (but are in the implementation below)
        eg. Table(f(i), [i,[1,2,3]]);

   An example of which kind of Table call is being handled is marked by >>> Table...

   The code could be refactored a bit.

|# 



(defmspec |$oldTable| (l)
  (cond ((null (rest l)) (merror "Table called with 0 arguments; 1 or more arguments are expected.")))
  (cond ( (null (cddr l)) (cadr l)) ; >>> Table[1] -->  1, mimics Mma behavior
	(t 
	 (let ((res (apply #'table1 (cadr l)  (cddr l))))
	   (cons '(mlist) res)))))

; make string [1,2,3] from list ( '(mlist) 1 2 3 ), for error message
; error messages follow Mma, but they could be more informative
(defun maxima-mlist-format (lis)
  (cond ((listp lis)
	 (format nil "[~{~a~^,~}]" (mapcar #'maxima-mlist-format (rest lis))))
	(t (string-left-trim "$" (format nil "~a" lis))))); remove '$' but does not correct case

;; form -- the first arg to Table, the expression that makes an element
;; l  -- list of iterators, each of which is a list that specifies how to iterate for
;;       level of a nested table.
(defun table1 (form &rest l &aux iter itlist var1 start stop step itlen res mits)
  (cond ( (> (length l) 1) (setq mits t)) ; set flag if we need to make a nested list.
	(t (setq mits nil)))              ; because there are multiple iterators
  (setq res '())
;  (setq iter (meval* (first l))) ; if you do this, it picks up unwanted definition in enclosing block.
  (setq iter (first l)) 
  (cond ( (not ($listp iter)) (setq iter (meval* iter)))) ; meval* to allow  >>> apply(lambda([x], Table(i,x)), [ [i,4]]);
  (cond ((atom iter) (merror "argument '~a' does not have the correct form for an iterator" (maxima-mlist-format iter)))
	(t
	 (setq itlen (length iter))
	 (cond ((eq itlen 2)     ; >>>  Table(a,[3])
		(setq stop (meval* (second iter)))
		(cond ( mits ; 
                       (setq res 
			     (loop for i from 1 to stop
				   collect
				      (cons '(mlist) 
					 (apply #'table1 (cons (meval* form) (rest l)))))))
		      (t  ; do meval every time because Mma does
		      ;  (setf res (make-list stop :initial-element (meval* form)))))) only evals onces
		      (setq res (loop for i from 1 to stop
			     collect (meval* form) )))))
	       ((> itlen 2)
		;(setq var1 (meval* (second iter))) ; which of these lines makes no difference ?
		(setq var1 (second iter))
		(cond (($listp (meval* (third iter))); >>> Table(i,[i,[1,2,3]])
		       (unless (eq 3 (length iter)) 
                         (merror "argument '~a' does not have the correct form for an iterator" (maxima-mlist-format iter)))
		       (setq itlist (rest (meval* (third iter))))
		       (progv (list var1)
			   (list nil)
			 (cond ( mits  ; >>> Table(i*j,[i,[1,2,3]],[j,...])
				(setq res
				      (loop for i in itlist do
					    (setf (symbol-value var1) i)
					    collect (cons '(mlist) 
							  (apply #'table1 (cons (meval* form) (rest l)))))))
			       (t
				(setf res
				      (loop for i in itlist do 
					    (setf (symbol-value var1) i)
					    collect (meval* form)))))))
		       (t  ; iterator with one, two, or three of start, stop, and step
			(cond ((eq itlen 3)  ; >>>  Table(i,[i,10]);
			      (setq start 1) 
                              (setq stop (meval* (third iter)))
			      (setq step 1))
			     ((eq itlen 4)  ; >>>  Table(i,[i,2,10]);
			      (setq start (meval* (third iter)))
			      (setq stop (meval* (fourth iter)))
			      (setq step 1))
			     ((eq itlen 5) ; >>>  Table(i,[i,2,10,2]);
			      (setq start (meval* (third iter)))
			      (setq stop (meval* (fourth iter)))
			      (setq step (meval* (fifth iter))))
			     (t
			      (merror "argument '~a' does not have the correct form for an iterator" (maxima-mlist-format iter))))
		       (cond ( (not (numberp step))  ; >>>  Table(x+2^x,[x,a,5*n+a,n])
			       (setq form ($substitute (add* start (mul* step var1)) var1 form))
			       (setq stop (add* stop (*mminus start)))
			       (setq stop (div* stop step))
			       (setq start 0)
			       (setq step 1)))
		       (progv (list var1)
			   (list nil)
			 (cond ( (< step 0)
				(setq step (* step -1)) ; >>>  Table(i,[i,10,2,-2]);
				 (cond ( mits
					(setq res 
					(loop for i from start downto stop by step do
					      (setf (symbol-value var1) i)
					      collect (cons '(mlist) 
							    (apply #'table1 (cons (meval* form) (rest l)))))))
				       (t  
					(setq res
					(loop for i from start downto stop by step do
					      (setf (symbol-value var1) i)
                                              collect (meval* form))))))
			       (t
				(cond ( mits
				       (setq  res 
					      (loop for i from start to stop by step do
						    (setf (symbol-value var1) i) collect
							  (cons '(mlist) 
								      (apply #'table1 (cons (meval* form) (rest l)))))))
				      (t  
				       (setq res (loop for i from start to stop by step do
					     (setf (symbol-value var1) i)
                                             collect
					     (meval* form))))))))))))))
  res)
