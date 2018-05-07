;; Table function base on calls to makelist
(in-package :maxima)

#|

  Copyright (C) 2008, John Lapeyre  All rights reserved.

  This program is free software; you can redistribute it
  and/or modify it only under the terms of the GNU General
  Public License version 2 as published by the Free Software
  Foundation.

  Mathematica is a registered trademark of Wolfram Research,
  Inc. (2008)

  This version of Table uses maxima's makelist, which I believe was written by
  Jaime Villate. I wanted to simply call makelist rather than copy the code, but
  see the example below where an extra evaluation of the iterator is done if it is
  an atom. So  table_makelist below is like makelist but the iterator arguments are
  in a list that is an argument to makelist. Then Table constructs a nested call to
  table_makelist and evaluates the result.

|#

#|> Function Table |#
(defmspec |$Table| (x &aux res form iters)
  (setq x (cdr x)) ; remove 'newTable
  (if (null x) (merror "Table called with 0 arguments; 1 or more arguments are expected."))
  (setq form (first x))
  (setq iters (reverse (rest x)))
  (setq res (build-makelist-expr form iters))
  (setq res (meval res))
  (cond ( (null iters) (second res)) ; Mma gives:  Table(a) -> a
        ( t
          res)))

(defun build-makelist-expr (form iters &aux expr)
  (setq expr (list  '($table_makelist)  form (first iters)))
  (dolist (iter (cdr iters))
    (setq expr (list '($table_makelist)  expr iter)))
  expr)

#|

   This works in Mma even though iters are not evaluated
   Apply[Table[(i-j),#1,#2]&,{{i,4},{j,5}}]

   The translation does not work unless the line below evaluating iters is there.
   Apply(lambda( [[lambda_args]], Table((i-j),lambda_args[1],lambda_args[2]) ),[[i,4],[j,5]]);

   Perhaps Mma has Apply or the lambda form evaluating things. I can try to make the translated
   lambda function evaluate its args.

|#


(defmspec $table_makelist (y &aux x iter)
  (setq y (cdr y))
  (setq iter (second y))
;; problematic. sometimes Mma does not evaluate the iter, but some real mma examples fail to be reproduced
;; without the following line
;;  (if (not ($listp iter)) (setq iter (meval* iter))); to allow :  apply(lambda([x], Table(i,x)), [ [i,4]])
;;  (format t " table iter '~a'~%" iter)
  (if (and (not (null iter)) (atom iter))
      (merror "argument '~a' does not have the correct form for an iterator" (maxima-mlist-format iter)))
  (setq x (cons (first y)  (rest iter)))
  (prog (n form arg a b c d lv)
     (setq n (length x))
     (cond
       ((= n 0) (return '((mlist))))
       ((= n 1)
        (setq form (first x))
        (return
          `((mlist) ,(meval `(($ev) ,@(list (list '(mquote) form)))))))
       ((= n 2)
        (setq form (first x))
        (setq b ($float (meval (second x))))
        (if (numberp b)
            (return
              (do
               ((m 1 (1+ m)) (ans))
               ((> m b) (cons '(mlist) (nreverse ans)))
               (push (meval `(($ev) ,@(list (list '(mquote) form))))
;;               (push (meval `(($ev) ,@(list  form)))
                     ans)))
            (merror (intl:gettext "makelist: second argument must evaluate to a number; found: ~M") b)))
       ((= n 3)
        (setq form (first x))
        (setq arg (second x))
        (setq b (meval (third x)))
        (if ($listp b)
            (setq lv (mapcar #'(lambda (u) (list '(mquote) u)) (cdr b)))
            (progn
              (setq b ($float (meval b)))
              (if ($numberp b)
                  (return
                    (do
                     ((m 1 (1+ m)) (ans))
                     ((> m b) (cons '(mlist) (nreverse ans)))
                      (push
                       (meval
                        `(($ev) ,@(list (list '(mquote) form)
                                        (list '(mequal) arg m)))) ans)))
                (merror (intl:gettext "makelist: third argument must be a number or a list; found: ~M") b)))))
       ((= n 4)
        (setq form (first x))
        (setq arg (second x))
        (setq a (meval (third x)))
        (setq b (meval (fourth x)))
        (setq d ($float (meval `((mplus) ,b ((mtimes) ,a -1)))))
        (if (numberp d)
            (setq lv (interval2 a 1 d))
            (merror (intl:gettext "makelist: the fourth argument minus the third one must evaluate to a number; found: ~M") d)))
       ((= n 5)
        (setq form (first x))
        (setq arg (second x))
        (setq a (meval (third x)))
        (setq b (meval (fourth x)))
        (setq c (meval (fifth x)))
        (setq d ($float
                 (meval 
                  `((mtimes) ((mplus) ,b ((mtimes) ,a -1)) ((mexpt) ,c -1)))))
        (if (numberp d)
            (setq lv (interval2 a c d))
            (merror (intl:gettext "makelist: the fourth argument minus the third one, divided by the fifth one must evaluate to a number; found: ~M") d)))
       (t (merror (intl:gettext "makelist: maximum 5 arguments allowed; found: ~M.~%To create a list with sublists, use nested makelist commands.") n)))
     (return 
       (do ((lv lv (cdr lv))
	    (ans))
	   ((null lv) (cons '(mlist) (nreverse ans)))
	 (push (meval `(($ev)
			,@(list (list '(mquote) form)
				(list '(mequal) arg (car lv)))))
	       ans)))))



; make string [1,2,3] from list ( '(mlist) 1 2 3 ), for error message
; error messages follow Mma, but they could be more informative
(defun maxima-mlist-format (lis)
  (cond ((listp lis)
	 (format nil "[~{~a~^,~}]" (mapcar #'maxima-mlist-format (rest lis))))
	(t (string-left-trim "$" (format nil "~a" lis))))); remove '$' but does not correct case

#| already defined with makelist code in mstuff.lisp
(defun interval2 (i s d)
  (do ((nn i (meval `((mplus) ,s ,nn)))
       (m 0 (1+ m))
       (ans))
      ((> m d) (nreverse ans))
    (push nn ans)))
|#
