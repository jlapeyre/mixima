;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mock MMA (A Lisp language mathematica-like system)
;;(c) copyright 1990, 1991 by Richard J. Fateman and Univ. of California
;; see enclosed notice (file copyright)

;; this file should be loaded in at COMPILE time for every file in
;; the mma package.  It should also be loaded in (once) when the
;; mma package is set up.

;; Mathematica, on which this is based,
;; is described in S. Wolfram: Mathematica, a
;; System for Doing Mathematics By Computer, (Addison-Wesley).

;; ok, I give up.  I will use CL standard losing case insensitivity.
;; Allegro had it right prior to the standard, but had to yield. The
;; result, that the case-sensitive stuff did not always work in later
;; releases, seems to  makes it advisable to just use case-insensitive-upper.
;; it also increases compatibility with other CLs  RJF. 10/30/97

;;;(eval-when (compile load eval)
;;;	   #+Allegro(cond((eq *current-case-mode* :case-sensitive-lower))
;;;		(t (excl::set-case-mode :case-sensitive-lower))))

;; obsolete (provide 'mma)

;; to stop error if not using maxima
(if (find-package :maxima) t (defpackage :maxima))
(if (find-package :common-lisp) t (defpackage :common-lisp))

(if (find-package :mma) t   (defpackage :mma (:nicknames "MockMMA") (:use :common-lisp
                                              #+gcl :lisp
					      #+allegro     :excl
					      )))

;; below is original fateman code, with ;; (shadow ..) gcl seems to
;; care about the comment somehow. I'll look into it again later
;;(defpackage :mma (:nicknames "MockMMA") (:use :common-lisp
;;					      #+allegro     :excl
;;;;  (shadow '(and or not plusp minusp) :mma) ;make these unbound in :mma
;;  
;;					      ))

(in-package :mma)
