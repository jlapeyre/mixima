;; grind_file.lisp -- grind each expression in a file
;; copyright 2010 by Robert Dodier
;; I release this work under terms of the GNU General Public License

(defun $grind_file (filename)
  (with-open-file (s filename)
    (let ((*mread-prompt* nil) x)
      (loop while (setq x (mread s))
        do
          (mgrind (third x) nil)
          (format t "$~%"))))
  '$done)
