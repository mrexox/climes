;;; Command Line Interface

(defparameter *lisp-interpreter* 'sbcl
  "Lisp interpreter")
(defparameter *lisp-interpreter-version* 0
  "Lisp interpreter version")
(defparameter *lisp-interpreter-version-restriction* '>=
  "Lisp interpreter version restriction")

(defparameter *production-dependencies* (list)
  "The list of production dependencies")
(defparameter *development-dependencies* (list)
  "The list of development dependencies")

;;; Helper functions

(defun lisp (&rest body)
  (assert (>= (length body) 2) ()
          "Must specify at least lisp interpreter and it's version")
  (let ((interpreter (first body))
        (restriction (second body))
        (version (third body)))
    (format t "~A" interpreter)
    (assert (eq interpreter 'sbcl) ()
            "Currently only sbcl interpreter is supported")
    (assert (if (null version) (find restriction '(> >= < <=)) t) ()
            ("Lisp version must have > >= < <= restriction. >= is used if not given")))
    (setq *lisp-interpreter* interpreter
          *lisp-interpreter-version* (or version restriction)
          *list-interpreter-version-restriction* (if (null version) '>= restriction))))

(defun production (&rest systems))
(defun development (&rest systems))
