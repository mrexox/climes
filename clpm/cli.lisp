;;; Command Line Interface

(defparameter *lisp-interpreter* 'sbcl
  "Lisp interpreter")
(defparameter *lisp-interpreter-version* 0
  "Lisp interpreter version")
(defparameter *lisp-interpreter-version-restriction* '>=
  "Lisp interpreter version restriction")

(defparameter *scopes* (make-hash-table)
  "The hash table of scoped dependencies")

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
          *list-interpreter-version-restriction* (if (null version) '>= restriction)))

(defun add-system-to-scope (&key scope name constraint version)
  (let ((scope-metadata (or (gethash scope *scopes*))))
    ;; TODO
    ))


(defmacro scope (key &rest systems)
  (dolist (definition systems)
    (let ((name (first definition))
          (constraint (second definition))
          (version (third definition)))
      `(add-system-to-scope :scope key
                            :name name
                            :constraint constraint
                            :version version))))
