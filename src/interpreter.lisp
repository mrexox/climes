#|
Systems file interpreter for Climes

Copyright (c) 2021, Valentine Kiselev
|#

(in-package :climes-interpreter)

;;; Parameters

(defparameter *interpreter* '((:name . :sbcl))
  "(DEPRECATE) Lisp implementation settings")

(defparameter *scopes* (make-hash-table)
  "The hash table of scoped dependencies")

(defun get-scopes () *scopes*)
;;; Parsing code

(defmacro lisp (&rest definition)
  "(DEPRECATE) Specify lisp implementation and it's version"
  `(setf *interpreter* (remove-if #'(lambda (cell) (null (cdr cell)))
                                  (list
                                   (cons :name (first ',definition))
                                   (cons :constraint (second ',definition))
                                   (cons :version (third ',definition))))))

(defun add-system-to-scope (&key scope name git ref)
  (let* ((definitions (gethash scope *scopes*))
         (new-scope? (null definitions)))
    (when new-scope?
      (setf definitions (make-hash-table)))
    (setf (gethash name definitions)
          (make-instance 'system
                         :name name
                         :git git
                         :ref ref))
    (when new-scope?
      (setf (gethash scope *scopes*) definitions))))

(defun find-by-key (key list)
  "Find the next value after keyword parameter `key' in `list'"
  (when list
    (let ((pos (position key list :test #'eql)))
      (if pos
          (elt list (1+ pos))))))

;;; Set dependencies by scope
;;;
;;; Example:
;;;   (scope :development
;;;     (:package-name >= "1.2.3"))
;;;
;;; Note:
;;;   Use keywords for scope and package names
(defmacro scope (key &rest systems)
  (let ((definition (gensym)))
    `(dolist (,definition ',systems)
       (add-system-to-scope :scope ,key
                            :name (first ,definition)
                            :git (find-by-key :git (cdr ,definition))
                            :ref (find-by-key :ref (cdr ,definition))))))
