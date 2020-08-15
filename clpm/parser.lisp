;;;; Parser

;;; Parameters

(defparameter *interpreter* '((:name . :sbcl))
  "Lisp interpreter settings")

(defparameter *scopes* (make-hash-table)
  "The hash table of scoped dependencies")

;;; Parsing code

;;; Set interpreter requirements
;;;
;;; Example:
;;;   (lisp :sbcl >= "1.2.3")
(defmacro lisp (&rest definition)
  `(setf *interpreter* (remove-if #'(lambda (cell) (null (cdr cell)))
                                  (list
                                   (cons :name (first ',definition))
                                   (cons :constraint (second ',definition))
                                   (cons :version (third ',definition))))))

(defun add-system-to-scope (&key scope name git tag)
  (let* ((scope-metadata (gethash scope *scopes*))
         (new-scopep (null scope-metadata))
         (metadata (remove-if #'(lambda (cell) (null (cdr cell)))
                              (list
                               (cons :git git)
                               (cons :tag tag)))))
    (when new-scopep
      (setf scope-metadata (make-hash-table)))
    (setf (gethash name scope-metadata) metadata)
    (when new-scopep
      (setf (gethash scope *scopes*) scope-metadata))))

(defun find-by-key (key list)
  (when list
    (elt list (1+ (position key list :test #'eql)))))

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
                            :tag (find-by-key :tag (cdr ,definition))))))
