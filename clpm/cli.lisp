;;; Command Line Interface

(defparameter *interpreter* '((:name . :sbcl))
  "Lisp interpreter settings")

(defparameter *scopes* (make-hash-table)
  "The hash table of scoped dependencies")

;;; Parsing

(defmacro lisp (&rest definition)
  `(setf *interpreter* (remove-if #'(lambda (cell) (null (cdr cell)))
                                      (list
                                       (cons :name (first ',definition))
                                       (cons :constraint (second ',definition))
                                       (cons :version (third ',definition))))))

(defun add-system-to-scope (&key scope name constraint version)
  (let* ((scope-metadata (gethash scope *scopes*))
         (new-scopep (null scope-metadata))
         (metadata (remove-if #'(lambda (cell) (null (cdr cell)))
                              (list
                               (cons :constraint constraint)
                               (cons :version version)))))
    (when new-scopep
      (setf scope-metadata (make-hash-table)))
    (setf (gethash name scope-metadata) metadata)
    (when new-scopep
      (setf (gethash scope *scopes*) scope-metadata))))

(defmacro scope (key &rest systems)
  (let ((name (gensym))
        (constraint (gensym))
        (version (gensym)))
  `(dolist (definition ',systems)
    (let ((,name (first definition))
          (,constraint (second definition))
          (,version (third definition)))
      (add-system-to-scope :scope ,key
                            :name ,name
                            :constraint ,constraint
                            :version ,version)))))
