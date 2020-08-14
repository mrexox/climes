This project is in WIP status.

## Definition of packages

```lisp
;;; Common Lisp Systems (dependencies)

(list :sbcl >= "2.0.1") ;; interpreter, constraint and version

;;; You can define a special scope of packages

(scope :production
  (:hunchentoot > "1.2.38")  ;; system with version constraint
  (:alexandria))             ;; versions can be omited, the latest will be taken

(scope :development
  (:drakma = "2.0.7"))  ;;
```