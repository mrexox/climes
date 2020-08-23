;;; Common Lisp Systems (dependencies)

(lisp :sbcl)

(scope :release
  (:unix-opts))

(scope :develop
  (:hunchentoot)
  (:drakma :git "https://github.com/edicl/drakma"
           :tag "v2.0.7"))
