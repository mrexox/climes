;;; Common Lisp Systems (dependencies)

(lisp :sbcl)

(scope :release
       (:unix-opts)
       (:alexandria :git "https://gitlab.common-lisp.net/alexandria/alexandria.git"
                    :ref "v1.2"))

(scope :develop
  (:hunchentoot)
  (:drakma :git "https://github.com/edicl/drakma"
           :ref "v2.0.7"))
