;;; Common Lisp Systems (dependencies)

(lisp :sbcl)

(scope :release
  (:unix-opts))

(scope :develop
  (:drakma)
  (:some-lisp-package :git "https://github.com/some/lisp-package"
                      :tag "v1.24"))
