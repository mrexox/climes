;;; Common Lisp Systems (dependencies)

(lisp :sbcl >= "2.0.1")

(scope :production
  (:hunchentoot > "1.2.38")
  (:alexandria = "20191227-git"))

(scope :development
 (:drakma = "2.0.7"))
