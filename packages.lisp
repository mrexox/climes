(defpackage :clpm
  (:use :common-lisp)
  (:export :install))

(defpackage :clpm-interpreter
  (:use :common-lisp)
  (:export :lisp
           :scope
           :get-scopes))
