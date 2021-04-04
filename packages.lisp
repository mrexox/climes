(defpackage :climes-system
  (:use :common-lisp)
  (:export :system
           :name
           :git
           :git-ref
           :source-type
           :print-object
           :install-system
           :quicklisp-install
           :git-install))

(defpackage :climes-interpreter
  (:use :common-lisp)
  (:export :lisp
           :scope
           ;; reader of scopes hash-table
           :get-scopes)
  (:import-from :climes-system
                :system
                :name
                :git
                :git-ref
                :source-type))

(defpackage :climes
  (:use :common-lisp
        :climes-system)
  (:export :install))
