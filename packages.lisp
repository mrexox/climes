(defpackage :clpm-interpreter
  (:use :common-lisp)
  (:export :lisp
           :scope
           ;; reader of scopes hash-table
           :get-scopes
           ;; 'system' class methods
           :name
           :source-type
           :git
           :tag))

(defpackage :clpm
  (:use :common-lisp)
  (:export :install)
  (:import-from :clpm-interpreter
                :name
                :source-type
                :git
                :tag))
