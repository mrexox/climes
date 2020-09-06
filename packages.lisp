(defpackage :climes-interpreter
  (:use :common-lisp)
  (:export :lisp
           :scope
           ;; reader of scopes hash-table
           :get-scopes
           ;; 'system' class
           :system
           :name
           :source-type
           :git
           :git-tag))

(defpackage :climes
  (:use :common-lisp)
  (:export :install)
  (:import-from :climes-interpreter
                :system
                :name
                :source-type
                :git
                :git-tag))
