(in-package :asdf-user)

(defsystem "climes"
  :version "1.0.0"
  :license "BSD 3-clause"
  :author "Valentine Kiselev <mrexox@yahoo.com>"
  :description "Common Lisp Manager for Systems"
  :depends-on (#:unix-opts)
  :components ((:file "packages")
               (:module "climes"
                :depends-on ("packages")
                :components ((:file "parser")
                             (:file "installer")))))
