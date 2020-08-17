(in-package :asdf-user)

(defsystem "clpm"
  :version "1.0.0"
  :license "BSD 3-clause"
  :author "Valentine Kiselev <mrexox@yahoo.com>"
  :description "Common Lisp package manager (WIP)"
  :depends-on (#:unix-opts)
  :components ((:file "packages")
               (:module "clpm"
                :depends-on ("packages")
                :components ((:file "parser")
                             (:file "installer")))))
