;;; Common Lisp Systems (dependencies)

(lisp 'sbcl '>= "2.0.1")

(production
  ('hunchentoot '> "1.2.38")
  ('alexandria '= "20191227-git"))

(development
 ('drakma '= "2.0.7"))
