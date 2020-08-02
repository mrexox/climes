;;; Common Lisp Systems (dependencies)

(lisp 'sbcl '>= "3.3.0")

(production
  ('asdf "1.2.3")
  ('hunchentoot '> "1.2.3")
  ('alexandria '~= "3.2.1"))

(development
 ('dracula '= "2.3.4"))
