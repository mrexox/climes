This project is in WIP status.

## Definition of packages

Filename: `systems.lisp`

```lisp
;;; Common Lisp Systems (dependencies)

(lisp :sbcl) ;; interpreter, constraint and version

;;; You can define a special scope of packages
;;; name - without any other settings: downloading from quicklisp
;;; git - means cloning from git repository
;;; tag - use specific git tag

(scope :production
  (:hunchentoot)
  (:alexandria :git "https://github.com/alexandria/cl-alexandria
               :tag "v1.2.3"))

(scope :development
  (:drakma))
```

## Usage

```bash
$ clpm install # install systems within all scopes
$ clpm install --scope=production,development # install only given scope(s)

# You can also just call clpm:

$ clpm # executes install by default
```