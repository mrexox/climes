# Climes

Making dependencies managing easier in Common Lisp

## Package definition

Filename: `systems.lisp`

```lisp
;;; Common Lisp Systems (dependencies)

(lisp :sbcl) ;; interpreter, constraint and version

;;; You can define a special scopes of packages and install them separately

;;; To define a dedendency use there options
;;; name - without any other options just installing via quicklisp
;;; :git - means cloning from git repository
;;; :ref - use specific git branch or tag

(scope :production
  (:hunchentoot)
  (:alexandria :git "https://github.com/alexandria/cl-alexandria
               :ref "v1.2"))

(scope :development
  (:drakma))
```

## Usage

```bash
$ climes install # install systems within all scopes
$ climes install --scope=production,development # install only given scope(s)

# You can also just call climes:

$ climes # executes install by default
```

## :warning: Known issues

- If you specify `:git` option you may find out that packages are not scoped in projects. E.g if you have two projects with different versions of packages - it will use one (`ls ~/common-lisp` - alphabetically first is going to be used)
- Dependencies for packages installed via git are managed via quicklisp only now