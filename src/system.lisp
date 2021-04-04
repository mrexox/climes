#|
Climes

Copyright (c) 2021, Valentine Kiselev
|#

(in-package :climes-system)

(defvar *asdf-user-directory* "~/common-lisp/")

(defclass system ()
  ((name
    :initarg :name
    :initform (error "Must supply system name.")
    :reader name)
   (git
    :initarg :git
    :initform nil
    :reader git)
   (git-ref
    :initarg :ref
    :initform nil
    :reader git-ref)))

(defmethod print-object ((system system) stream)
  "Pring system in readable way"
  (print-unreadable-object (system stream :type t)
    (format stream "~(~s~): from ~(~a~)~@[ ~s~]~@[ ref: ~a~]"
            (name system)
            (source-type system)
            (git system)
            (git-ref system))))

(defgeneric source-type (system)
  (:documentation
   "Get type of the source configured for the system.")
  (:method ((package system))
    (cond ((not (null (git package))) :git)
          (t :quicklisp))))

(defgeneric install-system (system)
  (:documentation "Install system based on given parameters.")
  (:method ((system system))
    (case (source-type system)
      ;; FIXME: This call may cause an exception, not handled yet
      (:quicklisp (quicklisp-install system))
      (:git       (git-install system)))))

(defgeneric quicklisp-install (system)
  (:documentation "Install system via quicklisp.")
  (:method ((system system))
    (handler-case (ql:quickload (name system) :silent t)
      (ql:system-not-found ()
        (format t "Not found~%"))
      (:no-error (_res) (format t "~a Done~%" (system-version (name system)))))))

;; Install source into ~/common-lisp/ directory and quickload them
;; FIXME: support multiple versions
;; FIXME: support dependencies across packages
(defgeneric git-install (system)
  (:documentation "Install system from git sources.")
  (:method ((system system))
    (let ((system-destination-dir (concatenate
                                   'string
                                   (string-downcase (name system))
                                   (when (git-ref system) "_")
                                   (when (git-ref system) (git-ref system)))))
      (ensure-directories-exist *asdf-user-directory*)
      (uiop:with-current-directory (*asdf-user-directory*)
        (unless (probe-file system-destination-dir)
          (multiple-value-bind (stdout stderr exit-code)

              ;; Checkout git project with a given ref
              (uiop:run-program
               (flatten
                (list "git" "clone"
                      (when (git-ref system) (list "-b" (git-ref system)))
                      (git system)
                      system-destination-dir))
               :ignore-error-status t
               :force-shell t
               :error-output '(:string :stripped t))

            ;; Want to be verbose as much as possible
            (unless (= exit-code 0)
              (format t "FAILED: git exit code ~a~%  ERROR-MESSAGE: ~a~%" exit-code stderr)))))

      ;; Quicklisp finishes dependencies installing
      ;; TODO: Add climes method to fetch dependencies too
      (quicklisp-install system))))

(defun system-version (system-designator)
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

(defun flatten (l)
  (flet ((to-list (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (to-list x) (flatten x))) l)))
