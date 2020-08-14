;;;; Command Line Interface

(asdf:load-system :unix-opts)

(defconstant +program+ "clpm")
(defconstant +usage+
  (format nil "
Usage:
  ~a         -- same as '~a install'
  ~a install -- install defined systems
" +program+ +program+ +program+))

(defconstant +command-handlers+
  '(("install" . #'install-dependencies)
    (nil . #'install-dependencies)))

;;; Define command line options

(opts:define-opts
  (:name :help
   :description +usage+
   :short #\h
   :long "help"))

;;; Handler functions

(defun terminate (status)
  #+sbcl     (           sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (              ccl:quit      status)                 ; Clozure CL
  #+clisp    (              ext:quit      status)                 ; GNU CLISP
  #+cmu      (             unix:unix-exit status)                 ; CMUCL
  #+ecl      (              ext:quit      status)                 ; ECL
  #+abcl     (              ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (             excl:exit      status :quiet t)        ; Allegro CL
  #+gcl      (common-lisp-user::bye       status)                 ; GCL
  #+ecl      (              ext:quit      status)                 ; ECL
  (cl-user::quit))

(defun print-usage ()
  (format t +usage+)
  (terminate 0))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun install-dependencies ()
  ;; TODO
  )

;;; Parse options

(multiple-value-bind (options free-args)
    (handler-bind ((opts:unknown-option #'unknown-option))
      (opts:get-opts))

  ;; Parse options
  (let ((option (getf options :help)))
    (when option
      (print-usage)))

  ;; Parse command
  (let* ((command (first free-args))
         ((handler (cdr (assoc command +command-handlers+ :test string=)))))
    (if handler
        (funcall handler)
        (progn
          (format t "Unknown command ~a", command)
          (pring-usage)))))
