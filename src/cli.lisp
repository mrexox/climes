#|
Command Line Interface for Climes

Copyright (c) 2021, Valentine Kiselev
|#

#+sbcl (sb-ext:disable-debugger)
#+sbcl (declaim (sb-ext:muffle-conditions cl:warning))

(asdf:load-system :climes)

(defconstant +program+ "climes")
(defconstant +usage+
  (format nil "Usage:
  ~a                         | same as ~:*'~a install'~:*
  ~a install                 | install defined systems~:*
  ~a install --scope develop | install systems from 'develop' scope
" +program+))

(defconstant +command-handlers+
  (list (cons "install" #'(lambda (arguments)
                            (climes:install :scope (gethash :scope arguments))))
        (cons "help"    #'(lambda (arguments) (print-usage)))
        (cons nil       #'(lambda (arguments)
                            (climes:install :scope (gethash :scope arguments))))))

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
  (terminate 1))

(defun call-handler (command arguments)
  (let ((handler (or (cdr (assoc command +command-handlers+ :test #'string=))
                    #'(lambda (&rest _)
                        (format t "Unknown command '~a'~%~%" command)
                        (print-usage)))))
    (funcall handler arguments)))

(defun opt-looks-like (option)
  (if (string= option "--" :start1 0 :end1 2)
      'LONGOPT
      (if (string= option "-" :start1 0 :end1 1)
          'SHORTOPT
          'COMMAND)))

(defvar *options*
  '(("s" . :scope) ("scope" . :scope)))


(let ((arguments (make-hash-table))
      (command nil)
      (was-an-option nil)
      (handler nil))
  (flet ((handle-short-option (dirty-option rest)
           (setq was-an-option t)
           (let ((option (subseq dirty-option 1 2)))
             (setf (gethash (cdr (assoc option *options* :test #'string=)) arguments)
                   (car rest))))
         (handle-long-option (dirty-option rest)
           (setq was-an-option t)
           (let ((option (subseq dirty-option 2)))
             (setf (gethash (cdr (assoc option *options* :test #'string=)) arguments)
                   (car rest))))
         (handle-command (cmd)
           (setf command cmd)))

    (loop for (opt . rest) on (cdr sb-ext:*posix-argv*)
          if was-an-option
            do (setq was-an-option nil)
          else
            do (let ((opt-type (opt-looks-like opt)))
                 (cond
                   ((eql opt-type 'SHORTOPT) (handle-short-option opt rest))
                   ((eql opt-type 'LONGOPT) (handle-long-option opt rest))
                   ((eql opt-type 'COMMAND) (handle-command opt)))))

    (call-handler command arguments)))
