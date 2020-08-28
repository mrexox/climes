#|

Command Line Interface for Climes

Copyright (c) 2020, Valentine Kiselev

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of climes nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|#

(asdf:load-system :unix-opts)
(asdf:load-system :climes)

(defconstant +program+ "climes")
(defconstant +usage+
  (format nil "
Usage:
  ~a         -- same as ~:*'~a install'~:*
  ~a install -- install defined systems
" +program+))

(defconstant +command-handlers+
  (list (cons "install" #'climes:install)
        (cons nil       #'climes:install)))

;;; Define command line options

(opts:define-opts
  (:name :help
   :description +usage+
   :short #\h
   :long "help")
  (:name :scope
   :description "Scope to install"
   :short #\s
   :arg-parser #'identity
   :long "scope"))

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

;;; Parse options

(multiple-value-bind (options free-args)
    (handler-bind ((opts:unknown-option #'unknown-option))
      (opts:get-opts))
  (let ((scope nil))

    ;; Parse scope
    (let ((option (getf options :scope)))
      (when option
        (setf scope option)))

    ;; Parse help
    (let ((option (getf options :help)))
      (when option
        (print-usage)))

  ;; Parse command
  (let* ((command (first free-args))
         (handler (cdr (assoc command +command-handlers+ :test #'string=))))
    (unless handler
      (format t "Unknown command ~a" command)
      (print-usage))
    (opts:exit (funcall handler :scope scope)))))
