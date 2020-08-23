#|

Installer for CLPM

Copyright (c) 2020, Valentine Kiselev

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of clpm nor the names of its contributors
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

(in-package :clpm)

(defparameter *file-does-not-exist-format*
  "~&Systems definition is expected here ~s~%But file does not exist~%Exiting...~%")
(defparameter *install-format*
  "Installing ... ~(~a~)~@[: ~(~a~)~]~%")

(define-condition file-does-not-exist ()
  ((path
    :initarg :path
    :reader path)))

(defun install (&key scope)
  "Install scope dependencies from systems file"
  (handler-case (parse-systems (curdir))
    (file-does-not-exist (e)
      (format t *file-does-not-exist-format* (namestring (path e)))
      (return-from install nil)))
  (let* ((scopes (check-scopes (split scope))))
    (format t "Scopes: [~{~(~a~)~^, ~}]~%"
            (loop for k being the hash-key in scopes collect k))
    (loop for scope being the hash-value in scopes
          do (install-scope scope))))

(defun split (scopes)
  "Split string by comma (,) and return a list of keywords"
  (when (not (null scopes))
    (flet ((empty? (str)
             (zerop (length (string-trim '(#\Space #\Tab #\Newline) str))))
           (index (lst &optional (start 0))
             (or (position #\, lst :start start)
                 (length lst))))
      (mapcar #'(lambda (s) (values (intern (string-upcase s) "KEYWORD")))
              (delete-if #'empty?
                         (delete-duplicates
                          (do* ((start 0 (1+ (index scopes start)))
                                (end (index scopes) (index scopes start))
                                (scope-list (list (subseq scopes start end))
                                            (cons (subseq scopes start end) scope-list)))
                               ((= (index scopes start) (length scopes)) scope-list))
                          :test #'string=))))))

(defun curdir ()
  (uiop:getenv "PWD"))

(defparameter *systems-filename* "systems.lisp"
  "Name of systems file with systems scopes")

(defun parse-systems (directory)
  "Parse systems file and save scopes"
  (let ((*package* (find-package :clpm-interpreter))
        (path (merge-pathnames
                         (concatenate 'string directory "/")
                         *systems-filename*)))
    (with-open-file (in path :if-does-not-exist nil)
      (unless in
        (error 'file-does-not-exist :path path))
      (loop for expression = (read in nil nil)
            while expression
            do (eval expression)))))

(defun check-scopes (scopes)
  "Checks if scopes are defined in systems file or return them all as hash-table"
  (let ((defined-scopes (clpm-interpreter:get-scopes)))
    (if (null scopes)
        defined-scopes
        (let ((filtered-scopes (make-hash-table)))
          (loop for k being the hash-keys in defined-scopes
                  using (hash-value v)
                when (member k scopes)
                  do (setf (gethash k filtered-scopes) v))
          filtered-scopes))))

(defun install-scope (scope)
  "Install given scope (hash-table of system objects)"
  (loop for system being the hash-value in scope
        do (install-system system)))

;;; Installation of system
(defgeneric install-system (system)
  (:documentation "Install system based on parsed parameters for it.")
  (:method ((system system))
    (format t *install-format* (name system) (source-type system))))
