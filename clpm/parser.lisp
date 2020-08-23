#|

Parser for CLPM

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

(in-package :clpm-interpreter)

;;; Parameters

(defparameter *interpreter* '((:name . :sbcl))
  "(DEPRECATE) Lisp implementation settings")

(defparameter *scopes* (make-hash-table)
  "The hash table of scoped dependencies")

(defun get-scopes () *scopes*)
;;; Parsing code

(defclass system ()
  ((name
    :initarg :name
    :initform (error "Must supply system name.")
    :reader name)
   (git
    :initarg :git
    :initform nil
    :reader git)
   (tag
    :initarg :tag
    :initform nil
    :reader git-tag)))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type t)
    (format stream "~(~s~): from ~(~a~)~@[ ~s~]"
            (name system)
            (source-type system)
            (git system))))

(defgeneric source-type (system)
  (:documentation
   "Get type of the source configured (or used by default) for the system.")
  (:method ((package system))
    (cond ((not (null (git package))) :git)
          (t :quicklisp))))

(defmacro lisp (&rest definition)
  "(DEPRECATE) Specify lisp implementation and it's version"
  `(setf *interpreter* (remove-if #'(lambda (cell) (null (cdr cell)))
                                  (list
                                   (cons :name (first ',definition))
                                   (cons :constraint (second ',definition))
                                   (cons :version (third ',definition))))))

(defun add-system-to-scope (&key scope name git tag)
  (let* ((definitions (gethash scope *scopes*))
         (new-scope? (null definitions)))
    (when new-scope?
      (setf definitions (make-hash-table)))
    (setf (gethash name definitions)
          (make-instance 'system
                         :name name
                         :git git
                         :tag tag))
    (when new-scope?
      (setf (gethash scope *scopes*) definitions))))

(defun find-by-key (key list)
  "Find the next value after keyword parameter `key' in `list'"
  (when list
    (elt list (1+ (position key list :test #'eql)))))

;;; Set dependencies by scope
;;;
;;; Example:
;;;   (scope :development
;;;     (:package-name >= "1.2.3"))
;;;
;;; Note:
;;;   Use keywords for scope and package names
(defmacro scope (key &rest systems)
  (let ((definition (gensym)))
    `(dolist (,definition ',systems)
       (add-system-to-scope :scope ,key
                            :name (first ,definition)
                            :git (find-by-key :git (cdr ,definition))
                            :tag (find-by-key :tag (cdr ,definition))))))
