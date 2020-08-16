;;;; Installer

;;; Copyright (c) 2020, Valentine Kiselev

;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * Neither the name of clpm nor the names of its contributors
;;;       may be used to endorse or promote products derived from this software
;;;       without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :clpm)

(defun install (&key scope)
  "Install scope dependencies"
  (let* ((scopes (split (string-downcase scope))))
    ;; TODO
    (parse-systems (curdir))
    (check-scopes scopes)
    (format t "Installing scopes: [~{~a~^, ~}]~%" scopes)
    (dolist (scope scopes)
      (install-scope scope))))

(defun split (scopes)
  "Split string by comma (,)"
  (flet ((empty (str) (zerop (length (string-trim '(#\Space #\Tab #\Newline) str))))
         (index (lst &optional (start 0))
           (or (position #\, lst :start start)
               (length lst))))
    (delete-if #'empty
               (delete-duplicates
                (do* ((start 0 (1+ (index scopes start)))
                      (end (index scopes) (index scopes start))
                      (scope-list (list (subseq scopes start end))
                                  (cons (subseq scopes start end) scope-list)))
                     ((= (index scopes start) (length scopes)) scope-list))
                :test #'string=))))

(defun curdir ()
  (uiop:getenv "PWD"))

(defun install-scope (scope)
  ;; TODO
  )
