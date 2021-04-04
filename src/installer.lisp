#|
Climes

Copyright (c) 2021, Valentine Kiselev
|#

(in-package :climes)

;;; ===========================================================================
;;; Parameters
;;; ===========================================================================

(defparameter *file-does-not-exist-format*
  "~&Systems definition is expected here ~s~%But file does not exist~%Exiting...~%")

(defparameter *install-format*
  "Installing ~(~a~) ... ")

(defparameter *systems-filename* "systems.lisp"
  "Name of systems file with systems scopes.")

;;; ===========================================================================
;;; Conditions
;;; ===========================================================================

(define-condition file-does-not-exist ()
  ((path
    :initarg :path
    :reader path)))

;;; ===========================================================================
;;; Installer logic
;;; ===========================================================================

(defun install (&key ((:scope raw-scopes)))
  "Install scope dependencies from systems file.
Returned value of this function is used as return value for the shell call."
  (handler-case (parse-systems (curdir))
    (file-does-not-exist (e)
      (format t *file-does-not-exist-format* (namestring (path e)))
      (return-from install 1)))
  (let* ((scopes (check-scopes (split raw-scopes))))
    (format t "Scopes: [~{~(~a~)~^, ~}]~%"
            (loop for k being the hash-key in scopes collect k))
    (loop for scope being the hash-value in scopes
          do (install-scope scope))))

(defun split (raw-scopes)
  "Split string by comma (,) and return a list of keywords."
  (when (not (null raw-scopes))
    (flet ((empty? (str)
             (zerop (length (string-trim '(#\Space #\Tab #\Newline) str))))
           (index (lst &optional (start 0))
             (or (position #\, lst :start start)
                 (length lst))))
      (mapcar #'(lambda (s) (values (intern (string-upcase s) "KEYWORD")))
              (delete-if #'empty?
                         (delete-duplicates
                          (do* ((start 0 (1+ (index raw-scopes start)))
                                (end (index raw-scopes) (index raw-scopes start))
                                (scope-list (list (subseq raw-scopes start end))
                                            (cons (subseq raw-scopes start end) scope-list)))
                               ((= (index raw-scopes start) (length raw-scopes)) scope-list))
                          :test #'string=))))))

(defun curdir ()
  (uiop:getenv "PWD"))

(defun parse-systems (directory)
  "Parse systems file and save scopes."
  (let ((*package* (find-package :climes-interpreter))
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
  "Checks if scopes are defined in systems file or return them all as hash-table."
  (let ((defined-scopes (climes-interpreter:get-scopes)))
    (if (null scopes)
        defined-scopes
        (let ((filtered-scopes (make-hash-table)))
          (loop for k being the hash-keys in defined-scopes
                  using (hash-value v)
                when (member k scopes)
                  do (setf (gethash k filtered-scopes) v))
          filtered-scopes))))

(defun install-scope (scope)
  "Install given scope (hash-table of system objects)."
  (loop for system being the hash-value in scope
     do (progn
          (format t *install-format* (name system))
          (install-system system))))
