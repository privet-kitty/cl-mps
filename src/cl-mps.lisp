(defpackage :cl-mps
  (:use :cl)
  (:shadow #:variable)
  (:export #:mps-syntax-warning #:mps-syntax-warning-line-number
           #:sense #:variable #:problem #:+maximize+ #:+minimize+
           #:read-mps))

(in-package :cl-mps)

(define-condition mps-syntax-warning (simple-warning)
  ((line-number :type (or null (integer 0))
                :initarg :line-number
                :initform nil
                :reader mps-syntax-warning-line-number)))

(defmethod print-object ((warning mps-syntax-warning) stream)
  (unless *print-escape*
    (when (mps-syntax-warning-line-number warning)
      (format stream "L~D: " (mps-syntax-warning-line-number warning))))
  (call-next-method))

(defconstant +maximize+ 1)
(defconstant +minimize+ -1)

(deftype sense () '(integer -1 1))

(defstruct variable
  (name nil :type string)
  (integer-p nil :type nil)
  (lo nil :type (or null real))
  (hi nil :type (or null real)))

(defstruct problem
  (name nil :type string)
  (sense nil :type sense))

(defun %split (string)
  (declare (optimize (speed 3))
           (string string))
  (delete-if-not (lambda (s) (> (length (the string s)) 0))
                 (the list (cl-ppcre:split "\\s+" string))))

(defun read-mps (stream default-sense)
  (check-type stream stream)
  (check-type default-sense sense)
  (let ((problem (make-problem :name "" :sense default-sense)))
    (loop
      for line-number from 0
      for line = (read-line stream nil nil)
      do (block continue
           (unless line (return))
           (let ((items (%split line)))
             (trivia:match (first items)
               ("ENDATA" (return))
               ("*" (return-from continue))
               ("NAME"
                (when (second items)
                  (setf (problem-name problem) (second items)))
                (when (third items)
                  (warn 'mps-syntax-warning
                        :line-number line-number
                        :format-control "NAME contains whitespaces: ~S"
                        :format-arguments (list line))))))))
    problem))
