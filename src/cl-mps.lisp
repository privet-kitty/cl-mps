(defpackage :cl-mps
  (:use :cl)
  (:shadow #:variable)
  (:export #:mps-syntax-warning #:mps-syntax-error #:mps-syntax-condition-line-number
           #:sense #:variable #:+maximize+ #:+minimize+ #:read-mps
           #:constraint #:constraint-name #:constraint-sense #:constraint-coefs #:constraint-rhs
           #:problem #:make-problem #:problem-name #:problem-sense))

(in-package :cl-mps)

(define-condition mps-syntax-condition (simple-condition)
  ((line-number :type (or null (integer 0))
                :initarg :line-number
                :initform nil
                :reader mps-syntax-condition-line-number)))
(define-condition mps-syntax-warning (simple-warning mps-syntax-condition) ())
(define-condition mps-syntax-error (simple-error mps-syntax-condition) ())

(defmethod print-object ((condition mps-syntax-condition) stream)
  (unless *print-escape*
    (when (mps-syntax-condition-line-number condition)
      (format stream "at line ~D: " (mps-syntax-condition-line-number condition))))
  (call-next-method))

(deftype sense () '(integer -1 1))

;; objective senses
(defconstant +maximize+ 1)
(defconstant +minimize+ -1)

;; constraint senses
(defconstant +ge+ 1)
(defconstant +eq+ 0)
(defconstant +le+ -1)

(defstruct variable
  (name nil :type string)
  (integer-p nil :type nil)
  (lo nil :type (or null real))
  (hi nil :type (or null real)))

(defstruct constraint
  (name nil :type string)
  (sense nil :type sense)
  (coefs nil :type hash-table)
  (rhs nil :type real))

(defstruct problem
  (name nil :type string)
  (sense nil :type sense))

(defun %split (string)
  (declare (optimize (speed 3))
           (string string))
  (delete-if-not (lambda (s) (> (length (the string s)) 0))
                 (the list (cl-ppcre:split "\\s+" string))))

(defun read-mps (stream &key (default-sense +minimize+))
  (check-type stream stream)
  (check-type default-sense sense)
  (let ((problem (make-problem :name "" :sense default-sense))
        mode
        (constraints (make-hash-table :test #'equal))
        (non-constraints (make-hash-table :test #'equal)))
    (loop
      for line-number from 0
      for line = (read-line stream nil nil)
      while line
      for items = (%split line)
      do (block continue
           (trivia:match (first items)
             ("ENDATA" (return))
             ("*" (return-from continue))
             ("NAME"
              (when (second items)
                (setf (problem-name problem) (second items)))
              (when (third items)
                (warn 'mps-syntax-warning
                      :line-number line-number
                      :format-control "NAME contains whitespaces: ~A"
                      :format-arguments (list line))))
             ((or "ROWS" "COLUMNS" "RHS" "BOUNDS" "RANGES" "OBJSENSE")
              (when (string= (first items) "RANGES")
                (warn 'mps-syntax-warning
                      :line-number line-number
                      :format-control "RANGES section will be ignored"))
              (setq mode (intern (first items) "CL-MPS")))
             (otherwise
              (ecase mode
                (rows)
                (columns)
                (rhs)
                (bounds)
                (ranges)
                (objsense
                 (setf (problem-sense problem)
                       (trivia:match (first items)
                         ("MAX" +maximize+)
                         ("MIN" +minimize+)
                         (otherwise
                          (error 'mps-syntax-error
                                 :line-number line-number
                                 :format-control "Unknown sense in OBJSENSE section: ~A"
                                 :format-arguments (subseq items 0 1)))))))))))
    problem))
