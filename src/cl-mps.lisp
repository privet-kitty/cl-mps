(defpackage :cl-mps
  (:use :cl)
  (:export #:mps-syntax-warning #:mps-syntax-error #:mps-syntax-condition-line-number
           #:sense #:+maximize+ #:+minimize+ #:+ge+ #:+eq+ #:+le+
           #:read-mps
           #:var #:make-var
           #:var-name #:var-integer-p #:var-lo #:var-hi
           #:constraint #:make-constraint
           #:constraint-name #:constraint-sense #:constraint-coefs #:constraint-rhs
           #:problem #:make-problem
           #:problem-name #:problem-sense))

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

(defstruct var ; avoid collision with cl:variable
  (name nil :type string)
  (integer-p nil :type nil)
  (lo nil :type (or null real))
  (hi nil :type (or null real)))

(defstruct constraint
  (name nil :type string)
  (sense nil :type (or null sense))
  (coefs nil :type hash-table)
  (rhs nil :type real))

(defstruct problem
  (name nil :type string)
  (sense nil :type sense)
  (variables nil :type hash-table)
  (constraints nil :type hash-table)
  (objective-name nil :type string)
  (objective nil :type hash-table))

(defun %split (string)
  (declare (optimize (speed 3))
           (string string))
  (delete-if-not (lambda (s) (> (length (the string s)) 0))
                 (the list (cl-ppcre:split "\\s+" string))))

(defun read-mps (stream &key (default-sense +minimize+))
  (check-type stream stream)
  (check-type default-sense sense)
  (let ((problem (make-problem :name ""
                               :sense default-sense
                               :variables (make-hash-table :test #'equal)
                               :constraints (make-hash-table :test #'equal)
                               :objective-name ""
                               :objective (make-hash-table :test #'equal)))
        mode
        (num-n 0))
    (symbol-macrolet ((constraints (problem-constraints problem))
                      (variables (problem-variables problem))
                      (objective (problem-objective problem))
                      (objective-name (problem-objective-name problem)))
      (loop
        for line-number from 0
        for line = (read-line stream nil nil)
        while line
        for items = (%split line)
        when items
        do (block continue
             (trivia:match (first items)
               ("ENDATA" (return))
               ((trivia.ppcre:ppcre "^\\*") (return-from continue))
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
                  (rows
                   (let ((sense (trivia:match (first items)
                                  ("N" (incf num-n) nil)
                                  ("G" +ge+)
                                  ("L" +le+)
                                  ("E" +eq+)
                                  (otherwise
                                   (error 'mps-syntax-error
                                          :line-number line-number
                                          :format-control "Unknown constraint sense: ~A"
                                          :format-arguments (subseq items 0 1)))))
                         (name (second items)))
                     (if sense
                         (if (gethash name constraints)
                             (error 'mps-syntax-error
                                    :line-number line-number
                                    :format-control "Row name ~A is already used"
                                    :format-arguments (list name))
                             (setf (gethash name constraints)
                                   (make-constraint :name name :sense sense
                                                    :coefs (make-hash-table :test #'equal)
                                                    :rhs 0)))
                         (if (= num-n 1)
                             (setq objective-name name)
                             (warn 'mps-syntax-warning
                                   :line-number line-number
                                   :format-control "Second or later N rows are ignored: ~A"
                                   :format-arguments (list line))))))
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
                                   :format-arguments (subseq items 0 1))))))))))))
    problem))
