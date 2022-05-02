(defpackage :cl-mps
  (:use :cl)
  (:export #:mps-syntax-condition #:mps-syntax-warning #:mps-syntax-error
           #:mps-syntax-condition-line-number
           #:sense #:+maximize+ #:+minimize+ #:+ge+ #:+eq+ #:+le+
           #:read-mps
           #:var #:make-var
           #:var-name #:var-integer-p #:var-lo #:var-up
           #:constraint #:make-constraint
           #:constraint-name #:constraint-sense #:constraint-coefs #:constraint-rhs
           #:problem #:make-problem
           #:problem-name #:problem-sense #:problem-variables #:problem-constraints
           #:problem-objective #:problem-objective-name))

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
  "Holds information for variable. NIL for LO (or UP) slots means negative (or
positive) infinity."
  (name nil :type string)
  (integer-p nil :type boolean)
  (lo nil :type (or null real))
  (up nil :type (or null real)))

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

(defun parse-real! (string)
  "Note that STRING may be modified."
  (declare (optimize (speed 3))
           (string string))
  (uiop:if-let ((exp-pos (position #\D string :test #'char-equal)))
    (setf (aref string exp-pos) #\E))
  ;; NOTE: This function doesn't throw error even if STRING contains an invalid
  ;; exponent sign (i.e. S, F, or, L). In this case, the loss of precision may
  ;; also occur.
  (coerce (read-from-string string) *read-default-float-format*))

(defun read-mps (stream &key (default-sense +minimize+))
  "Reads MPS format and returns a PROBLEM object.

Note:
- OBJSENSE section takes precedence over DEFAULT-SENSE."
  (check-type stream stream)
  (check-type default-sense sense)
  (let ((problem (make-problem :name ""
                               :sense default-sense
                               :variables (make-hash-table :test #'equal)
                               :constraints (make-hash-table :test #'equal)
                               :objective-name ""
                               :objective (make-hash-table :test #'equal)))
        mode
        integer-p
        (non-constrained-rows (make-hash-table :test #'equal)))
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
                   (let* ((name (second items))
                          (sense (trivia:match (first items)
                                   ("N" nil)
                                   ("G" +ge+)
                                   ("L" +le+)
                                   ("E" +eq+)
                                   (otherwise
                                    (error 'mps-syntax-error
                                           :line-number line-number
                                           :format-control "Unknown constraint sense: ~A"
                                           :format-arguments (subseq items 0 1))))))
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
                         (if (and (setf (gethash name non-constrained-rows) t)
                                  (= (hash-table-count non-constrained-rows) 1))
                             (setq objective-name name)
                             (warn 'mps-syntax-warning
                                   :line-number line-number
                                   :format-control "Second or later N rows are ignored: ~A"
                                   :format-arguments (list line))))))
                  (columns
                   (if (string= (or (second items) "") "'MARKER'")
                       (trivia:match (third items)
                         ("'INTORG'" (setq integer-p t))
                         ("'INTEND'" (setq integer-p nil))
                         (otherwise
                          (warn 'mps-syntax-warning
                                :line-number line-number
                                :format-control "Unknown marker: ~A"
                                :format-arguments (subseq items 2 3))))
                       (let* ((name (car items)))
                         (unless (gethash name variables)
                           (setf (gethash name variables)
                                 (make-var :name name
                                           :integer-p integer-p
                                           :lo (coerce 0 *read-default-float-format*)
                                           :up nil)))
                         (loop
                           for (row-name coef-string) on (cdr items) by #'cddr
                           for coef = (parse-real! coef-string)
                           for constraint = (gethash row-name constraints)
                           do (cond
                                ((string= objective-name row-name)
                                 (setf (gethash name objective) coef))
                                ((gethash row-name non-constrained-rows))
                                (constraint
                                 (when (gethash name (constraint-coefs constraint))
                                   (error 'mps-syntax-error
                                          :line-number line-number
                                          :format-control "Duplicate columns in ROW ~A: ~A."
                                          :format-arguments (list row-name name)))
                                 (setf (gethash name (constraint-coefs constraint)) coef))
                                (t
                                 (error 'mps-syntax-error
                                        :line-number line-number
                                        :format-control "Unknown row name: ~A"
                                        :format-arguments (list row-name))))))))
                  (rhs
                   (loop for (row-name rhs-string) on (cdr items) by #'cddr
                         for rhs = (parse-real! rhs-string)
                         for constraint = (gethash row-name constraints)
                         do (cond
                              ((gethash row-name non-constrained-rows)
                               (error 'mps-syntax-error
                                      :line-number line-number
                                      :format-control "RHS is tried to be set for a N row"))
                              (constraint
                               (setf (constraint-rhs constraint) rhs))
                              (t
                               (error 'mps-syntax-error
                                      :line-number line-number
                                      :format-control "Unknown row name: ~A"
                                      :format-arguments (list row-name))))))
                  (bounds
                   (unless (<= 3 (length items) 4)
                     (error 'mps-syntax-error
                            :line-number line-number
                            :format-control "Invalid number of items"))
                   (destructuring-bind (bound-type _ col-name &optional bound-string) items
                     (declare (ignore _))
                     (let ((bound (when bound-string
                                    (parse-real! bound-string)))
                           (var (gethash col-name variables)))
                       (unless var
                         (error 'mps-syntax-error
                                :line-number line-number
                                :format-control "Unknown column name: ~A"
                                :format-arguments (list col-name)))
                       (labels ((check-bound ()
                                  (unless bound
                                    (error 'mps-syntax-error
                                           :line-number line-number
                                           :format-control "No bound given"))))
                         (trivia:match bound-type
                           ("LO" (check-bound)
                            (setf (var-lo var) bound))
                           ("UP" (check-bound)
                            (setf (var-up var) bound))
                           ("FX" (check-bound)
                            (setf (var-lo var) bound
                             (var-up var) bound))
                           ("FR" (setf (var-lo var) nil
                                  (var-up var) nil))
                           ("MI" (setf (var-lo var) nil))
                           ("PL" (setf (var-up var) nil))
                           ("BV" (setf (var-integer-p var) t
                                  (var-lo var) 0
                                  (var-up var) 1))
                           ("LI" (check-bound)
                            (setf (var-integer-p var) t
                             (var-lo var) bound))
                           ("UI" (check-bound)
                            (setf (var-integer-p var) t
                             (var-up var) bound))
                           ("SC" (warn 'mps-syntax-warning
                                  :line-number line-number
                                  :format-control "Bound type SC is ignored")))))))
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
