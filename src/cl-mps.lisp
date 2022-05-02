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
           #:constraint-range #:constraint-lo #:constraint-up
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; objective senses
  (defconstant +maximize+ 1)
  (defconstant +minimize+ -1)

  ;; constraint senses
  (defconstant +ge+ 1)
  (defconstant +eq+ 0)
  (defconstant +le+ -1))

(defstruct var ; avoid collision with cl:variable
  "Holds information for variable. NIL for LO (or UP) slots means negative (or
positive) infinity."
  (name "" :type string)
  (integer-p nil :type boolean)
  (lo nil :type (or null real))
  (up nil :type (or null real)))

(defstruct constraint
  (name "" :type string)
  (sense nil :type (or null sense))
  (coefs (make-hash-table :test #'equal) :type hash-table)
  (rhs nil :type real)
  (range nil :type (or null real)))

(defun constraint-up (constraint)
  "Returns the upper bound of CONSTRAINT."
  (let ((range (constraint-range constraint))
        (rhs (constraint-rhs constraint))
        (sense (constraint-sense constraint)))
    (if range
        (ecase sense
          (#.+ge+ (+ rhs (abs range)))
          (#.+eq+ (max rhs (+ rhs range)))
          (#.+le+ rhs))
        (if (<= sense 0) rhs nil))))

(defun constraint-lo (constraint)
  "Returns the lower bound of CONSTRAINT."
  (let ((range (constraint-range constraint))
        (rhs (constraint-rhs constraint))
        (sense (constraint-sense constraint)))
    (if range
        (ecase sense
          (#.+ge+ rhs)
          (#.+eq+ (min rhs (+ rhs range)))
          (#.+le+ (- rhs (abs range))))
        (if (>= sense 0) rhs nil))))

(defstruct problem
  (name "" :type string)
  (sense nil :type sense)
  (variables (make-hash-table :test #'equal) :type hash-table)
  (constraints (make-hash-table :test #'equal) :type hash-table)
  (objective-name "" :type string)
  (objective (make-hash-table :test #'equal) :type hash-table))

(defun %split (string)
  (declare (optimize (speed 3))
           (string string))
  (delete-if-not (lambda (s) (> (length (the string s)) 0))
                 (the list (cl-ppcre:split "\\s+" string))))

(defun detect-section (items)
  (declare (optimize (speed 3)))
  (if (and (equal (first items) "OBJECT")
           (equal (second items) "BOUND"))
      (list "OBJECT BOUND" (cddr items))
      items))

(defun coerce* (value type)
  (declare (optimize (speed 3)))
  (if (eql type 'rational)
      (rationalize value)
      (coerce value type)))

(defun parse-real! (string line-number)
  "Note that STRING may be modified."
  (declare (optimize (speed 3))
           (string string))
  (uiop:if-let ((exp-pos (position #\D string :test #'char-equal)))
    (setf (aref string exp-pos) #\E))
  ;; NOTE: This function doesn't throw error even if STRING contains an invalid
  ;; exponent sign (i.e. S, F, or, L). In this case, the loss of precision may
  ;; also occur.
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (error 'mps-syntax-error
                                 :line-number line-number
                                 :format-control "~A cannot be parsed as a real number"
                                 :format-arguments (list string)))))
    (coerce* (read-from-string string) *read-default-float-format*)))

(defun read-mps (stream &key (default-sense +minimize+))
  "Reads MPS format and returns a PROBLEM object.

Note:
- OBJSENSE section takes precedence over DEFAULT-SENSE."
  (check-type stream stream)
  (check-type default-sense sense)
  (let ((problem (make-problem :sense default-sense))
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
        for items = (detect-section (%split line))
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
               ((or "OBJECT BOUND")
                (warn 'mps-syntax-warning
                      :line-number line-number
                      :format-control "~A section will be ignored"
                      :format-arguments (subseq items 0 1))
                (setq mode (intern (first items) "CL-MPS")))
               ((or "ROWS" "COLUMNS" "RHS" "RANGES" "BOUNDS" "RANGES" "OBJSENSE")
                (setq mode (intern (first items) "CL-MPS")))
               (otherwise
                (case mode
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
                                   (make-constraint :name name :sense sense :rhs 0)))
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
                           for coef = (parse-real! coef-string line-number)
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
                  ((rhs ranges)
                   (loop for (row-name value-string) on (if (oddp (length items))
                                                            (cdr items)
                                                            items)
                         by #'cddr
                         for value = (parse-real! value-string line-number)
                         for constraint = (gethash row-name constraints)
                         do (cond
                              ((gethash row-name non-constrained-rows)
                               (warn 'mps-syntax-warning
                                     :line-number line-number
                                     :format-control "~A is tried to be set for a N row"
                                     :format-arguments (list mode)))
                              (constraint
                               (ecase mode
                                 (rhs (setf (constraint-rhs constraint) value))
                                 (ranges (setf (constraint-range constraint) value))))
                              (t
                               (error 'mps-syntax-error
                                      :line-number line-number
                                      :format-control "Unknown row name: ~A"
                                      :format-arguments (list row-name))))))
                  (bounds
                   (let ((bound-type (first items))
                         bound-string col-name)
                     (unless (member bound-type '("LO" "UP" "FX" "FR" "MI" "PL"
                                                  "BV" "LI" "UI")
                                     :test #'string=)
                       (warn 'mps-syntax-warning
                             :line-number line-number
                             :format-control "Bound type ~A is ignored"
                             :format-arguments (list bound-type))
                       (return-from continue))
                     ;; HACK: An "empty" bound name is allowed in fixed MPS format
                     ;; and sometimes found in famous MPS data.
                     ;; For example, netlib/DFL001 contains the following line in
                     ;; BOUNDS section:
                     ;;  UP           C03609             14.
                     (if (member bound-type '("LO" "UP" "FX" "LI" "UI")
                                 :test #'string=)
                         (progn
                           (unless (<= 3 (length items) 4)
                             (error 'mps-syntax-error
                                    :line-number line-number
                                    :format-control "Invalid number of items"))
                           (destructuring-bind (i2 i3 &optional i4) (cdr items)
                             (if i4
                                 (multiple-value-setq (col-name bound-string)
                                   (values i3 i4))
                                 (multiple-value-setq (col-name bound-string)
                                   (values i2 i3)))))
                         (progn
                           (unless (<= 2 (length items) 3)
                             (error 'mps-syntax-error
                                    :line-number line-number
                                    :format-control "Invalid number of items"))
                           (destructuring-bind (i2 &optional i3) (cdr items)
                             (if i3
                                 (setq col-name i3)
                                 (setq col-name i2)))))
                     (let ((bound (when bound-string
                                    (parse-real! bound-string line-number)))
                           (var (gethash col-name variables)))
                       (unless var
                         (error 'mps-syntax-error
                                :line-number line-number
                                :format-control "Unknown column name: ~A"
                                :format-arguments (list col-name)))
                       (trivia:match bound-type
                         ("LO" (setf (var-lo var) bound))
                         ("UP" (setf (var-up var) bound))
                         ("FX" (setf (var-lo var) bound
                                (var-up var) bound))
                         ("FR" (setf (var-lo var) nil
                                (var-up var) nil))
                         ("MI" (setf (var-lo var) nil))
                         ("PL" (setf (var-up var) nil))
                         ("LI" (setf (var-integer-p var) t
                                (var-lo var) bound))
                         ("UI" (setf (var-integer-p var) t
                                (var-up var) bound))
                         ("BV" (setf (var-integer-p var) t
                                (var-lo var) 0
                                (var-up var) 1))
                         (otherwise (error "Unreachable"))))))
                  (objsense
                   (setf (problem-sense problem)
                         (trivia:match (first items)
                           ("MAX" +maximize+)
                           ("MIN" +minimize+)
                           (otherwise
                            (error 'mps-syntax-error
                                   :line-number line-number
                                   :format-control "Unknown sense in OBJSENSE section: ~A"
                                   :format-arguments (subseq items 0 1))))))
                  ((nil)
                   (warn 'mps-syntax-warning
                         :line-number line-number
                         :format-control "Non-comment line that belongs to no section is found and will be ignored: ~A"
                         :format-arguments (list line)))))))))
    problem))
