(defpackage :cl-mps
  (:use :cl #:trivia.ppcre #:trivia)
  (:export #:mps-syntax-condition #:mps-syntax-warning #:mps-syntax-error
           #:mps-syntax-condition-line-number
           #:sense #:+maximize+ #:+minimize+ #:+ge+ #:+eq+ #:+le+
           #:read-fixed-mps #:read-free-mps
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
  (rhs 0 :type real)
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
  (objective nil :type constraint))

(defun %split (string)
  (declare (optimize (speed 3))
           (string string))
  (delete-if-not (lambda (s) (> (length (the string s)) 0))
                 (the list (cl-ppcre:split "\\s+" string))))

;; Hack for netlib/AGG
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

(defun subseq* (line l r)
  (declare (optimize (speed 3))
           (string line)
           ((mod #.array-dimension-limit) l r))
  (let ((res (make-string (- r l) :element-type 'base-char :initial-element #\Space)))
    (replace res line :start2 l :end2 (min (length line) r))
    res))

(defun parse-fixed-items (line)
  (declare (optimize (speed 3))
           (string line))
  (loop for (l . r) in '((1 . 4) (4 . 12) (14 . 22) (24 . 36) (39 . 47) (49 . 61))
        while (< l (length line))
        collect (subseq* line l r)))

(defun parse-section-name (line)
  (declare (optimize (speed 3))
           (string line))
  (multiple-value-bind (l r)
      (ppcre:scan "^(ENDATA|\\*|NAME|OBJECT BOUND|ROWS|COLUMNS|RHS|RANGES|BOUNDS|OBJSENSE)"
                  line)
    (when (and l r)
      (subseq line l r))))

(defun proc-rows (items constraints non-constrained-rows objective line-number)
  (let* ((name (second items))
         (sense (match (first items)
                  ((ppcre "^\\s*N\\s*$") nil)
                  ((ppcre "^\\s*G\\s*$") +ge+)
                  ((ppcre "^\\s*L\\s*$") +le+)
                  ((ppcre "^\\s*E\\s*$") +eq+)
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
            (setf (constraint-name objective) name)
            (warn 'mps-syntax-warning
                  :line-number line-number
                  :format-control "Second or later N rows are ignored: ~A"
                  :format-arguments (list name))))))

(defun proc-bounds (items variables line-number)
  (unless (<= 3 (length items) 4)
    (error 'mps-syntax-error
           :line-number line-number
           :format-control "Invalid number of items"))
  (destructuring-bind (bound-type _ col-name &optional bound-string) items
    (declare (ignore _))
    (let ((bound (when bound-string
                   (parse-real! bound-string line-number)))
          (var (gethash col-name variables)))
      (unless var
        (error 'mps-syntax-error
               :line-number line-number
               :format-control "Unknown column name: ~A"
               :format-arguments (list col-name)))
      (labels ((check-bound ()
                 (unless bound-string
                   (error 'mps-syntax-error
                          :line-number line-number
                          :format-control "No bound given"))))
        (match bound-type
          ((ppcre "^LO")
           (check-bound)
           (setf (var-lo var) bound))
          ((ppcre "^UP")
           (check-bound)
           (setf (var-up var) bound)
           ;; Follow CPLEX behaviour
           ;; See https://www.ibm.com/docs/ar/icos/20.1.0?topic=standard-records-in-mps-format
           (when (and (< bound 0)
                      (zerop (var-lo var)))
             (warn
              'mps-syntax-warning
              :line-number line-number
              :format-control "Default LO value of ~A is assumed to be -inf as UP is set to ~A"
              :format-arguments (list col-name bound))
             (setf (var-lo var) nil)))
          ((ppcre "^FX")
           (check-bound)
           (setf (var-lo var) bound
                 (var-up var) bound))
          ((ppcre "^FR")
           (setf (var-lo var) nil
                 (var-up var) nil))
          ((ppcre "^MI")
           (setf (var-lo var) nil))
          ((ppcre "^PL")
           (setf (var-up var) nil))
          ((ppcre "^LI")
           (check-bound)
           (setf (var-integer-p var) t
                 (var-lo var) bound))
          ((ppcre "^UI")
           (check-bound)
           (setf (var-integer-p var) t
                 (var-up var) bound))
          ((ppcre "^BV")
           (setf (var-integer-p var) t
                 (var-lo var) 0
                 (var-up var) 1))
          (otherwise
           (warn 'mps-syntax-warning
                 :line-number line-number
                 :format-control "Bound type ~A is ignored"
                 :format-arguments (list bound-type))))))))

(defun proc-objsense (line problem line-number)
  (setf (problem-sense problem)
        (match line
          ((ppcre "MAX") +maximize+)
          ((ppcre "MIN") +minimize+)
          (otherwise
           (error 'mps-syntax-error
                  :line-number line-number
                  :format-control "Unknown sense in OBJSENSE section: ~A"
                  :format-arguments line)))))

(defun read-fixed-mps (stream &key (default-sense +minimize+))
  "Reads fixed MPS format and returns a PROBLEM object.

Note:
- OBJSENSE section takes precedence over DEFAULT-SENSE."
  (check-type stream stream)
  (check-type default-sense sense)
  (let* ((objective (make-constraint))
         (problem (make-problem :sense default-sense :objective objective))
         mode
         integer-p
         (non-constrained-rows (make-hash-table :test #'equal)))
    (symbol-macrolet ((constraints (problem-constraints problem))
                      (variables (problem-variables problem))
                      (objective-name (constraint-name objective))
                      (objective-coefs (constraint-coefs objective)))
      (loop
        for line-number from 0
        for line = (read-line stream nil nil)
        while line
        for items = (parse-fixed-items line)
        when (find #\Space line :test-not #'char=)
        when (char= #\Space (aref line 0))
        do (case mode
             (rows
              (proc-rows items constraints non-constrained-rows objective line-number))
             (columns
              (destructuring-bind (_ name row-name1 coef1 &optional row-name2 coef2) items
                (declare (ignore _))
                (if (ppcre:scan "^'MARKER'" row-name1)
                    (match (or row-name2 "")
                      ((ppcre "^'INTORG'") (setq integer-p t))
                      ((ppcre "^'INTEND'") (setq integer-p nil))
                      (otherwise
                       (warn 'mps-syntax-warning
                             :line-number line-number
                             :format-control "Unknown marker: ~A"
                             :format-arguments (list coef1))))
                    (labels
                        ((frob (row-name coef-string)
                           (let ((coef (parse-real! coef-string line-number))
                                 (constraint (gethash row-name constraints)))
                             (cond
                               ((string= objective-name row-name)
                                (setf (gethash name objective-coefs) coef))
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
                                       :format-arguments (list row-name)))))))
                      (unless (gethash name variables)
                        (setf (gethash name variables)
                              (make-var :name name
                                        :integer-p integer-p
                                        :lo (coerce 0 *read-default-float-format*)
                                        :up nil)))
                      (frob row-name1 coef1)
                      (when row-name2
                        (if coef2
                            (frob row-name2 coef2)
                            (warn 'mps-syntax-warning
                                  :line-number line-number
                                  :format-control "No value given to row ~A"
                                  :format-arguments (list row-name2))))))))
             ((rhs ranges)
              (destructuring-bind (_ __ row-name1 value1 &optional row-name2 value2) items
                (declare (ignore _ __))
                (labels ((frob (row-name value-string)
                           (let ((value (parse-real! value-string line-number))
                                 (constraint (gethash row-name constraints)))
                             (cond
                               ((and (eql mode 'rhs)
                                     (string= row-name objective-name))
                                (setf (constraint-rhs objective) value))
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
                                       :format-arguments (list row-name)))))))
                  (frob row-name1 value1)
                  (when row-name2
                    (if value2
                        (frob row-name2 value2)
                        (warn 'mps-syntax-warning
                              :line-number line-number
                              :format-control "No value given to row ~A"
                              :format-arguments (list row-name2)))))))
             (bounds (proc-bounds items variables line-number))
             (objsense (proc-objsense line problem line-number))
             ((nil)
              (warn 'mps-syntax-warning
                    :line-number line-number
                    :format-control "Non-comment line that belongs to no section is found and will be ignored: ~A"
                    :format-arguments (list line))))
        else
        do (let ((section (parse-section-name line)))
             (match section
               ("ENDATA" (return))
               ("*")
               ("NAME"
                (if (third items)
                    (setf (problem-name problem) (third items))
                    (warn 'mps-syntax-warning
                          :line-number line-number
                          :format-control "No NAME given")))
               ("OBJECT BOUND"
                (warn 'mps-syntax-warning
                 :line-number line-number
                 :format-control "~A section will be ignored"
                 :format-arguments (list section))
                (setq mode (intern section "CL-MPS")))
               ((or "ROWS" "COLUMNS" "RHS" "RANGES" "BOUNDS" "OBJSENSE")
                (setq mode (intern section "CL-MPS")))
               (otherwise
                (warn 'mps-syntax-warning
                      :line-number line-number
                      :format-control "Unknown section is ignored: ~A"
                      :format-arguments (list line)))))))
    problem))

(defun read-free-mps (stream &key (default-sense +minimize+))
  "Reads free MPS format and returns a PROBLEM object.

Note:
- OBJSENSE section takes precedence over DEFAULT-SENSE.
- The length of the item (e.g. variable) name is arbitrary.
- You cannot use whitespaces in item names. More precisely, characters that
  match the regex `\s` are regarded as separators. Therefore, all the blanks at
  both end of item names are trimmed.
- In COLUMN and RHS section, you can put three or more variables on a single
  line. (In fixed MPS format, only one or two variables are allowed.)
- The following words are reserved and cannot be used as item names: `NAME`,
  `ENDATA`, `ROWS`, `COLUMNS`, `RHS`, `BOUNDS`, `RANGES`, `OBJSENSE`, and
  `OBJECT BOUND`.
"
  (check-type stream stream)
  (check-type default-sense sense)
  (let* ((objective (make-constraint))
         (problem (make-problem :sense default-sense :objective objective))
         mode
         integer-p
         (non-constrained-rows (make-hash-table :test #'equal)))
    (symbol-macrolet ((constraints (problem-constraints problem))
                      (variables (problem-variables problem))
                      (objective-name (constraint-name objective))
                      (objective-coefs (constraint-coefs objective)))
      (loop
        for line-number from 0
        for line = (read-line stream nil nil)
        while line
        for items = (detect-section (%split line))
        when items
        do (block continue
             (match (first items)
               ("ENDATA" (return))
               ((ppcre "^\\*") (return-from continue))
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
                   (proc-rows items constraints non-constrained-rows objective line-number))
                  (columns
                   (if (string= (or (second items) "") "'MARKER'")
                       (match (third items)
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
                                 (setf (gethash name objective-coefs) coef))
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
                              ((and (eql mode 'rhs)
                                    (string= row-name objective-name))
                               (setf (constraint-rhs objective) value))
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
                  (bounds (proc-bounds items variables line-number))
                  (objsense (proc-objsense line problem line-number))
                  ((nil)
                   (warn 'mps-syntax-warning
                         :line-number line-number
                         :format-control "Non-comment line that belongs to no section is found and will be ignored: ~A"
                         :format-arguments (list line)))))))))
    problem))
