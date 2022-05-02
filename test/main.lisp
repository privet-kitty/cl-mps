(defpackage cl-mps/test
  (:use :cl :cl-mps :fiveam)
  (:export #:main-suite))

(in-package :cl-mps/test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-mps)' in your Lisp.

(def-suite main-suite)
(in-suite main-suite)

(test read-mps
  (signals mps-syntax-warning
    (read-mps (make-string-input-stream "NAME two words")))
  (signals mps-syntax-warning
    (read-mps (make-string-input-stream "RANGES")))
  (signals mps-syntax-warning
    (read-mps (make-string-input-stream "ROWS
G ROW1
G ROW2
N ROW3
N ROW4
")))
  (signals mps-syntax-error
    (read-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3 ROW1 4")))
  (signals mps-syntax-error
    (read-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3 ROW2 4")))
  (signals mps-syntax-warning
    (read-mps (make-string-input-stream "COLUMNS
MARK 'MARKER' 'INTORH'
")))
  (signals mps-syntax-error
    (read-mps (make-string-input-stream "OBJSENSE
max?")))
  (signals mps-syntax-error
    (read-mps (make-string-input-stream "ROWS
G ROW1
H ROW2
")))
  (is (= (problem-sense (read-mps (make-string-input-stream "ENDATA"))) +minimize+))
  (is (= (problem-sense (read-mps (make-string-input-stream "ENDATA")
                                  :default-sense +maximize+))
         +maximize+))
  (let ((problem
          (handler-bind ((warning (lambda (c) (declare (ignorable c)) (error "warn"))))
            (read-mps (make-string-input-stream
                       "NAME          Problem_Name
ROWS
 N  COST
 L  LIM1
 G  LIM2
 E  MYEQN
COLUMNS
    XONE      COST                 1   LIM1                 1
    XONE      LIM2                 1
    MARK0     'MARKER'             'INTORG'
    YTWO      COST                 4   LIM1                 1
    YTWO      MYEQN               -1
    MARK0     'MARKER'             'INTEND'
    ZTHREE    COST                 9   LIM2                 1
    ZTHREE    MYEQN                1
RHS
    RHS1      LIM1                 5   LIM2                10
    RHS1      MYEQN                7
BOUNDS
 UP BND1      XONE                 4
 LO BND1      YTWO                -1
 UP BND1      YTWO                 1
ENDATA")
                      :default-sense +minimize+))))
    (is (string= (problem-name problem) "Problem_Name"))
    (is (string= (problem-objective-name problem) "COST"))
    (is (= (problem-sense problem) +minimize+))
    (symbol-macrolet ((constraints (problem-constraints problem))
                      (variables (problem-variables problem)))
      (is (= 3 (hash-table-count constraints)))
      (dolist (row-name '("LIM1" "LIM2" "MYEQN"))
        (is (gethash row-name constraints)))
      (is (eql +le+ (constraint-sense (gethash "LIM1" constraints))))
      (is (eql +ge+ (constraint-sense (gethash "LIM2" constraints))))
      (is (eql +eq+ (constraint-sense (gethash "MYEQN" constraints))))
      (is (= 3 (hash-table-count variables)))
      (dolist (col-name '("XONE" "YTWO" "ZTHREE"))
        (is (gethash col-name variables)))
      (is (not (var-integer-p (gethash "XONE" variables))))
      (is (var-integer-p (gethash "YTWO" variables)))
      (is (not (var-integer-p (gethash "ZTHREE" variables)))))))
