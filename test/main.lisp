(defpackage cl-mps/test
  (:use :cl :cl-mps :fiveam)
  (:export #:main-suite))

(in-package :cl-mps/test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-mps)' in your Lisp.

(def-suite main-suite)
(in-suite main-suite)

(test constraint-lo-up
  ;; ==
  (is (= 3 (constraint-lo (make-constraint :sense +eq+ :rhs 3))))
  (is (= 3 (constraint-up (make-constraint :sense +eq+ :rhs 3))))
  (is (= 3 (constraint-lo (make-constraint :sense +eq+ :rhs 3 :range 10))))
  (is (= 13 (constraint-up (make-constraint :sense +eq+ :rhs 3 :range 10))))
  (is (= -7 (constraint-lo (make-constraint :sense +eq+ :rhs 3 :range -10))))
  (is (= 3 (constraint-up (make-constraint :sense +eq+ :rhs 3 :range -10))))
  ;; >=
  (is (= 3 (constraint-lo (make-constraint :sense +ge+ :rhs 3))))
  (is (null (constraint-up (make-constraint :sense +ge+ :rhs 3))))
  (is (= 3 (constraint-lo (make-constraint :sense +ge+ :rhs 3 :range 10))))
  (is (= 13 (constraint-up (make-constraint :sense +ge+ :rhs 3 :range 10))))
  (is (= 3 (constraint-lo (make-constraint :sense +ge+ :rhs 3 :range -10))))
  (is (= 13 (constraint-up (make-constraint :sense +ge+ :rhs 3 :range -10))))
  ;; <=
  (is (null (constraint-lo (make-constraint :sense +le+ :rhs 3))))
  (is (= 3 (constraint-up (make-constraint :sense +le+ :rhs 3))))
  (is (= -7 (constraint-lo (make-constraint :sense +le+ :rhs 3 :range 10))))
  (is (= 3 (constraint-up (make-constraint :sense +le+ :rhs 3 :range 10))))
  (is (= -7 (constraint-lo (make-constraint :sense +le+ :rhs 3 :range -10))))
  (is (= 3 (constraint-up (make-constraint :sense +le+ :rhs 3 :range -10)))))

(test read-fixed-mps
  ;; invalid ROWS
  (signals mps-syntax-warning
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
 G  ROW2
 N  ROW3
 N  ROW4")))
  (finishes
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
 G  ROW2
 N  ROW3")))
  (signals mps-syntax-error
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
 H  ROW2")))
  ;; invalid COLUMNS
  (signals mps-syntax-error
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3              ROW1      4")))
  (finishes
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3              ")))
  (signals mps-syntax-error
    (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3              ROW2      4")))
  ;; invalid marker
  (signals mps-syntax-warning
    (read-fixed-mps (make-string-input-stream "COLUMNS
    MARK      'MARKER'                 'INTORH'")))
  (finishes
    (read-fixed-mps (make-string-input-stream "COLUMNS
    MARK      'MARKER'                 'INTORG'")))
  ;; invalid RHS
  (signals mps-syntax-error (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
RHS
    RHS1      ROW3      3")))
  (finishes (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
RHS
    RHS1      ROW1      3")))
  ;; invalid BOUNDS
  (signals mps-syntax-warning (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3
BOUNDS
 UP BND1      COL1      -3")))
  (signals mps-syntax-error (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3
BOUNDS
 LO BND1      COL1")))
  (finishes (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3
BOUNDS
 LO BND1      COL1      5")))
  (finishes (read-fixed-mps (make-string-input-stream "ROWS
 G  ROW1
COLUMNS
    COL1      ROW1      3
BOUNDS
 BV BND1      COL1")))
  ;; invalid OBJSENSE
  (signals mps-syntax-error
    (read-fixed-mps (make-string-input-stream "OBJSENSE
  max?")))
  (is (eql +maximize+ (problem-sense (read-fixed-mps (make-string-input-stream "OBJSENSE
  MAX")))))
  (is (= (problem-sense (read-fixed-mps (make-string-input-stream "ENDATA"))) +minimize+))
  (is (= (problem-sense (read-fixed-mps (make-string-input-stream "ENDATA")
                                        :default-sense +maximize+))
         +maximize+))
  (dolist (*read-default-float-format* '(double-float single-float #+(or sbcl clozure) rational))
    (let ((problem (read-fixed-mps (make-string-input-stream
                                    "NAME          Prob
ROWS
 N  COST
 L  LIM1
 G  LIM2
 E  MYEQN
COLUMNS
    XONE      COST                 1   LIM1                 1
    XONE      LIM2                 1
    MARK0     'MARKER'                 'INTORG'
    YTWO      COST                 4   LIM1                 1
    YTWO      MYEQN               -1
    MARK0     'MARKER'                 'INTEND'
    ZTHREE    COST                 9   LIM2                 1
    ZTHREE    MYEQN                1
    WFOUR     LIM1                 3
RHS
    RHS1      LIM1               5e3   LIM2              1D-3
    RHS1      MYEQN             7.23
    RHS1      COST              -1E2
BOUNDS
 MI BND1      XONE
 UI BND1      XONE                 4
 LO BND1      YTWO                -1
 UP BND1      YTWO                 1
 UP BND1      WFOUR               -1
RANGES
    RNG1      LIM1               0.5
ENDATA")
                                   :default-sense +minimize+)))
      (is (string= (problem-name problem) "Prob    "))
      (is (= (problem-sense problem) +minimize+))
      (symbol-macrolet ((constraints (problem-constraints problem))
                        (variables (problem-variables problem))
                        (objective (problem-objective problem)))
        (is (= 3 (hash-table-count constraints)))
        (dolist (row-name '("LIM1    " "LIM2    " "MYEQN   "))
          (is (gethash row-name constraints)))
        (is (eql +le+ (constraint-sense (gethash "LIM1    " constraints))))
        (is (eql +ge+ (constraint-sense (gethash "LIM2    " constraints))))
        (is (eql +eq+ (constraint-sense (gethash "MYEQN   " constraints))))
        (is (eql (coerce 5000 *read-default-float-format*)
                 (constraint-rhs (gethash "LIM1    " constraints))))
        (is (eql (coerce 1/1000 *read-default-float-format*)
                 (constraint-rhs (gethash "LIM2    " constraints))))
        (is (eql (coerce 723/100 *read-default-float-format*)
                 (constraint-rhs (gethash "MYEQN   " constraints))))
        (is (eql (cl-mps::coerce* 0.5 *read-default-float-format*)
                 (constraint-range (gethash "LIM1    " constraints))))
        (is (null (constraint-range (gethash "LIM2    " constraints))))
        (is (null (constraint-range (gethash "MYEQN   " constraints))))
        (is (string= "COST    " (constraint-name objective)))
        (is (eql (coerce -100 *read-default-float-format*) (constraint-rhs objective)))
        (let ((coefs (constraint-coefs objective)))
          (is (= 3 (hash-table-count coefs)))
          (is (eql (coerce 1 *read-default-float-format*) (gethash "XONE    " coefs)))
          (is (eql (coerce 4 *read-default-float-format*) (gethash "YTWO    " coefs)))
          (is (eql (coerce 9 *read-default-float-format*) (gethash "ZTHREE  " coefs))))
        (is (= 4 (hash-table-count variables)))
        (loop for (col row val) in '(("XONE    " "LIM1    " 1) ("XONE    " "LIM2    " 1)
                                     ("YTWO    " "LIM1    " 1) ("YTWO    " "MYEQN   " -1)
                                     ("ZTHREE  " "LIM2    " 1) ("ZTHREE  " "MYEQN   " 1))
              for coef = (gethash col (constraint-coefs (gethash row constraints)))
              do (is (= coef val)))
        (dolist (col-name '("XONE    " "YTWO    " "ZTHREE  " "WFOUR   "))
          (is (gethash col-name variables)))
        (is (var-integer-p (gethash "XONE    " variables)))
        (is (var-integer-p (gethash "YTWO    " variables)))
        (is (not (var-integer-p (gethash "ZTHREE  " variables))))
        (is (not (var-integer-p (gethash "WFOUR   " variables))))
        (is (null (var-lo (gethash "XONE    " variables))))
        (is (eql (coerce 4 *read-default-float-format*)
                 (var-up (gethash "XONE    " variables))))
        (is (eql (coerce -1 *read-default-float-format*)
                 (var-lo (gethash "YTWO    " variables))))
        (is (eql (coerce 1 *read-default-float-format*)
                 (var-up (gethash "YTWO    " variables))))
        (is (eql (coerce 0 *read-default-float-format*)
                 (var-lo (gethash "ZTHREE  " variables))))
        (is (null (var-up (gethash "ZTHREE  " variables))))
        (is (null (var-lo (gethash "WFOUR   " variables))))
        (is (eql (coerce -1 *read-default-float-format*)
                 (var-up (gethash "WFOUR   " variables))))))))

(test read-free-mps
  (signals mps-syntax-warning
    (read-free-mps (make-string-input-stream "NAME two words")))
  ;; invalid ROWS
  (signals mps-syntax-warning
    (read-free-mps (make-string-input-stream "ROWS
G ROW1
G ROW2
N ROW3
N ROW4
")))
  (signals mps-syntax-error
    (read-free-mps (make-string-input-stream "ROWS
G ROW1
H ROW2
")))
  ;; invalid COLUMNS
  (signals mps-syntax-error
    (read-free-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3 ROW1 4")))
  (signals mps-syntax-error
    (read-free-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3 ROW2 4")))
  ;; invalid marker
  (signals mps-syntax-warning
    (read-free-mps (make-string-input-stream "COLUMNS
MARK 'MARKER' 'INTORH'
")))
  ;; invalid RHS
  (signals mps-syntax-error (read-free-mps (make-string-input-stream "ROWS
G ROW1
RHS
RHS1 ROW3 3")))
  ;; invalid BOUNDS
  (signals mps-syntax-warning (read-free-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3
BOUNDS
UP BND1 COL1 -3")))
  (signals mps-syntax-error (read-free-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3
BOUNDS
LO BND1 COL1")))
  (finishes (read-free-mps (make-string-input-stream "ROWS
G ROW1
COLUMNS
COL1 ROW1 3
BOUNDS
BV BND1 COL1")))
  ;; invalid OBJSENSE
  (signals mps-syntax-error
    (read-free-mps (make-string-input-stream "OBJSENSE
max?")))
  (is (eql +maximize+ (problem-sense (read-fixed-mps (make-string-input-stream "OBJSENSE
  MAX")))))

  (is (= (problem-sense (read-free-mps (make-string-input-stream "ENDATA"))) +minimize+))
  (is (= (problem-sense (read-free-mps (make-string-input-stream "ENDATA")
                                       :default-sense +maximize+))
         +maximize+))
  (dolist (*read-default-float-format* '(double-float single-float #+(or sbcl clozure) rational))
    (let ((problem (read-free-mps (make-string-input-stream
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
    WFOUR     LIM1                 3
RHS
    RHS1      LIM1                 5e3 LIM2                1D-3
    RHS1      MYEQN                7.23
    RHS1      COST                 -1E2
BOUNDS
 MI BND1      XONE
 UI BND1      XONE                 4
 LO BND1      YTWO                -1
 UP BND1      YTWO                 1
 UP BND1      WFOUR                -1
RANGES
 RNG1 LIM1 0.5
ENDATA")
                                  :default-sense +minimize+)))
      (is (string= (problem-name problem) "Problem_Name"))
      (is (= (problem-sense problem) +minimize+))
      (symbol-macrolet ((constraints (problem-constraints problem))
                        (variables (problem-variables problem))
                        (objective (problem-objective problem)))
        (is (= 3 (hash-table-count constraints)))
        (dolist (row-name '("LIM1" "LIM2" "MYEQN"))
          (is (gethash row-name constraints)))
        (is (eql +le+ (constraint-sense (gethash "LIM1" constraints))))
        (is (eql +ge+ (constraint-sense (gethash "LIM2" constraints))))
        (is (eql +eq+ (constraint-sense (gethash "MYEQN" constraints))))
        (is (eql (coerce 5000 *read-default-float-format*)
                 (constraint-rhs (gethash "LIM1" constraints))))
        (is (eql (coerce 1/1000 *read-default-float-format*)
                 (constraint-rhs (gethash "LIM2" constraints))))
        (is (eql (coerce 723/100 *read-default-float-format*)
                 (constraint-rhs (gethash "MYEQN" constraints))))
        (is (eql (cl-mps::coerce* 0.5 *read-default-float-format*)
                 (constraint-range (gethash "LIM1" constraints))))
        (is (null (constraint-range (gethash "LIM2" constraints))))
        (is (null (constraint-range (gethash "MYEQN" constraints))))
        (is (string= "COST" (constraint-name objective)))
        (is (eql (coerce -100 *read-default-float-format*) (constraint-rhs objective)))
        (let ((coefs (constraint-coefs objective)))
          (is (= 3 (hash-table-count coefs)))
          (is (eql (coerce 1 *read-default-float-format*) (gethash "XONE" coefs)))
          (is (eql (coerce 4 *read-default-float-format*) (gethash "YTWO" coefs)))
          (is (eql (coerce 9 *read-default-float-format*) (gethash "ZTHREE" coefs))))
        (is (= 4 (hash-table-count variables)))
        (loop for (col row val) in '(("XONE" "LIM1" 1) ("XONE" "LIM2" 1)
                                     ("YTWO" "LIM1" 1) ("YTWO" "MYEQN" -1)
                                     ("ZTHREE" "LIM2" 1) ("ZTHREE" "MYEQN" 1))
              for coef = (gethash col (constraint-coefs (gethash row constraints)))
              do (is (= coef val)))
        (dolist (col-name '("XONE" "YTWO" "ZTHREE" "WFOUR"))
          (is (gethash col-name variables)))
        (is (var-integer-p (gethash "XONE" variables)))
        (is (var-integer-p (gethash "YTWO" variables)))
        (is (not (var-integer-p (gethash "ZTHREE" variables))))
        (is (not (var-integer-p (gethash "WFOUR" variables))))
        (is (null (var-lo (gethash "XONE" variables))))
        (is (eql (coerce 4 *read-default-float-format*)
                 (var-up (gethash "XONE" variables))))
        (is (eql (coerce -1 *read-default-float-format*)
                 (var-lo (gethash "YTWO" variables))))
        (is (eql (coerce 1 *read-default-float-format*)
                 (var-up (gethash "YTWO" variables))))
        (is (eql (coerce 0 *read-default-float-format*)
                 (var-lo (gethash "ZTHREE" variables))))
        (is (null (var-up (gethash "ZTHREE" variables))))
        (is (null (var-lo (gethash "WFOUR" variables))))
        (is (eql (coerce -1 *read-default-float-format*)
                 (var-up (gethash "WFOUR" variables))))))))
