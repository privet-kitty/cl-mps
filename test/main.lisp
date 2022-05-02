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
  (signals mps-syntax-error
    (read-mps (make-string-input-stream "OBJSENSE
max?")))
  (is (= (problem-sense (read-mps (make-string-input-stream "ENDATA"))) +minimize+))
  (is (= (problem-sense (read-mps (make-string-input-stream "ENDATA")
                                  :default-sense +maximize+))
         +maximize+))
  (let ((problem (read-mps (make-string-input-stream
                            "NAME Problem_Name
RANGES
OBJSENSE
MAX
"))))
    (is (string= (problem-name problem) "Problem_Name"))
    (is (= (problem-sense problem) +maximize+))))
