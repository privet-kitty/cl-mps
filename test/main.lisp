(defpackage cl-mps/test/main
  (:use :cl :cl-mps :fiveam)
  (:export #:main-suite))

(in-package :cl-mps/test/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-mps)' in your Lisp.

(def-suite main-suite)
(in-suite main-suite)

