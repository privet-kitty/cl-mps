(defpackage :cl-mps
  (:use :cl)
  (:export #:mps-syntax-warning #:mps-syntax-warning-line-number))

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
