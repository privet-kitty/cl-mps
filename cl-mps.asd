;; -*- mode: lisp -*-

(defsystem "cl-mps"
  :version "0.0.1"
  :author "Hugo Sansaqua"
  :license "MIT"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "cl-mps"))))
  :description "Reader and writer for MPS format"
  :in-order-to ((test-op (test-op "cl-mps/test"))))

(defsystem "cl-mps/test"
  :pathname "test"
  :depends-on ("cl-mps"
               "fiveam")
  :components ((:file "main"))
  :description "Test system for cl-mps"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'cl-mps/test/main:main-suite)")))
