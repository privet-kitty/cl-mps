;; -*- mode: lisp -*-

(defsystem "cl-mps"
  :version "0.1.0"
  :author "Hugo Sansaqua"
  :license "MIT"
  :depends-on ("cl-ppcre" "trivia" "trivia.ppcre")
  :components ((:module "src"
                :components
                ((:file "cl-mps"))))
  :description "Reader for MPS format"
  :in-order-to ((test-op (test-op "cl-mps/test"))))

(defsystem "cl-mps/test"
  :pathname "test"
  :depends-on ("cl-mps"
               "fiveam")
  :components ((:file "main"))
  :description "Test system for cl-mps"
  :perform (test-op (o c)
                    (uiop:eval-input "(fiveam:run! 'cl-mps/test:main-suite)")))
