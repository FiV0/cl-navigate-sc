#|
  This file is a part of cl-navigate-sc.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(asdf:defsystem #:cl-navigate-sc
  :description "A library for extracting source code information from CL files."
  :author "Finn Völkel <firstname.lastname@gmail.com>"
  :license  "MIT Licence"
  :version "0.0.1"
  :serial t
  :depends-on (#:asdf
               #:cl-fad
               #:quicklisp
               #:concrete-syntax-tree
               #:eclector
               #:eclector-concrete-syntax-tree
               #:hyperspec)
  :components ((:file "package")
               (:file "utils")
               (:file "file-location-read")
               (:file "environment")
               (:file "lambda-list-helpers")
               (:file "cl-navigate-sc")
               (:file "process-project"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-navigate-sc-test))))
