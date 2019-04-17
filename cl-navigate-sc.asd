;;;; cl-navigate-sc.asd

(asdf:defsystem #:cl-navigate-sc
  :description "A library for extracting source code information from CL files."
  :author "fiv0"
  :license  "MIT Licence"
  :version "0.0.1"
  :serial t
  :depends-on (#:quicklisp
               #:concrete-syntax-tree
               #:eclector
               #:eclector-concrete-syntax-tree
               #:hyperspec)
  :components ((:file "package")
               (:file "utils")
               (:file "file-location-read")
               (:file "environment")
               (:file "cl-navigate-sc")))
