;;;; cl-navigate-sc.asd

(asdf:defsystem #:cl-navigate-sc
  :description ""
  :author "fiv0"
  :license  "MIT Licence"
  :version "0.0.1"
  :serial t
  :depends-on (#:quicklisp #:eclector #:cl-git)
  :components ((:file "package")
               (:file "cl-navigate-sc")))
