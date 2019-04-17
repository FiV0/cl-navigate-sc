#|
  This file is a part of cl-navigate-sc.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(asdf:defsystem cl-navigate-sc-test
  :name "cl-navitgate-sc-test"
  :version "1.0.0"
  :author "Finn Völkel <firstname.lastname@gmail.com>"
  :license "MIT Licence"
  :description "A library for extracting source code information from Common Lisp files.
Unit tests package."
  :components ((:file "cl-navigate-sc-test"))
  :depends-on (:cl-navigate-sc :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :cl-navigate-sc-test :run-test)))
