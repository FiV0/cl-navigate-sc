#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(in-package #:cl-user)
(defpackage #:cl-navigate-sc.test
  (:use #:cl #:cl-navigate-sc #:parachute)
  (:shadow #:run)
  (:export #:cl-navigate-sc.test))

(in-package #:cl-navigate-sc.test)


;;Working with hunchentoot as an example. The request.lisp file below is from
;;hunchentoot.
(ql:quickload :hunchentoot)
(ql:quickload :alexandria)

;; file-location-read.lisp
(defvar filepath #P"./tmp/lisp-test-file.lisp")
(defvar filepath1 #P"./tmp/request.lisp")

(define-test test-calculate-line-breaks
  (let ((result (with-open-file (is filepath)
                  (calculate-line-breaks is))))
    (is #'eq'cons (type-of result))
    (is #'eq 10 (length result))))


(define-test test-find-if-consecutive
 (is-values (find-if-consecutive #'(lambda (x y)
                                     (declare (ignore x)) (< 7 y))
                                 '(1 3 6 8))
   (= 6)
   (= 3)))

(define-test test-file-position-to-location
  :depends-on (test-find-if-consecutive)
  (let* ((res (file-position-to-location 69 '(1 45)))
         (line (car res))
         (column (cdr res)))
    (is = 2 line)
    (is = 24 column)))

(define-test test-create-file-position-to-file-location-function
  :depends-on (test-calculate-line-breaks test-file-position-to-location)
  (let* ((res (with-open-file (is filepath)
               (let ((fn (create-file-position-to-file-location-function is)))
                 (funcall fn 209))))
         (line (car res))
         (column (cdr res)))
    (is = 5 line)
    (is = 25 column)))


(defvar program "(1 #|comment|# ;;test
                       \"string\")")

(define-test test-eclector-read
  (true (with-input-from-string (is program)
          (eclector.parse-result:read
            (make-instance 'symbol-location-client
                           :file-position-to-file-location
                           (create-file-position-to-file-location-function is))
            is))))

(define-test test-parse-from-file
 (true (parse-from-file filepath1)))

;; enviroment.lisp testing
(define-test test-set-get-environment
  (let* ((fl (make-instance 'file-location
                            :start-line 1
                            :end-line 2
                            :start 1
                            :end 2))
         (si (cl-navigate-sc::make-symbol-information
               'test))
         (sr (cl-navigate-sc::make-source-reference si fl))
         (env (cl-navigate-sc::empty-environment))
         (env2 (cl-navigate-sc::add-function-to-env env sr))
         (res (cl-navigate-sc::find-function 'test env2)))
    (is #'eq 'cl-navigate-sc::source-reference (type-of res))
    (is = 1 (cl-navigate-sc::file-location-start-line res))
    (is #'eq 'test (cl-navigate-sc::symbol-information-symbol res))))

;; cl-navigate-sc.lisp testing

(defvar program2 "(list 1 2 3)")

(define-test test-parse-cst-simple
  (let* ((cst (with-input-from-string (is program2)
                (car (read-program is))))
         (res (parse-cst cst (empty-environment)))
         (list-ref (caar res)))
    (is #'eq 'list (cl-navigate-sc::symbol-information-symbol list-ref))
    (is = 0 (cl-navigate-sc::file-location-start-line list-ref))
    (is = 1 (cl-navigate-sc::file-location-start list-ref))))

(test 'test-parse-cst-simple)
