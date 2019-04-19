#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(in-package #:cl-user)
(defpackage #:cl-navigate-sc-test
  (:use #:cl #:cl-navigate-sc #:parachute)
  (:shadow #:run)
  (:import-from #:cl-navigate-sc
                #:file-location-start-line
                #:file-location-end-line
                #:file-location-start
                #:file-location-end
                #:process-function-binding
                #:source-reference-parent
                #:symbol-information-symbol)
  (:export #:cl-navigate-sc-test))

(in-package #:cl-navigate-sc-test)


;;Working with hunchentoot as an example. The request.lisp file below is from
;;hunchentoot.
(ql:quickload :hunchentoot)
(ql:quickload :alexandria)

;; file-location-read.lisp
(defvar filepath #P"./tmp/lisp-test-file.lisp")
(defvar filepath1 #P"./tmp/request.lisp")

(define-test calculate-line-breaks
  (let ((result (with-open-file (is filepath)
                  (calculate-line-breaks is))))
    (is #'eq'cons (type-of result))
    (is #'eq 10 (length result))))


(define-test find-if-consecutive
 (is-values (find-if-consecutive #'(lambda (x y)
                                     (declare (ignore x)) (< 7 y))
                                 '(1 3 6 8))
   (= 6)
   (= 3)))

(define-test file-position-to-location
  :depends-on (find-if-consecutive)
  (let* ((res (file-position-to-location 69 '(1 45)))
         (line (car res))
         (column (cdr res)))
    (is = 2 line)
    (is = 24 column)))

(define-test create-file-position-to-file-location-function
  :depends-on (calculate-line-breaks file-position-to-location)
  (let* ((res (with-open-file (is filepath)
               (let ((fn (create-file-position-to-file-location-function is)))
                 (funcall fn 209))))
         (line (car res))
         (column (cdr res)))
    (is = 5 line)
    (is = 25 column)))


(defvar program "(1 #|comment|# ;;test
                       \"string\")")

(define-test eclector-read
  (true (with-input-from-string (is program)
          (eclector.parse-result:read
            (make-instance 'symbol-location-client
                           :file-position-to-file-location
                           (create-file-position-to-file-location-function is))
            is))))

(define-test parse-from-file
  :depends-on (eclector-read)
 (true (parse-from-file filepath1)))

;; enviroment.lisp testing
(define-test set-get-environment
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

(defun read-one-cst (string)
  (with-input-from-string (is string)
    (car (read-program is))))

(defvar program2 "(list 1 2 3)")

(define-test parse-cst-simple
  (let* ((cst (read-one-cst program2))
         (res (parse-cst cst (empty-environment)))
         (list-ref (car res)))
    (is #'eq 'list (symbol-information-symbol list-ref))
    (is = 0 (file-location-start-line list-ref))
    (is = 1 (file-location-start list-ref))))

(defvar program3 "(let ((x 1)
                        (y 2))
                    (print x)
                    (list x y))")

(define-test parse-let
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program3))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x (nth 1 res))
         (sr-print-x (nth 4 res))
         (sr-y (nth 2 res))
         (sr-list-y (nth 7 res)))
    (is #'eq sr-x (source-reference-parent sr-print-x))
    (is #'eq sr-y (source-reference-parent sr-list-y))))

(defvar program4 "(let* ((x 1)
                         (y x))
                    (print y))")

(define-test parse-let*
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program4))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x (nth 1 res))
         (sr-form-x (nth 3 res))
         (sr-y (nth 2 res))
         (sr-print-y (nth 5 res)))
    (is #'eq sr-x (source-reference-parent sr-form-x))
    (is #'eq sr-y (source-reference-parent sr-print-y))))

(defvar program5 "(flet ((id (x) x)
                         (id2 (x) x))
                    (id 1))")

(define-test parse-flet
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program5))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the

         ;; order of the source references
         (sr-id (nth 1 res))
         (sr-call-id (nth 7 res))
         (sr-x-2 (nth 5 res))
         (sr-eval-x-2 (nth 6 res)))
    (is #'eq sr-id (source-reference-parent sr-call-id))
    (is #'eq sr-x-2 (source-reference-parent sr-eval-x-2))))