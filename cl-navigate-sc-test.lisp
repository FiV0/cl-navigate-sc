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
                #:add-variable-to-env-global
                #:dummy-source-reference
                #:find-binding
                #:file-location-start-line
                #:file-location-end-line
                #:file-location-start
                #:file-location-end
                #:missing-source-reference
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

(defvar program6 "(labels ((id (x) x)
                                (id2 (x) (id x)))
                         (id2 1))")

(define-test parse-labels
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program6))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-id-2 (nth 4 res))
         (sr-call-id-2 (nth 8 res))
         (sr-x-2 (nth 5 res))
         (sr-eval-x-2 (nth 7 res)))
    (is #'eq sr-id-2 (source-reference-parent sr-call-id-2))
    (is #'eq sr-x-2 (source-reference-parent sr-eval-x-2))))

;; tests that local variable function bindings don't get passed onward
(define-test parse-function-binding
  :depends-on (parse-cst-simple eclector-read)
  ;; cst is (id (x) x)
  (let* ((cst (cst:first (cst:first (cst:rest (read-one-cst program5))))))
    (multiple-value-bind (srefs env)
      (process-function-binding cst (empty-environment))
      (declare (ignore srefs))
      (true (handler-case (find-binding 'x env)
              (missing-source-reference (con) (declare (ignore con)) T)
              (condition (con) (declare (ignore con)) nil))))))

(defvar program-horrible
  "(let ((x 1))
     (flet ((x (x) x))
       (x x)))")

(define-test parse-let-flet
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program-horrible))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x-var (nth 1 res))
         (sr-eval-x-var (nth 7 res))
         (sr-x-fun (nth 3 res))
         (sr-x-fun-call (nth 6 res)))
    (is #'eq sr-x-var (source-reference-parent sr-eval-x-var))
    (is #'eq sr-x-fun (source-reference-parent sr-x-fun-call))))

(defvar program8 "(if 1
                      (let ((x 1)) x)
                      (let ((x 1)) x))")

(define-test parse-if
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program8))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x-1 (nth 2 res))
         (sr-eval-x-1 (nth 3 res))
         (sr-x-2 (nth 5 res))
         (sr-eval-x-2 (nth 6 res)))
    (is #'eq sr-x-1 (source-reference-parent sr-eval-x-1))
    (is #'eq sr-x-2 (source-reference-parent sr-eval-x-2))))

(defvar program9 "(progv '(*x* *y*) (1 2) (list *x* *y*))")

(define-test parse-progv
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program9))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-x-star (nth 1 res))
         (sr-x-star-eval (nth 4 res))
         (sr-y-star (nth 2 res))
         (sr-y-star-eval (nth 5 res)))
    (is #'eq sr-x-star (source-reference-parent sr-x-star-eval))
    (is #'eq sr-y-star (source-reference-parent sr-y-star-eval))))

(defvar program10 "'test-test")
(defvar program11 "(quote test-test)")

(define-test parse-quote
  :depends-on (parse-cst-simple eclector-read)
  (let* ((_ (intern "test-test" :cl-navigate-sc-test))
         (dummy-sr (dummy-source-reference 'test-test))
         (env (add-variable-to-env-global (empty-environment) dummy-sr))
         (cst1 (read-one-cst program10))
         (cst2 (read-one-cst program11))
         (res1 (parse-cst cst1 env))
         (res2 (parse-cst cst2 env))
         )
    (declare (ignore _))
    (is = 1 (length res1))
    (is = 2 (length res2))))

(defvar program12 "(block test-test (+ 1 2) (return-from test-test))")

(define-test parse-block
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program12))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-test (nth 1 res))
         (sr-test-quote (nth 4 res)))
    (is #'eq sr-test (source-reference-parent sr-test-quote))))


(defvar program13 "(eval-when (:compile-toplevel) (let ((x 1)) x))")

(define-test parse-eval-when
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst program13))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-x (nth 2 res))
         (sr-eval-x (nth 3 res)))
    (is #'eq sr-x (source-reference-parent sr-eval-x))))
