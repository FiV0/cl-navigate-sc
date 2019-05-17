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
                #:change-package
                #:cst-source-position
                #:add-variable-to-env-global
                #:dummy-source-reference
                #:find-binding
                #:file-location-start-line
                #:file-location-end-line
                #:file-location-start
                #:file-location-end
                #:missing-source-reference
                #:parse-program
                #:parse-macro-lambda-list
                #:parse-ordinary-lambda-list
                #:process-defun
                #:process-function-binding
                #:process-system
                #:read-program
                #:source-reference-parent
                #:symbol-information-symbol)
  (:export #:cl-navigate-sc-test))

(in-package #:cl-navigate-sc-test)

;; BIG TODO
;; remove all the relative parts and assumptions on repositories being in
;; certain places

;;Working with hunchentoot as an example. The request.lisp file below is from
;;hunchentoot.

;; file-location-read.lisp
(defvar *filepath* #P"./tmp/lisp-test-file.lisp")
(defvar *filepath1* #P"./tmp/request.lisp")

(define-test calculate-line-breaks
  (let ((result (with-open-file (is *filepath*)
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
  (let* ((res1 (file-position-to-location  4 '(23 45)))
         (line1 (car res1))
         (column1 (cdr res1)))
    (is = 0 line1)
    (is = 4 column1))
  (let* ((res2 (file-position-to-location 27 '(23 45)))
         (line2 (car res2))
         (column2 (cdr res2)))
    (is = 1 line2)
    (is = 3 column2))
  (let* ((res3 (file-position-to-location 46 '(1 45)))
         (line3 (car res3))
         (column3 (cdr res3)))
    (is = 2 line3)
    (is = 0 column3)))

(define-test create-file-position-to-file-location-function
  :depends-on (calculate-line-breaks file-position-to-location)
  (let* ((res (with-open-file (is *filepath*)
               (let ((fn (create-file-position-to-file-location-function is)))
                 (funcall fn 209))))
         (line (car res))
         (column (cdr res)))
    (is = 5 line)
    (is = 24 column)))

(defvar *program* "(1 #|comment|# ;;test
                       \"string\")")

(define-test eclector-read
  (true (with-input-from-string (is *program*)
          (eclector.parse-result:read
            (make-instance 'cst-source-position
                           :file-position-to-file-location
                           (create-file-position-to-file-location-function is))
            is))))

(define-test parse-from-file
  :depends-on (eclector-read)
 (true (parse-from-file *filepath1*)))

;; enviroment.lisp testing
(define-test set-get-environment
  (let* ((fl (make-instance 'source-location
                            :start-line 1
                            :end-line 2
                            :start 1
                            :end 2
                            :source-filepath nil))
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

(defvar *program2* "(list 1 2 3)")

(define-test parse-cst-simple
  (let* ((cst (read-one-cst *program2*))
         (res (parse-cst cst (empty-environment)))
         (list-ref (car res)))
    (is #'eq 'list (symbol-information-symbol list-ref))
    (is = 0 (file-location-start-line list-ref))
    (is = 1 (file-location-start list-ref))))

(defvar *program3* "(let ((x 1)
                          (y 2))
                      (print x)
                      (list x y))")

(define-test parse-let
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program3*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x (nth 1 res))
         (sr-print-x (nth 4 res))
         (sr-y (nth 2 res))
         (sr-list-y (nth 7 res)))
    (is #'eq sr-x (source-reference-parent sr-print-x))
    (is #'eq sr-y (source-reference-parent sr-list-y))))

(defvar *program3b* "
  (let (i
        (b 2))
    (list i b))")

(define-test parse-let-nil-binding
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program3b*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-i (nth 1 res))
         (sr-eval-i (nth 4 res))
         (sr-b (nth 2 res))
         (sr-eval-b (nth 5 res)))
    (is #'eq sr-i (source-reference-parent sr-eval-i))
    (is #'eq sr-b (source-reference-parent sr-eval-b))
    (is #'eq t (source-reference-globalp sr-eval-b))))

(defvar *program4* "(let* ((x 1)
                         (y x))
                         (print y))")

(define-test parse-let*
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program4*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-x (nth 1 res))
         (sr-form-x (nth 3 res))
         (sr-y (nth 2 res))
         (sr-print-y (nth 5 res)))
    (is #'eq sr-x (source-reference-parent sr-form-x))
    (is #'eq sr-y (source-reference-parent sr-print-y))))

(defvar *program5* "(flet ((id (x) x)
                           (id2 (x) x))
                      (id 1))")

(define-test parse-flet
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program5*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         ;; order of the source references
         (sr-id (nth 1 res))
         (sr-call-id (nth 7 res))
         (sr-x-2 (nth 5 res))
         (sr-eval-x-2 (nth 6 res)))
    (is #'eq sr-id (source-reference-parent sr-call-id))
    (is #'eq sr-x-2 (source-reference-parent sr-eval-x-2))))

(defvar *program6* "(labels ((id (x) x)
                             (id2 (x) (id x)))
                         (id2 1))")

(define-test parse-labels
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program6*))
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
  (let* ((cst (cst:first (cst:first (cst:rest (read-one-cst *program5*))))))
    (multiple-value-bind (srefs env)
      (process-function-binding cst (empty-environment))
      (declare (ignore srefs))
      (true (handler-case (find-binding 'x env)
              (missing-source-reference (con) (declare (ignore con)) T)
              (condition (con) (declare (ignore con)) nil))))))

(defvar *program-horrible*
  "(let ((x 1))
     (flet ((x (x) x))
       (x x)))")

(define-test parse-let-flet
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program-horrible*))
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

(defvar *program9* "(progv '(*x* *y*) (1 2) (list *x* *y*))")

(define-test parse-progv
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program9*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-x-star (nth 1 res))
         (sr-x-star-eval (nth 4 res))
         (sr-y-star (nth 2 res))
         (sr-y-star-eval (nth 5 res)))
    (is #'eq sr-x-star (source-reference-parent sr-x-star-eval))
    (is #'eq sr-y-star (source-reference-parent sr-y-star-eval))))

(defvar *program10* "'test-test")
(defvar *program11* "(quote test-test)")

(define-test parse-quote
  :depends-on (parse-cst-simple eclector-read)
  (let* ((_ (intern "test-test" :cl-navigate-sc-test))
         (dummy-sr (dummy-source-reference 'test-test))
         (env (add-variable-to-env-global (empty-environment) dummy-sr))
         (cst1 (read-one-cst *program10*))
         (cst2 (read-one-cst *program11*))
         (res1 (parse-cst cst1 env))
         (res2 (parse-cst cst2 env))
         )
    (declare (ignore _))
    (is = 1 (length res1))
    (is = 2 (length res2))))

(defvar *program12* "(block test-test (+ 1 2) (return-from test-test))")

(define-test parse-block
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program12*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-test (nth 1 res))
         (sr-test-quote (nth 4 res)))
    (is #'eq sr-test (source-reference-parent sr-test-quote))))


(defvar *program13* "(eval-when (:compile-toplevel) (let ((x 1)) x))")

(define-test parse-eval-when
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program13*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the
         (sr-x (nth 2 res))
         (sr-eval-x (nth 3 res)))
    (is #'eq sr-x (source-reference-parent sr-eval-x))))

(defvar *program14* "(tagbody
                       t1
                       (let ((x 1))
                         (go t1))
                       t2
                       (go t2))")

(define-test parse-tagbody
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program14*))
         (res (parse-cst cst (empty-environment)))
         ;; TODO this is a bad setup as it makes assumptions about the order of
         ;; processing
         (sr-t1 (nth 1 res))
         (sr-t2 (nth 2 res))
         (sr-eval-t1 (nth 6 res))
         (sr-eval-t2 (nth 8 res)))
    (is #'eq sr-t1 (source-reference-parent sr-eval-t1))
    (is #'eq sr-t2 (source-reference-parent sr-eval-t2))))

(defvar *program15* "(funcall (function +) 1 2)")

(define-test parse-function
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program15*))
         (res (parse-cst cst (empty-environment))))
    (declare (ignore res))
    (true T)))

(defvar *program16*
  "(defun id (x) x)
   (id 1)")

(defun read-program-from-string (str)
  (with-input-from-string (is str)
    (read-program is)))

(define-test parse-defun
  :depends-on (parse-cst-simple eclector-read)
  (let* ((csts (read-program-from-string *program16*))
         (res (parse-program csts))
         (sr-id (nth 1 res))
         (sr-id-eval (nth 4 res)))
    (is #'eq sr-id (source-reference-parent sr-id-eval))))


(defvar *program17*
  "(defun test (a &rest rest)
     (list a rest))")

(define-test parse-ordinary-lambda-list
  (let* ((cst (read-one-cst *program17*))
         (lambda-list (cst:third cst))
         (res (parse-ordinary-lambda-list lambda-list))
         (cst-a (nth 0 res))
         (cst-rest (nth 1 res)))
    (is = 2 (length res))
    (is #'string= "A" (symbol-name (cst:raw cst-a)))
    (is #'string= "REST" (symbol-name (cst:raw cst-rest)))))

(define-test parse-defun2
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program17*))
         (res (parse-cst cst (empty-environment)))
         (sr-test (nth 1 res))
         (sr-a (nth 2 res))
         (sr-a-eval (nth 5 res))
         (sr-rest (nth 3 res))
         (sr-rest-eval (nth 6 res)))
    (true (source-reference-globalp sr-test))
    (false (source-reference-globalp sr-a))
    (is #'eq sr-a (source-reference-parent sr-a-eval))
    (is #'eq sr-rest (source-reference-parent sr-rest-eval))))

(defvar *program18*
  "(defun test (a &optional (b 1) &key c)
     (list a b c))")

(define-test parse-defun3
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program18*))
         (res (parse-cst cst (empty-environment)))
         (sr-a (nth 2 res))
         (sr-b (nth 3 res))
         (sr-c (nth 4 res))
         (sr-a-eval (nth 6 res))
         (sr-b-eval (nth 7 res))
         (sr-c-eval (nth 8 res)))
    (is #'eq sr-a (source-reference-parent sr-a-eval))
    (is #'eq sr-b (source-reference-parent sr-b-eval))
    (is #'eq sr-c (source-reference-parent sr-c-eval))))

(defvar *program19*
  "(defmacro loser ((a b) &body body)
     `(list ,a ,b ,@body))")

(define-test parse-macro-lambda-list
  (let* ((cst (read-one-cst *program19*))
         (lambda-list (cst:third cst))
         (res (parse-macro-lambda-list lambda-list))
         (cst-a (nth 0 res))
         (cst-b (nth 1 res))
         (cst-body (nth 2 res)))
    (is = 3 (length res))
    (is #'string= "A" (cst:raw cst-a))
    (is #'string= "B" (cst:raw cst-b))
    (is #'string= "BODY" (cst:raw cst-body))))

(define-test parse-macro
  (let* ((cst (read-one-cst *program19*))
         (res (parse-cst cst (empty-environment)))
         (sr-a (nth 2 res))
         (sr-a-eval (nth 5 res))
         (sr-b (nth 3 res))
         (sr-b-eval (nth 6 res))
         (sr-body (nth 4 res))
         (sr-body-eval (nth 7 res)))
    (is = 8 (length res))
    (is #'eq sr-a (source-reference-parent sr-a-eval))
    (is #'eq sr-b (source-reference-parent sr-b-eval))
    (is #'eq sr-body (source-reference-parent sr-body-eval))))

(defvar *program19b*
  "(defmacro loser ((a b) &body body)
     `(list ,a ,b ,@body))
   (loser (1 2) (3 4))")

(define-test parse-macro
  (let* ((cst (read-one-cst *program19*))
         (res (parse-cst cst (empty-environment)))
         (sr-a (nth 2 res))
         (sr-a-eval (nth 5 res))
         (sr-b (nth 3 res))
         (sr-b-eval (nth 6 res))
         (sr-body (nth 4 res))
         (sr-body-eval (nth 7 res)))
    (is = 8 (length res))
    (is #'eq sr-a (source-reference-parent sr-a-eval))
    (is #'eq sr-b (source-reference-parent sr-b-eval))
    (is #'eq sr-body (source-reference-parent sr-body-eval))))

(defvar *program19c*
  "(let ((i 1)
         (j 2))
     (case 1
           ((2) (print i))
           (1 (print j))))")

(define-test parse-case
  (let* ((cst (read-one-cst *program19c*))
         (res (parse-cst cst (empty-environment)))
         (sr-i (nth 1 res))
         (sr-i-eval (nth 5 res))
         (sr-j (nth 2 res))
         (sr-j-eval (nth 7 res)))
    (is = 8 (length res))
    (is #'eq sr-i (source-reference-parent sr-i-eval))
    (is #'eq sr-j (source-reference-parent sr-j-eval))))

(defvar *program19d*
  "(let ((i 1)
         (j 2))
     (typecase i
       (int (print i))
       (t (print j))))")

(define-test parse-typecase
  (let* ((cst (read-one-cst *program19d*))
         (res (parse-cst cst (empty-environment)))
         (sr-i (nth 1 res))
         (sr-i-eval (nth 6 res))
         (sr-j (nth 2 res))
         (sr-j-eval (nth 8 res)))
    (is = 9 (length res))
    (is #'eq sr-i (source-reference-parent sr-i-eval))
    (is #'eq sr-j (source-reference-parent sr-j-eval))))

(defvar *program20*
  "(in-package :hunchentoot)
   (defclass test () ())")

(define-test test-in-package-defclass
  :depends-on (parse-cst-simple eclector-read)
  (let* ((csts (read-program-from-string *program20*))
         (res (parse-program csts)))
    ;;TODO change once implemented
    (false res)))

(defvar *program21* "
  (defmacro with-lock-held ((lock) &body body)
    `(bt:with-lock-held (,lock) ,@body))")

(define-test test-defmacro-with-unquote-inside-list
  :depends-on (parse-cst-simple eclector-read)
  (let* ((csts (read-program-from-string *program21*))
         (res (parse-program csts)))
    (true res)))

(define-test change-package
  (let* ((test-string ":hunchentoot)")
         (client (make-instance 'cl-navigate-sc::cst-source-position
                                :file-position-to-file-location nil
                                :current-package *package*))
         (res-client (with-input-from-string (is test-string)
                       (change-package client is))))
    (is #'eq
        (find-package :hunchentoot)
        (cl-navigate-sc::cst-source-position-current-package res-client))))

(defvar *program22* "(:NAME . \"JSONRPCError\")")

(define-test quasiquoted-pair-string
  :depends-on (parse-cst-simple eclector-read)
  (let* ((cst (read-one-cst *program22*)))
    (parse-cst cst (empty-environment) T)))

(defvar *program23* "
(let ((i 2)
      (j 2))
  (cond ((eq i j) (+ i i))
        (t (- j j))))")

(define-test parse-cond
  (let* ((cst (read-one-cst *program23*))
         (res (parse-cst cst (empty-environment)))
         (sr-i (nth 1 res))
         (sr-i-eval (nth 8 res))
         (sr-j (nth 2 res))
         (sr-j-eval (nth 11 res)))
    (is = 13 (length res))
    (is #'eq sr-i (source-reference-parent sr-i-eval))
    (is #'eq sr-j (source-reference-parent sr-j-eval))))

(defvar *program24* "
(defparameter *toto* (list 2 3))
(mapcar #'print *toto*)")

(define-test parse-cond
  (let* ((csts (read-program-from-string *program24*))
         (res (parse-csts csts (empty-environment)))
         (toto (nth 1 res))
         (toto-eval (nth 5 res)))
    (is = 6 (length res))
    (is #'eq toto (source-reference-parent toto-eval))))

(defpackage my-package)
(defvar *program25* "
 (defpackage my-package
   (:nicknames mypkg :MY-PKG)
   (:use common-lisp)
   (:shadow CAR :cdr #:cons)
   (:export \"CONS\"))
 (in-package my-package)")

(define-test parse-defpackage
  (let* ((csts (read-program-from-string *program25*))
         (res (parse-csts csts (empty-environment) T))
         (my-package (nth 1 res))
         (my-package-ref (nth 3 res)))
    (is = 4 (length res))
    (is #'eq my-package (source-reference-parent my-package-ref))))

(test 'parse-defpackage)

(defparameter *filepath2*
  "/home/fv/Code/CL/hunchentoot/session.lisp")

;; TODO really bad test as this even on the code location in hunchentoot
(define-test parse-whole-file
  (let* ((csts (parse-from-file *filepath2*))
         (cst1 (nth 14 csts))
         (with-session-lock-held-symbol (cst:raw (cst:first (cst:fifth cst1))))
         (cst2 (nth 16 csts))
         (only-once-symbol (cst:raw (cst:first (cst:sixth cst2))))
         (res (parse-program csts)))
    (is #'eq with-session-lock-held-symbol 'hunchentoot::with-session-lock-held)
    (is #'eq
        (symbol-package with-session-lock-held-symbol)
        (find-package :hunchentoot))
    (is #'eq only-once-symbol 'alexandria:once-only)
    (is #'eq
        (symbol-package only-once-symbol)
        (find-package :alexandria))
    (true res)))

(defparameter *filepath3*
  "./test/utf8-test.lisp")

(define-test utf-8-parse-test
  (let* ((cst (car (parse-from-file *filepath3*)))
         (file-location (cst:source cst)))
    (is = 1 (file-location-start-line file-location))
    (is = 1 (file-location-end-line file-location))
    (is = 0 (file-location-start file-location))
    (is = 1 (file-location-end file-location))))

;; tests for process project
(define-test test-process-system
  (let* ((res (process-system :hunchentoot #P"/home/fv/Code/CL/hunchentoot/"))
         (src-refs (alexandria:flatten
                     (mapcar #'wrapper-source-references res)))
         (res T))
    (dolist (src-ref src-refs)
      (setf res (and res (symbolp (symbol-information-symbol src-ref)))))
    (true res)))
