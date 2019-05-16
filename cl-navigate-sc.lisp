#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

;; Every function in this file works on a cst.
;; The source-reference are currently returned in the order they are
;; encountered.
;; Explain the file....


(in-package #:cl-navigate-sc)

(define-condition not-yet-implemented ()
  ())

(define-condition should-not-happen ()
  ())

(defun resti (cst i)
  "Recursive application of cst:rest."
  (if (= i 0)
      cst
      (resti (cst:rest cst) (1- i))))

(defun atom-cst-p (cst)
  "Checks if the cst is of type atom."
  (eq (type-of cst) 'cst:atom-cst))

(defun cons-cst-p (cst)
  "Checks if the cst is of type cons."
  (eq (type-of cst) 'cst:cons-cst))

(defun find-source-reference (symbol env &optional (find-fn #'find-binding))
  "Find the source-reference of parent if present."
  (handler-case (funcall find-fn symbol env)
    (missing-source-reference (con)
      (declare (ignore con))
      nil)
    (condition () (error 'should-not-happen))))

(defun parse-atom (cst env)
  "Creates sources-references + env for an atom CST."
  (let* ((item (cst:raw cst)))
    (if (eq (type-of item) 'symbol)
        (mvlet
          ((symbol-information (make-symbol-information item))
           ;; TODO figure out how to best handle the case other package
           ;; for now just check if its a standard symbol
           ((parent globalp) (find-source-reference item env))
           (file-location (cst:source cst)))
          (if file-location
              (values
                (list (make-source-reference symbol-information
                                             file-location parent globalp))
                env)
              (values '() env)))
        (values '() env))))

(defun special-cst-p (cst)
  "Returns True if the cst is special symbol."
  (special-operator-p (cst:raw cst)))

#|
 List of special forms:
   block       1  let*                   1  return-from            1
   catch       1  load-time-value        1  setq                   1
   eval-when   1  locally                1  symbol-macrolet
   flet        1  macrolet                  tagbody                1
   function    1  multiple-value-call    1  the                    1
   go          1  multiple-value-prog1   1  throw                  1
   if          1  progn                  1  unwind-protect         1
   labels      1  progv                  1
   let         1  quote
|#

(defparameter +special-like-function+
  (list 'if 'function 'go 'load-time-value 'locally 'multiple-value-call
        'multiple-value-prog1 'mulitple-value-prog1 'progn 'return-from 'setq
        'the 'throw 'unwind-protect))

(defun process-special-cst (cst env)
  "Process a special symbol operator."
  (let ((first (cst:first cst)))
    (cond ((or (eq (cst:raw first) 'let) (eq (cst:raw first) 'let*))
           (process-let-bindings cst env))
          ((or (eq (cst:raw first) 'flet) (eq (cst:raw first) 'labels))
           (process-local-function-bindings cst env))
          ((eq (cst:raw first) 'progv)
           (process-progv cst env))
          ((eq (cst:raw first) 'quote)
           (process-quote cst env))
          ((or (eq (cst:raw first) 'block) (eq (cst:raw first) 'catch))
           (process-block cst env))
          ((eq (cst:raw first) 'eval-when)
           (process-eval-when cst env))
          ((eq (cst:raw first) 'tagbody)
           (process-tagbody cst env))
          ((member (cst:raw first) +special-like-function+)
           ;; special/global needs recursive here
           (process-function-call cst env))
          ((member (cst:raw first) '(macrolet symbol-macrolet))
           (progn
             (format *standard-output* "WARNING: ~a is not yet implemented!~%"
                     (cst:raw first))
             (values '() env)))
          (T (error 'not-yet-implemented)))))

(defun standard-symbol-p (cst)
  "Returns True if the symbol is in the Standard."
  (not (null (hyperspec:lookup (cst:raw cst)))))

(defun cst-to-list (cst)
  "Transforms a cst into a list of csts of its subexpressions."
  (cond
    ((cst:null cst)
     '())
    ((cst:consp cst)
     (cons (cst:first cst) (cst-to-list (cst:rest cst))))
    (T (list cst))))

(defun process-function-call (cst env)
  "Process a function call cst. Some special operators are also processed using
   this function as they are no different in terms of source referencing."
  (mvlet* ((first (cst:first cst))
           (function (cst:raw first))
           (symbol-information (make-symbol-information function))
           ;; TODO check how to test for function from external package
           ((parent globalp) (find-source-reference function env #'find-function))
           (file-location (cst:source first))
           (sr (when file-location
                 (list (make-source-reference symbol-information file-location
                                              parent globalp)))))
    (multiple-value-bind (srefs new-env)
      (parse-csts (cst-to-list (cst:rest cst)) env)
      (declare (ignore new-env))
      (values (append sr srefs) env))))

#| Macro symbols in :cl

AND ASSERT CALL-METHOD CASE CCASE CHECK-TYPE COND CTYPECASE DECF DECLAIM
DEFCLASS DEFCONSTANT DEFGENERIC DEFINE-COMPILER-MACRO DEFINE-CONDITION
DEFINE-METHOD-COMBINATION DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER
DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD DEFPACKAGE DEFPARAMETER DEFSETF
DEFSTRUCT DEFTYPE DEFUN DEFVAR DESTRUCTURING-BIND DO DO* DO-ALL-SYMBOLS
DO-EXTERNAL-SYMBOLS DO-SYMBOLS DOLIST DOTIMES ECASE ETYPECASE FORMATTER
HANDLER-BIND HANDLER-CASE IGNORE-ERRORS IN-PACKAGE INCF LAMBDA LOOP
LOOP-FINISH MULTIPLE-VALUE-BIND MULTIPLE-VALUE-LIST MULTIPLE-VALUE-SETQ
NTH-VALUE OR POP PPRINT-EXIT-IF-LIST-EXHAUSTED PPRINT-LOGICAL-BLOCK
PPRINT-POP PRINT-UNREADABLE-OBJECT PROG PROG* PROG1 PROG2 PSETF PSETQ PUSH
PUSHNEW REMF RESTART-BIND RESTART-CASE RETURN ROTATEF SETF SHIFTF STEP
TIME TRACE TYPECASE UNLESS UNTRACE WHEN WITH-ACCESSORS
WITH-COMPILATION-UNIT WITH-CONDITION-RESTARTS WITH-HASH-TABLE-ITERATOR
WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM
WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR WITH-SIMPLE-RESTART WITH-SLOTS
WITH-STANDARD-IO-SYNTAX

|#

;;TODO
(defparameter *macros-process-like-function*
  '(and assert or check-type decf incf declaim))

(defparameter *clos-symbols*
  '(call-method defclass defgeneric define-method-combination defmethod ))

(defparameter *global-var-specifiers*
  '(defconstant defparameter defvar))

(defun process-other-cst (cst env)
  "Process a CST that is not special"
  (let ((raw (cst:raw (cst:first cst) )))
    (cond ((not (macro-function raw))
           (process-function-call cst env))
          ;;TODO
          ((member raw *clos-symbols*)
           (progn
             (format *standard-output* "WARNING: CLOS symbols not yet implemented")
             (values '() env)))
          ((eq raw 'defun)
           (process-defun cst env))
          ((eq raw 'defmacro)
           (process-defmacro cst env))
          ((member raw (list 'case 'ccase 'ecase 'typecase 'ctypecase
                             'etypecase))
           (process-case cst env))
          ((member raw *global-var-specifiers*)
           (process-global-vars cst env))
          ((eq raw 'cond)
           (process-cond cst env))
          ;;TODO
          (T (progn
               (format *standard-output* "WARNING: ~a is not yet implemented!~%"
                       raw)
               (values '() env))))))

(defun parse-cons (cst env)
  "Parse a cons-cst."
  (let ((first (cst:first cst)))
    (cond ((stop-parse first) (parse-cst (cst:second cst) env T))
          ((special-cst-p first) (process-special-cst cst env))
          (T (process-other-cst cst env)))))


(defun parse-cst (cst env &optional (quasiquoted nil))
  "Parse a cst of any form."
  (if quasiquoted
      (cond ((atom-cst-p cst) (values '() env))
            ;;TODO check if this is needed
            ((cst:null cst) (values '() env))
            ((cst:null (cst:rest cst)) (parse-cst (cst:first cst) env T))
            (T (let ((first (cst:first cst))
                     (rest (cst:rest cst)))
                 ;; checking if to turn off quasiquotation
                 (if (member (cst:raw first) +start-parse-symbols+)
                     ;;TODO check should be only one cst
                     (parse-cst (cst:first rest) env)
                     (parse-csts (cst-to-list cst) env T T)))))
      (if (atom-cst-p cst)
          (parse-atom cst env)
          (parse-cons cst env))))

(defun parse-csts (csts env &optional (recursive nil) (quasiquoted nil))
  "Parse a list of expressions (csts)."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (parse-cst cst (cdr refs-env) quasiquoted)
             (cons (append (car refs-env) refs) (if recursive new-env env)))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

(defun parse-program (csts &optional (env (empty-environment)))
  "Process a program in the form of toplevel csts. Essentially just a alias for
   (parse-csts csts env T) == (parse-program csts env)"
  (parse-csts csts env T))

(defparameter +stop-parse-symbols+ '(eclector.reader:quasiquote))
(defparameter +start-parse-symbols+ '(eclector.reader:unquote
                                      eclector.reader:unquote-splicing))

(defun stop-parse (cst)
  "Returns true if the cst is of a raw symbol to stop parsing."
  (and (atom-cst-p cst) (member (cst:raw cst) +stop-parse-symbols+)))

(defun add-symbol-to-env (cst env &optional
                              (add-env-function #'add-variable-to-env))
  "Add a symbol to env and creates a source-reference."
  (let* ((symbol (cst:raw cst))
         (symbol-information (make-symbol-information symbol))
         (file-location (cst:source cst))
         (sr (make-source-reference
               symbol-information
               file-location
               nil
               (not (member add-env-function
                            (list #'add-variable-to-env
                                  #'add-function-to-env))))))
    (values (list sr) (funcall add-env-function env sr))))

(defun add-symbols-to-env (csts env &optional
                                (add-env-function #'add-variable-to-env))
  "Add a list of symbols to env. Typically used with function lambda lists."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (add-symbol-to-env cst (cdr refs-env) add-env-function)
             (cons (append (car refs-env) refs) new-env))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

;; defmacro
(defun process-standard-def (cst env &optional (parse-lambda-list-fn
                                                 #'parse-ordinary-lambda-list))
  "Process a defmacro or defun definition."
  (let ((def (cst:first cst))
        (name (cst:second cst))
        (lambda-list (funcall parse-lambda-list-fn (cst:third cst)))
        (body (resti cst 3)))
    (let ((srefs1 (parse-atom def env)))
      (multiple-value-bind (srefs2 env2)
        (add-symbol-to-env name env #'add-function-to-env-global)
        (multiple-value-bind (srefs3 env3)
          (add-symbols-to-env lambda-list (copy-environment env2))
          (multiple-value-bind (srefs4 env4)
            (parse-csts (cst-to-list body) (join-environments env3 env2))
            (declare (ignore env4))
            (values (append srefs1 srefs2 srefs3 srefs4)
                    env2)))))))

(defun process-defmacro (cst env)
  "Process a defmacro declaration."
  (process-standard-def cst env #'parse-macro-lambda-list))

(defun process-global-vars (cst env)
  "Process a defconstant, defparameter or defvar."
  (mvlet* ((def (cst:first cst))
           (name (cst:second cst))
           (value (cst:third cst))
           (srefs1 (parse-atom def env))
           ((srefs2 env2) (add-symbol-to-env name env
                                            #'add-variable-to-env-global))
           (srefs3 (parse-cst value (copy-environment env))))
    (values (append srefs1 srefs2 srefs3) env2)))

;; defun
;TODO add optional form eval
(defun process-defun (cst env)
  "Process a function declaration."
  (if (consp (cst:raw (cst:second cst)))
      (progn
       (format *standard-output*
               "WARNING: defun is not yet implemented with setf!~%")
       (values '() env))
      (process-standard-def cst env)))

(defun split-clauses (clauses)
  "A list of the form ((keys forms*)*) gets split into a list of keys and a list
   of forms*."
  (let ((res (reduce #'(lambda (res clause)
                          (list (cons (cst:first clause) (car res))
                                (cons (cst:rest clause) (cadr res))))
                     (cst-to-list clauses) :initial-value (list '() '()))))
    (values (nreverse (car res)) (nreverse (cadr res)))))

;; case
(defun process-case (cst env)
  "Process a case declaration"
  (let ((case (cst:first cst))
        (keyform (cst:second cst))
        (clauses (resti cst 2)))
    (mvlet* ((srefs1 (parse-atom case env))
             (srefs2 (parse-cst keyform env))
             ((keys forms*) (split-clauses clauses))
             (srefs3 (alexandria:flatten
                       (mapcar #'(lambda (forms)
                                   (parse-csts (cst-to-list forms)
                                               (copy-environment env) T))
                               forms*))))
      (values (append srefs1 srefs2 srefs3) env))))

;; cond
(defun process-cond (cst env)
  "Process a cond declaration."
  (mvlet* ((cond (cst:first cst))
           (clauses (cst:rest cst))
           (srefs1 (parse-atom cond env))
           ((conds forms*) (split-clauses clauses))
           (srefs2-lists (mapcar #'(lambda (cond)
                                     (parse-cst cond (copy-environment env)))
                                 conds))
           (srefs3-lists (mapcar #'(lambda (forms)
                                     (parse-csts (cst-to-list forms)
                                                 (copy-environment env) T))
                                 forms*))
           (srefs2-3 (alexandria:flatten
                       (mapcar #'(lambda (sref2 sref3)
                                   (append sref2 sref3))
                               srefs2-lists
                               srefs3-lists))))
    (values (append srefs1 srefs2-3) env)))

;; quote
(defun process-quote (cst env)
  "Process a quote cst."
  (let ((quote (cst:first cst))
        (val (cst:second cst)))
    (assert (eq (cst:raw quote) 'quote))
    (multiple-value-bind (srefs1 env1)
      (parse-atom quote env)
      (declare (ignore env1))
      (if (eq (type-of val) 'cst:atom-cst)
          (multiple-value-bind (srefs2 env2)
            (parse-atom val env)
            (declare (ignore env2))
            (values (append srefs1 srefs2) env))
          ;;TODO
          (values '() env)))))

;; eval-when

(defun process-eval-when (cst env)
  "Process an eval-when. Ignoring the cases."
  (let ((eval-when (cst:first cst))
        (forms (resti cst 2)))
    (multiple-value-bind (srefs1 env1)
      (parse-atom eval-when env)
      (declare (ignore env1))
      (multiple-value-bind (srefs2 env2)
        (parse-csts (cst-to-list forms) env)
        (declare (ignore env2))
        (values (append srefs1 srefs2) env)))))

;; tagbody

(defun extract-tags-and-forms (cst)
  "Helper function for process-tagbody. Splits the tagbody forms into tags and
   actual forms to be evaluated."
  (flet ((helper (tags-and-forms cst)
           (if (atom-cst-p cst)
               (cons (cons cst (car tags-and-forms)) (cdr tags-and-forms))
               (cons (car tags-and-forms) (cons cst (cdr tags-and-forms))))))
    (let ((res (reduce #'helper (cst-to-list cst)
                       :initial-value (cons '() '()))))
      (values (nreverse (car res)) (nreverse (cdr res))))))

(defun process-tagbody (cst env)
  "Process a tagbody."
  (let* ((tagbody (cst:first cst))
         (tags-and-forms (cst:rest cst)))
    (multiple-value-bind (srefs1 env1)
      (parse-atom tagbody env)
      (declare (ignore env1))
      (multiple-value-bind (tags forms)
        (extract-tags-and-forms tags-and-forms)
        (multiple-value-bind (srefs2 env2)
          (add-symbols-to-env tags (copy-environment env))
          (multiple-value-bind (srefs3 env3)
            (parse-csts forms env2)
            (declare (ignore env3))
            (values (append srefs1 srefs2 srefs3) env)))))))

;; block

(defun process-block (cst env)
  "Process a block/catch cst. Catch has different semantics then block but in
   in terms of source references it should be the same thing."
  (let ((block (cst:first cst))
        (symbol-declaration (cst:second cst))
        (forms (resti cst 2)))
    (multiple-value-bind (srefs1 env1)
      (parse-atom block env)
      (declare (ignore env1))
      (multiple-value-bind (srefs2 env2)
        (add-symbol-to-env symbol-declaration (copy-environment env))
        (multiple-value-bind (srefs3 env3)
          (parse-csts (cst-to-list forms) env2)
          (declare (ignore env3))
          (values (append srefs1 srefs2 srefs3) env))))))

;;; progv

(defun process-progv (cst env)
  "Process a progv cst."
  (let* ((progv (cst:first cst))
         (bindings (cst:second cst))
         (bindings-without-quote (cst:second bindings))
         (values (cst:third cst))
         (forms (cst:fourth cst)))
    (assert (eq (cst:raw (cst:first bindings)) 'quote))
    (multiple-value-bind (srefs1 env1)
      (parse-atom progv env)
      (declare (ignore env1))
      (multiple-value-bind (srefs2 env2)
        ;; don't actually need to be added to global env
        (add-symbols-to-env (cst-to-list bindings-without-quote)
                            (copy-environment env))
        (multiple-value-bind (srefs3 env3)
          (parse-csts (cst-to-list values) (copy-environment env))
          (declare (ignore env3))
          (multiple-value-bind (srefs4 env4)
            (parse-csts (cst-to-list forms) env2)
            (declare (ignore env4))
            (values (append srefs1 srefs2 srefs3 srefs4) env)))))))

;;; local function bindings

(defun process-lambda (cst env)
  "Process a cst of the form ((lambda-list body)"
  (with-env-copy (copy-env env)
   (let ((lambda-list (cst:first cst))
         (body (cst:rest cst)))
     (multiple-value-bind (srefs1 new-env1)
       (add-symbols-to-env (cst-to-list lambda-list) env)
       (multiple-value-bind (srefs2 new-env2)
         (parse-csts (cst-to-list body) new-env1)
         (declare (ignore new-env2))
         ;; TODO take care of special symbols
         (values (append srefs1 srefs2) copy-env))))))

(defun process-function-binding (cst env &optional (recursive nil))
  "Process one binding of the form (name (lambda-list) body)."
  (with-env-copy (copy-env env)
    (let ((name (cst:first cst))
          (lambda (cst:rest cst)))
      (multiple-value-bind (srefs1 new-env1)
        (add-symbol-to-env name env #'add-function-to-env)
        (with-env-copy (res-env new-env1)
          (multiple-value-bind (srefs2 new-env2)
            (process-lambda lambda (if recursive new-env1 copy-env))
            (declare (ignore new-env2))
            (values (append srefs1 srefs2) res-env)))))))

(defun process-function-bindings (csts env &optional (recursive nil))
  "Process a list of function bindings."
  ;(declare (ignore recursive))
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (process-function-binding cst (cdr refs-env) recursive)
             (cons (append (car refs-env) refs)
                   (if recursive new-env (copy-environment env))))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

(defun process-local-function-bindings (cst env)
  "Process a flet/labels."
  (let* ((type-form (cst:first cst))
         (recursive (eq (cst:raw type-form) 'labels))
         (bindings (cst-to-list (cst:second cst)))
         (forms (cst-to-list (resti cst 2))))
    (multiple-value-bind (refs1 env1)
      (parse-atom type-form (copy-environment env))
      (declare (ignore env1))
      (multiple-value-bind (refs2 env2)
        (process-function-bindings bindings (copy-environment env) recursive)
        ;; TODO add global special
        (multiple-value-bind (refs3 env3) (parse-csts forms env2)
          (declare (ignore env3))
          (values (append refs1 refs2 refs3) env))))))

;;; let bindings

(define-condition binding-error ()
  ())

(defun process-binding (cst env &optional
                            (add-env-function #'add-variable-to-env))
  "Process one binding of the form (binding form)."
  ;; Case where the binidng has no form.
  (if (atom-cst-p cst)
      (add-symbol-to-env cst env add-env-function)
      (let ((binding-cst (cst:first cst))
            (form-cst (cst:first (cst:rest cst))))
        ;; TODO add global special stuff from form
        (multiple-value-bind (sref new-env1)
          (add-symbol-to-env binding-cst env add-env-function)
          (multiple-value-bind (srefs new-env2)
            (parse-cst form-cst (copy-environment env))
            (declare (ignore new-env2))
            (values (append sref srefs) new-env1))))))

(defun process-bindings (csts env &optional (recursive nil))
  "Process a list of binding csts."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (process-binding cst (if recursive
                                      (cdr refs-env)
                                      (copy-environment env)))
             (cons (append (car refs-env) refs)
                   (join-environments (cdr refs-env) new-env)))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

(defun process-let-bindings (cst env)
  "Process a let/let*."
  (let* ((let-cst (cst:first cst))
         (recursive (eq (cst:raw let-cst) 'let*))
         (bindings (cst-to-list (cst:second cst)))
         (forms (cst-to-list (resti cst 2))))
    (multiple-value-bind (refs1 env1)
      (parse-atom let-cst (copy-environment env))
      (declare (ignore env1))
      (multiple-value-bind (refs2 env2)
        (process-bindings bindings (copy-environment env) recursive)
        ;; TODO add global special
        (multiple-value-bind (refs3 env3) (parse-csts forms env2)
          (declare (ignore env3))
          (values (append refs1 refs2 refs3) env))))))
