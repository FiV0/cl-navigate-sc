#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

#|
  Every function in this file works on a cst.
  The source-reference are currently returned in the order they are encountered.
  Explain the file....
|#

(in-package #:cl-navigate-sc)

;; use it to check the standard
(ql:quickload :alexandria)

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

(defun parse-atom (cst env)
  "Creates sources-references + env for an atom CST."
  (let* ((item (cst:raw cst)))
    (if (eq (type-of item) 'symbol)
        (let
          ((symbol-information (make-symbol-information item))
           ;; TODO figure out how to best handle the case other package
           ;; for now just check if its a standard symbol
           (parent (when (not (standard-symbol-p cst))
                     (find-binding item env)
                     ;(if (fboundp item)
                         ;(error 'should-not-happen);(find-function item env)
                         ;(find-variable item env))
                     ))
           (file-location (cst:source cst)))
          (if file-location
           (values
             (list (make-source-reference symbol-information
                                          file-location parent))
             env)
           (values '() env)))
        (values '() env))))

(defparameter +stop-parse-symbols+ '(eclector.reader:quasiquote))
(defun stop-parse (cst)
  "Returns true if the cst is of a raw symbol to stop parsing."
  (and (atom-cst-p cst) (member (cst:raw cst) +stop-parse-symbols+)))

(defun special-cst-p (cst)
  "Returns True if the cst is special symbol."
  (special-operator-p (cst:raw cst)))

#|
 List of special forms:
   block       1  let*                   1  return-from            1
   catch       1  load-time-value           setq
   eval-when   1  locally                   symbol-macrolet
   flet        1  macrolet                  tagbody
   function       multiple-value-call       the
   go             multiple-value-prog1      throw                  1
   if          1  progn                  1  unwind-protect
   labels      1  progv                  1
   let         1  quote
|#

(defparameter +special-like-function+ '(if progn return-from throw go))

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
           (process-eval-when cst env))
          ((member (cst:raw first) +special-like-function+)
           ;; special/global needs recursive here
           (process-function-call cst env))
          (T (error 'not-yet-implemented)))))

(defun standard-symbol-p (cst)
  "Returns True if the symbol is in the Standard."
  (not (null (hyperspec:lookup (cst:raw cst)))))

(defun cst-to-list (cst)
  "Transforms a cst into a list of csts of its subexpressions."
  (if (cst:null cst)
      '()
      (cons (cst:first cst) (cst-to-list (cst:rest cst)))))

(defun process-function-call (cst env)
  "Process a function call cst. Some special operators are also processed using
   this function as they are no different in terms of source referencing."
  (let* ((first (cst:first cst))
        (function (cst:raw first))
        (symbol-information (make-symbol-information function))
        ;; TODO check how to test for function from external package
        (parent (when (not (standard-symbol-p first))
                  (find-function function env)))
        (file-location (cst:source first))
        (sr (make-source-reference symbol-information file-location parent)))
    (multiple-value-bind (srefs new-env)
      (parse-csts (cst-to-list (cst:rest cst)) env)
      (declare (ignore new-env))
      (values (cons sr srefs) env))))

(defun process-other-cst (cst env)
  "Process a CST that is not special"
  (let ((first (cst:first cst)))
    (cond ((not (macro-function (cst:raw first)))
           (process-function-call cst env))
          (T (error 'not-yet-implemented)))))

(defun parse-cons (cst env)
  "Parse a cons-cst."
  (let ((first (cst:first cst)))
    (cond ((stop-parse first) (values '() env))
          ((special-cst-p first) (process-special-cst cst env))
          (T (process-other-cst cst env)))))

(defun parse-cst (cst env)
  "Parse a cst of any form."
  (if (atom-cst-p cst)
      (parse-atom cst env)
      (parse-cons cst env)))

(defun parse-csts (csts env &optional (recursive nil))
  "Parse a list of expressions (csts)."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (parse-cst cst (cdr refs-env))
             (cons (append (car refs-env) refs) (if recursive new-env env)))))
    (let  ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))


(defun add-symbol-to-env (cst env &optional
                              (add-env-function #'add-variable-to-env))
  "Add a symbol to env and creates a source-reference."
  (let* ((symbol (cst:raw cst))
         (symbol-information (make-symbol-information symbol))
         (file-location (cst:source cst))
         (sr (make-source-reference symbol-information file-location)))
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
          (error 'not-yet-implemented)))))

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
  (let ((binding-cst (cst:first cst))
        (form-cst (cst:first (cst:rest cst))))
    ;; TODO add global special stuff from form
    (multiple-value-bind (sref new-env1)
      (add-symbol-to-env binding-cst env add-env-function)
      (multiple-value-bind (srefs new-env2)
        (parse-cst form-cst (copy-environment env))
        (declare (ignore new-env2))
        (values (append sref srefs) new-env1)))))

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
