#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

#|
  Explain the file....
|#

(in-package #:cl-navigate-sc)

;; use it to check the standard
(ql:quickload :alexandria)

(define-condition not-yet-implemented ()
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
                     (if (fboundp item)
                         (find-function item env)
                         (find-variable item env))))
           (file-location (cst:source cst)))
          (values
            (list (make-source-reference symbol-information
                                         file-location parent))
            env))
        (values '() env))))

(defparameter +stop-parse-symbols+ '(quote eclector.reader:quasiquote))
(defun stop-parse (cst)
  "Returns true if the cst is of a raw symbol to stop parsing."
  (and (atom-cst-p cst) (member (cst:raw cst) +stop-parse-symbols+)))

(defun special-cst-p (cst)
  "Returns True if the cst is special symbol."
  (special-operator-p (cst:raw cst)))

(defun process-special-cst (cst env)
  "Process a special symbol operator."
  (let ((first (cst:first cst)))
    (cond ((or (eq (cst:raw first) 'let) (eq (cst:raw first) 'let*))
           (process-let-bindings cst env))
          (T (error 'not-yet-implemented)))))

(defun standard-symbol-p (cst)
  "Returns True if the symbol is in the Standard."
  (not (null (hyperspec:lookup (cst:raw cst)))))

(defun cst-to-list (cst)
  "Transforms a cst into a list of csts of its subexpressions."
  (if (cst:null cst)
      '()
      (cons (cst:first cst) (cst-to-list (cst:rest cst)))))

(defun process-other-cst (cst env)
  "Process a CST that is not special"
  (let ((first (cst:first cst)))
    (cond ((not (macro-function (cst:raw first)))
           (parse-csts (cst-to-list cst) env))
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
  "Adds a symbol to env and creates a source-reference."
  (let* ((symbol (cst:raw cst))
         (symbol-information (make-symbol-information symbol))
         (file-location (cst:source cst))
         (sr (make-source-reference symbol-information file-location)))
    (values (list sr) (funcall add-env-function env sr))))

(defun add-symbols-to-env (csts env &optional
                                (add-env-function #'add-function-to-env))
  "Adds a list of symbols to env. Typically used with function lambda lists."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (add-symbol-to-env cst (cdr refs-env) add-env-function)
             (cons (append (car refs-env) refs) new-env))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

(define-condition binding-error ()
  ())

(defun process-binding (cst env &optional
                            (add-env-function #'add-variable-to-env))
  "Prcessess one binding of the form (binding form)."
  (let ((binding-cst (cst:first cst))
        (form-cst (cst:first (cst:rest cst))))
    ;; TODO add global special stuff from form
    (multiple-value-bind (sref newenv1)
      (add-symbol-to-env binding-cst env add-env-function)
      (multiple-value-bind (srefs newenv2) (parse-cst form-cst env)
        (declare (ignore newenv2))
        (values (append sref srefs) newenv1)))))

(defun process-bindings (csts env &optional (recursive nil))
  "Processes a list of binding csts."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs new-env)
             (process-binding cst (cdr refs-env))
             (cons (append (car refs-env) refs)
                   (if recursive
                       new-env
                       env)))))
    (let ((res (reduce #'helper csts :initial-value (cons '() env))))
      (values (car res) (cdr res)))))

(defun process-let-bindings (cst env)
  "Process a let/let*."
  (let* ((let-cst (cst:first cst))
         (recursive (eq (cst:raw let-cst) 'let*))
         (bindings (cst-to-list (cst:second cst)))
         (forms (cst-to-list (resti cst 2))))
    (multiple-value-bind (refs1 env1) (parse-atom let-cst env)
      (declare (ignore env1))
      (multiple-value-bind (refs2 env2) (process-bindings bindings env
                                                          recursive)
        ;; TODO add global special
        (multiple-value-bind (refs3 env3) (parse-csts forms env2)
          (declare (ignore env3))
          (values (append refs1 refs2 refs3) env))))))
