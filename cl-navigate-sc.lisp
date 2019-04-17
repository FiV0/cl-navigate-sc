;;;; cl-navigate-sc.lisp

#|
In this section we consider expression as returned by the
symbol-location-client. So if we say we process a binding of the (var expr), we
actually mean we process what the reader would have returned for such a form.
Which would be in this case something of the form:
 :RESULT (#SYBMOL-INFORMATION ...) :SOURCE #FILE-LOCATION :CHILDREN
   ((:RESULT #SYMBOL-INFORMATION :SOURCE #FILE-LOCATION :CHILDREN NIL)
    (:RESULT #SYMBOL-INFORMATION :SOURCE #FILE-LOCATION :CHILDREN NIL))
|#

(in-package #:cl-navigate-sc)

;; use it to check the standard
(ql:quickload :alexandria)

(define-condition not-yet-implemented ()
  ())

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
           (parent (if (symbol-function item)
                       (find-function item env)
                       (find-variable item env)))
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
  (let ((first (cst:raw first)))
    (cond ((eq first 'let) (process-let-bindings)) ;; TODO
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
           (parse-csts (cst-to-list cst))))))

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

(defun parse-csts (csts env)
  "Parse a list of expressions (csts)."
  (flet ((helper (refs-env cst)
           (multiple-value-bind (refs env) (parse-cst cst (cdr refs-env))
             (cons (append refs (car refs-env)) env))))
    (reduce #'helper csts :initial-value (cons '() env))))



