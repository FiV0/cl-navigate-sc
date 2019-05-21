;; utils for cl-navigate-sc

(in-package #:cl-navigate-sc)

(defun symbol-lambda-list (symbol)
  "Returns the lambda list of a function/macro."
  (sb-introspect:function-lambda-list symbol))

(defmacro specialp (symbol)
  "Checks wether a variable is special."
  (let ((f (gensym "FUNC-")))
    `(let ((,symbol 1))
       (let ((,f (lambda () ,symbol)))
         (let ((,symbol 2))
           (eql 2 (funcall ,f)))))))

(defun symbol-standard-p (symbol)
  "Checks wether a symbol is in :cl."
  (eq (symbol-package symbol)
      #.(find-package :cl)))

(defun macroexpand* (form)
  "Macroexpands a form that starts with a macro recursively until it no longer
   starts with a macro."
  (if (and (consp form) (not (symbol-standard-p (car form))))
      (macroexpand-1 form)
      form))
