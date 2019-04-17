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

;; checking for symbol to be macro use macro-function
