;;;; environment.lisp

(in-package #:cl-navigate-sc)

(defclass source-reference (symbol-information source-location)
  ((source-reference :initarg :parent
                     :accessor source-reference-parent
                     :documentation "EQ to itself if the symbol is also the
                                     definition. Otherwise points to the
                                     SOURCE-REFERENCE of the definition.")))

(defun set-source-reference (source-reference parent)
  (setf (source-reference-parent source-reference) parent)
  source-reference)

(defun make-source-reference (symbol-information source-location
                                                 &optional (parent nil))
  (let ((instance
          (make-instance 'source-reference
                         :parent parent
                         :start-line (file-location-start-line source-location)
                         :end-line (file-location-end-line source-location)
                         :start (file-location-start source-location)
                         :end (file-location-end source-location)
                         :source-filename
                         (source-location-source-file source-location)
                         :symbol (symbol-information-symbol symbol-information)
                         :error (symbol-information-error symbol-information))))
    ;(if parent
        ;(set-source-reference instance parent)
        ;(set-source-reference instance instance))
    instance))

(defun dummy-source-reference (symbol)
  (make-instance 'source-reference
                 :parent nil
                 :start-line 0 :end-line 0 :start 0 :end 0
                 :symbol symbol
                 :error nil))

(define-condition missing-symbol-information () ())

(defclass scope ()
  ((v-namespace :initarg :v-namespace
                :accessor scope-v-namespace
                :initform '()
                :documentation "Variable namespace. Currently an a-list
                                symbol . source-reference.")
   (f-namespace :initarg :f-namespace
                :accessor scope-f-namespace
                :initform '()
                :documentation "Function namespace. Currently an a-list
                                symbol . source-reference.")))

(defun empty-scope ()
  "Creates an empty scope instance."
  (make-instance 'scope))

(defun copy-scope (scope)
  (make-instance 'scope
                 :v-namespace (copy-list (scope-v-namespace scope))
                 :f-namespace (copy-list (scope-f-namespace scope))))

(defun join-scope (scope1 scope2)
  (make-instance 'scope
                 :v-namespace (append (copy-list (scope-v-namespace scope1))
                                      (copy-list (scope-v-namespace scope2)))
                 :f-namespace (append (copy-list (scope-f-namespace scope1))
                                      (copy-list (scope-f-namespace scope2)))))

;; The local vs global scope here is essentially to create source references in
;; quasiquated code the refers back to the global scope, instead of the local.
(defclass environment ()
  ((local-scope :initarg :local-scope
                :accessor env-local-scope
                :initform (empty-scope)
                :documentation "Local scope namespaces.")
   (global-scope :initarg :global-scope
                 :accessor env-global-scope
                 :initform (empty-scope)
                 :documentation "Global scope namespaces")))

(defun empty-environment ()
  "Creates an empty environement instance."
  (make-instance 'environment))

(defun copy-environment (env)
  (make-instance 'environment
                 :local-scope (copy-scope (env-local-scope env))
                 :global-scope (copy-scope (env-global-scope env))))

(defun join-environments (env1 env2)
  (make-instance 'environment
                 :local-scope (join-scope (env-local-scope env1)
                                          (env-local-scope env2))
                 :global-scope (join-scope (env-global-scope env1)
                                           (env-global-scope env2))))

(defmacro with-env-copy ((copy env) &body body)
  `(let ((,copy (copy-environment ,env)))
     ,@body))

(defun local-variable-namespace (env)
  "Return the local variable namespace."
  (scope-v-namespace (env-local-scope env)))

(defun local-function-namespace (env)
  "Return the local function namespace."
  (scope-f-namespace (env-local-scope env)))

(defun global-variable-namespace (env)
  "Return the global variable namespace."
  (scope-v-namespace (env-global-scope env)))

(defun global-function-namespace (env)
  "Return the global function namespace."
  (scope-f-namespace (env-global-scope env)))

(defun add-variable-to-env (env source-reference)
  "Add a variable to the local scope."
  (push (cons (symbol-information-symbol source-reference) source-reference)
        (scope-v-namespace (env-local-scope env)))
  env)

(defun add-function-to-env (env source-reference)
  "Add a function to the local scope."
  (push (cons (symbol-information-symbol source-reference) source-reference)
        (scope-f-namespace (env-local-scope env)))
  env)

(defun add-variable-to-env-global (env source-reference)
  "Add a variable to the global scope."
  (push (cons (symbol-information-symbol source-reference) source-reference)
        (scope-v-namespace (env-global-scope env)))
  env)

(defun add-function-to-env-global (env source-reference)
  "Add a function to the global scope."
  (push (cons (symbol-information-symbol source-reference) source-reference)
        (scope-f-namespace (env-global-scope env)))
  env)

(define-condition missing-source-reference ()
  ())

(defun find-binding (symbol env
                            &optional
                            (local-namespace-f #'local-variable-namespace)
                            (global-namespace-f #'global-variable-namespace))
  ;; TODO maybe use aif
  (let ((local-src-ref (assoc symbol (funcall local-namespace-f env))))
    (if local-src-ref (values (cdr local-src-ref) T)
        (let ((global-src-ref (assoc symbol
                                     (funcall global-namespace-f env))))
          (if global-src-ref (values (cdr global-src-ref) nil)
              (error 'missing-source-reference))))))

(defun find-variable (var env)
  "Find the variable in env and return a source-reference. A second value
   indicates if it was found in the local (T) or global (nil) scope."
  (find-binding var env))

(defun find-function (f env)
  "Find the function/macro in env and return a source-reference. A second value
   indicates if it was found in the local (T) or global (nil) scope."
  (find-binding f env #'local-function-namespace #'global-function-namespace))
