#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(in-package :cl-navigate-sc)

(defmacro with-client ((client) &body body)
  `(let ((,client (make-instance 'cst::client)))
     ,@body))

;; TODO the two functions below do the same thing. Refactor if no more
;; specialization is needed.

(defun singleton-parameter-group-p (cst)
  "Check wheather the group is a singleton-paramter-group-mixin"
  (subtypep (type-of cst) 'cst:singleton-parameter-group))

(defun parameter-group-get-parameters (cst)
  "Get the parameters as list for a parameter group."
  (if (singleton-parameter-group-p cst)
      (list (cst:parameter cst))
      (cst:parameters cst)))

(defun parse-ordinary-lambda-list (cst)
  "Process an ordinary lambda list and return the corresponding csts."
  (with-client (client)
    (let ((cst-lambda-list (cst:parse-ordinary-lambda-list client cst)))
      (flet ((extract-parameters (res cst-parameter-group)
               (let ((parameters
                       (parameter-group-get-parameters cst-parameter-group)))
                 (append res (mapcar #'cst:name parameters)))))
        (reduce #'extract-parameters (cst:children cst-lambda-list)
                :initial-value '())))))

(defun parse-lambda-list-type (cst-lambda-list)
  "Helper for parse-macro-lambda-list."
  (assert (subtypep (type-of cst-lambda-list) 'cst:lambda-list-type))
  ;; As macro-lambda lists can be arbitrary list structures we need to check
  ;; if recursive extracting is needed.
  (labels ((recursive-parse (cst)
             (if (subtypep (type-of cst) 'cst:lambda-list-type)
                 (parse-lambda-list-type cst)
                 (list (cst:name cst))))
           (extract-parameters (res cst-parameter-group)
             (let ((parameters
                     (parameter-group-get-parameters cst-parameter-group)))
               ;; TODO a bit ugly here. Clean up.
               (append res (alexandria:flatten
                             (mapcar #'recursive-parse parameters))))))
    (reduce #'extract-parameters (cst:children cst-lambda-list)
            :initial-value '())))

(defun parse-macro-lambda-list (cst)
  "Process an macro lambda list and returns"
  (with-client (client)
    (let ((cst-lambda-list (cst:parse-macro-lambda-list client cst)))
      (parse-lambda-list-type cst-lambda-list))))
