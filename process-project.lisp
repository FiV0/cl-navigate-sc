#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(in-package #:cl-navigate-sc)

(defclass file-source-references ()
  ((source-references :initarg :source-references
                      :accessor file-source-references
                      :initform '()
                      :documentation "List of source references for a file.")
   (system-name :initarg :system-name
                :accessor :file-system-anme
                :documentation "System the source references are part of.")
   ;(path :initarg :path
         ;:accessor :file-path
         ;:documentation "Path to the file from the repo root.")
   (filepath :initarg :filepath
             :accessor :file-filepath
             :documentation "Filepaht of the file")))

(defun make-file-source-references (srs system-name filepath)
  (make-instance 'file-source-references
                 :source-references srs
                 :system-name system-name
                 :filepath filepath))

;;general process
;; add path to registry
;; load system
;; process
;; ql:uninstall

(defun setup-system (system-name path)
  "Setup for processing a directory."
  (when path
    (push path asdf:*central-registry*))
  (asdf:load-system system-name))

(defun clean-system (system-name)
  "Clean up after a system has been loaded with SETUP-SYSTEM."
  ;; TODO check if quicklisp remains intact
  ;(ql:uninstall system-name)
  (declare (ignore system-name))
  (asdf:clear-source-registry))

(defun get-source-files (system-name)
  "Gets the source files in order of dependency for a "
  (let ((system (asdf:find-system system-name)))
    (labels ((extract-files-recursive (component)
               (if (or (subtypep (type-of component) 'asdf/component:module)
                       (subtypep (type-of component) 'asdf/system:system))
                   (reduce #'(lambda (res child)
                               (append res (extract-files-recursive child)))
                           (asdf/component:component-children component)
                           :initial-value '())
                   (when (subtypep (type-of component)
                                   'asdf/lisp-action:cl-source-file)
                     (list component)))))
      (extract-files-recursive system))))

(defun remove-path-to-root (path-to-root filepath)
  (subseq (namestring filepath) (length (namestring path-to-root))))

;; TODO sovle the in-package issue
(defun process-source-file (file env system-name path-to-root)
  "Parses a file of a system. For now also the package is given."
  (format *standard-output* "Processing file ~a in system ~a.~%"
          (asdf/component:component-pathname file)
          system-name)
  (let* ((filepath (asdf/component:component-pathname file))
         (csts (cl-navigate-sc:parse-from-file filepath))
         (src-refs (parse-program csts env))
         (relative-filepath (remove-path-to-root path-to-root filepath)))
    (make-file-source-references src-refs system-name relative-filepath)))

(defun process-system (system-name path-to-root)
  "Process a whole system and returns the source references."
  ;;TODO add path-to-root
  (setup-system system-name nil)
  (let ((source-files (get-source-files system-name))
        (env (empty-environment))
        (res '()))
    (dolist (source-file source-files)
      (push (process-source-file source-file env system-name path-to-root) res))
    (clean-system system-name)
    (nreverse res)))
