#|
  This file is a part of cl-navigate-sc-test.
  (c) 2019 fiv0
  Author: Finn Völkel <firstname.lastname@gmail.com>
|#

(in-package #:cl-navigate-sc)

;; TODO system-name and filepath have somehow been made redundant by the fact
;; that  filepath is now part of source-location and the system name also known
;; when processing files
(defclass source-references-wrapper ()
  ((source-references :initarg :source-references
                      :accessor wrapper-source-references
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
  (make-instance 'source-references-wrapper
                 :source-references srs
                 :system-name system-name
                 :filepath filepath))

;;general process
;; add path to registry
;; load system
;; process
;; ql:uninstall

(defun setup-system (system-name)
  "Setup for processing a directory."
  (asdf:load-system system-name))

(defun clean-system (system-name)
  "Clean up after a system has been loaded with SETUP-SYSTEM."
  (asdf:clear-system system-name))

(defmacro with-system-setup ((system-name) &body body)
  `(progn
     (setup-system ,system-name)
     (unwind-protect
       (progn ,@body)
       (clean-system ,system-name))))

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
  (assert (and path-to-root filepath))
  (subseq (namestring filepath) (length (namestring path-to-root))))

;; TODO sovle the in-package issue
(defun process-source-file (file env system-name path-to-root)
  "Parses a file of a system. For now also the package is given."
  (format *standard-output* "Processing file ~a in system ~a.~%"
          (asdf/component:component-pathname file)
          system-name)
  (let* ((filepath (asdf/component:component-pathname file))
         (relative-filepath (remove-path-to-root path-to-root filepath))
         (csts (parse-from-file filepath relative-filepath))
         (src-refs (parse-program csts env)))
    (make-file-source-references src-refs system-name relative-filepath)))

;;TODO root can most likely be found from asdf/system:system class
(defun process-system (system-name path-to-root)
  "Process a whole system and returns the source references."
  ;;TODO add path-to-root
  (with-system-setup (system-name)
    (let ((source-files (get-source-files system-name))
          (env (empty-environment))
          (res '()))
      (dolist (source-file source-files)
        (push (process-source-file source-file env system-name path-to-root) res))
      (nreverse res))))
