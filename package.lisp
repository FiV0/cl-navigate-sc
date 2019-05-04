;;;; package.lisp

(defpackage #:cl-navigate-sc
  (:use #:cl)
  (:nicknames #:nav)
  (:export #:cl-navigate-sc
           ;; definitely needed
           #:file-position-error
           #:file-location
           #:start
           #:end
           #:start-line
           #:end-line
           #:file-location-start-line
           #:file-location-end-line
           #:file-location-start
           #:file-location-end
           #:source-location
           #:source-location-source-filepath
           #:source-filepath
           #:source-references-wrapper
           #:wrapper-source-references
           #:process-system
           #:symbol-information
           #:symbol-information-symbol
           #:symbol-information-error
           #:source-reference
           #:source-reference-parent
           ;; TO CHECK
           #:source-location
           #:calculate-line-breaks
           #:find-if-consecutive
           #:file-position-to-location
           #:symbol-location-client
           #:create-file-position-to-file-location-function
           #:read-program
           #:parse-from-file
           #:empty-environment
           #:parse-cst
           #:parse-csts
           ))
