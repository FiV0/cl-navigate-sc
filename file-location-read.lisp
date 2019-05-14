;;;; cl-navigate-sc.lisp

(in-package #:cl-navigate-sc)

;; TODO remove later
(setf *print-circle* T)


(define-condition file-position-error (error) ()
  (:documentation "An error that gets signalled when repositioning of a file
                   stream fails"))

(defclass file-location ()
  ((start-line :initarg :start-line
               :accessor file-location-start-line
               :documentation "The start line number of file location.")
   (end-line :initarg :end-line
             :accessor file-location-end-line
             :documentation "The end line number of file location.")
   (start :initarg :start
          :accessor file-location-start
          :documentation "The start column of the file location.")
   (end :initarg :end
        :accessor file-location-end
        :documentation "The end column of the file location (exclusive).")))

(defclass source-location (file-location)
  ((source-filepath :initarg :source-filepath
                    :accessor source-location-source-filepath
                    :documentation "The filename of the source file.")))

(defun calculate-line-breaks (is)
  "Calculates the file positions in IS where line breaks occur."
  (let ((cur-position (file-position is))
        (res '()))
    (unless (file-position is :start)
      (error 'file-position-error))
    (do ((c (read-char is) (read-char is nil 'eof)))
        ((not (characterp c)))
       (when (or (eq c #\return) (eq c #\newline))
         (push (1- (file-position is)) res)))
    (unless (file-position is cur-position)
      (error 'file-position-error))
    (nreverse res)))

(defun find-if-consecutive (fn lst)
  "Applies FN on consecutive pairs of LST and returns the first element,
   position in LST if FN returns true (generilized boolean) and nil, nil
   otherwise."
  (loop for (a b) on lst while b
        for i = 1 then (1+ i)
        when (funcall fn a b)
        do (return (values a i))))


(defun file-position-to-location (position line-breaks)
  "Given a list of LINE-BREAKS positions return the pair line . column for
   POSITION."
  ;; TODO can be optimized
  (if (or (null line-breaks) (<= position (car line-breaks)))
      ;;first line case
      (cons 0 position)
      (multiple-value-bind (last-line-break linenumber)
        (find-if-consecutive
          #'(lambda (pos1 pos2)
              (declare (ignore pos1))
              (<= position pos2))
          line-breaks)
       (if last-line-break
           ;;normal case
           (cons linenumber (- position last-line-break 1))
           ;;last line case
           (cons (length line-breaks)
                 (- position (car (last line-breaks)) 1))))))

(defun create-file-position-to-file-location-function (is)
  "Takes a input stream IS and returns a function that given a file-position
   returns a line . column pair. Does not modify IS."
  (let ((line-breaks (calculate-line-breaks is)))
    #'(lambda (position)
        (file-position-to-location position line-breaks))))

;; This client only serves to transform the source
(defclass cst-source-position (eclector.concrete-syntax-tree:cst-client)
  ((file-position-to-file-location :initarg :file-position-to-file-location
                                   :reader cst-source-position-fp-to-fl
                                   :documentation
                                   "A function that transforms a
                                    file-postion to a line,column pair.")
   (current-package :initarg :current-package
                    :accessor cst-source-position-current-package
                    :initform (find-package :cl)
                    :documentation "The current package when reading a file.")
   (current-file :initarg :current-file
                 :accessor cst-source-position-current-file
                 :initform nil
                 :documentation "The current file being processed.")
   (fallback-package :initarg :current-package
                     :accessor cst-source-position-fallback-package
                     :initform (make-package (gensym "TEMPORARY"))
                     :documentation "A package for interning unknown symbols ")
   ))

(defmethod eclector.parse-result:make-source-range
  ((client cst-source-position) start end)
  (let* ((location-function (cst-source-position-fp-to-fl client))
         (start-line-column (funcall location-function start))
         (end-line-column (funcall location-function end)))
    (make-instance 'source-location
                   :start-line (car start-line-column)
                   :start (cdr start-line-column)
                   :end-line (car end-line-column)
                   :end (cdr end-line-column)
                   :source-filepath (cst-source-position-current-file client))))

(define-condition change-package-error ()
  ())

(defun change-package (client input-stream)
  "Change the default package of client when an IN-PACKAGE token has been read
   from input-stream."
  (let ((cur-position (file-position input-stream))
        (package-indicator
          (make-array 100
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0))
        (package-indicator-capitalized
          (make-array 100
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)))
    (loop for c = (read-char input-stream)
          until (not (member c '(#\Space #\: #\#)))
          finally (unread-char c input-stream))
    (loop for c = (read-char input-stream)
          until (or (eq c #\Space) (eq c #\) ))
          do (vector-push-extend c package-indicator))
    (unless (file-position input-stream cur-position)
      (error 'change-package-error))
    (format package-indicator-capitalized "~@:(~a~)" package-indicator)
    (setf (cst-source-position-current-package client)
          (find-package package-indicator-capitalized))
    ;;TODO do something fatal here
    (assert (not (null (cst-source-position-current-package client))))
    client))
(find-package "HUNCHENTOOT")

(defmethod eclector.reader:interpret-symbol
  ((client cst-source-position) input-stream (package-indicator null)
                                symbol-name internp)
  (intern symbol-name (cst-source-position-fallback-package client)))

(defmethod eclector.reader:interpret-symbol
  ((client cst-source-position) input-stream package-indicator symbol-name
                                internp)
  (declare (ignore internp))
  ;; TODO remove this big hack
  (when (equal symbol-name "IN-PACKAGE")
    (change-package client input-stream))
  (let ((package (case package-indicator
                   (:current (cst-source-position-current-package client))
                   (:keyword (find-package "KEYWORD"))
                   (t        (or (find-package package-indicator)
                                 (eclector.reader::%reader-error
                                  input-stream
                                  'eclector.reader:package-does-not-exist
                                  :package-name package-indicator))))))
    (multiple-value-bind (symbol status)
      (find-symbol symbol-name package)
      ;(format *standard-output* "Symbol ~a Package ~a Status ~a.~%"
              ;symbol-name package status)
      (cond ((null status)
             ;; TODO see above
             (intern symbol-name (cst-source-position-fallback-package client)))
            ((eq status :internal)
             symbol)
            (t
             symbol)))))

(defmethod eclector.reader:check-feature-expression
  ((client cst-source-position) (feature-expression t))
  ;;TODO
  (declare (ignore client))
  (declare (ignore feature-expression)))

;(find-method #'eclector.reader:check-feature-expression
             ;'() (mapcar #'find-class '(cst-source-position t)))
;(remove-method #'eclector.reader:check-feature-expression *)

;;TODO cleanup the filepaht thing in the two functions below

(defun read-program (is &optional (filepath nil))
  "Reads a source-file from the stream IS. Assumes all dependent packages have
   been loaded."
  (alexandria:with-gensyms (eof)
    (let ((client
            (make-instance 'cst-source-position
                           :file-position-to-file-location
                           (create-file-position-to-file-location-function is)
                           :current-package *package*
                           :current-file filepath)))
     (loop for exp = (eclector.parse-result:read client is nil eof)
           until (equal exp eof)
           collect exp))))

(defun parse-from-file (filepath &optional (relative-filepath nil))
  "Parses a whole file into a list of expressions."
  (with-open-file (st filepath)
    (read-program st relative-filepath)))

(defclass symbol-information ()
  ((symbol :initarg :symbol
           :accessor symbol-information-symbol
           :documentation "The symbol value.")
   (error :initarg :error
          :accessor symbol-information-error
          :documentation "nil if no error. One of the following otherwise:
                          'package-does-not-exist
                          'symbol-does-not-exist
                          'symbol-is-not-external")))

(defun symbol-information-p (symbol-information)
  (eq (type-of symbol-information) 'symbol-information))

(defun make-symbol-information (symbol &optional (error nil))
  (make-instance 'symbol-information
                 :symbol symbol
                 :error error))


;;;;;;;;;;
;;;
;;; This is most likely all that we need.
;;;

;; slot with source information
(defclass symbol-location-client (eclector.parse-result:parse-result-client)
  ((file-position-to-file-location :initarg :file-position-to-file-location
                                   :reader symbol-location-client-fp-to-fl
                                   :documentation
                                   "A function that transforms a file-postion to
                                    a line,column pair.")))

(defmethod eclector.parse-result:make-expression-result
    ((client symbol-location-client) (result t) (children t) (source t))
  (list :result result :source source :children children))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client symbol-location-client) (stream t) (reason t) (source t))
  (list :reason reason :source source))

(defmethod eclector.parse-result:make-source-range
  ((client symbol-location-client) start end)
  (let* ((location-function (symbol-location-client-fp-to-fl client))
         (start-line-column (funcall location-function start))
         (end-line-column (funcall location-function end)))
    (make-instance 'file-location
                   :start-line (car start-line-column)
                   :start (cdr start-line-column)
                   :end-line (car end-line-column)
                   :end (cdr end-line-column))))

(defmethod eclector.reader:interpret-symbol
  ((client symbol-location-client) input-stream (package-indicator null)
                                   symbol-name internp)
  (intern symbol-name *temporary-package*))

;; mainly copied from Eclector by Robert Strandh
(defmethod eclector.reader:interpret-symbol
  ((client symbol-location-client) input-stream package-indicator
                                   symbol-name internp)
  (declare (ignore internp))
  (let* ((package-non-existant nil)
         (package (case package-indicator
                    (:current *package*)
                    (:keyword (find-package "KEYWORD"))
                    (t        (or (find-package package-indicator)
                                  (progn
                                    (setf package-non-existant T)
                                    (make-package package-indicator))
                                  ;; Should not happen
                                  (eclector.reader::%reader-error
                                    input-stream 'package-does-not-exist
                                    :package-name package-indicator))))))
    (if package-non-existant
        (make-symbol-information (intern symbol-name package)
                                 'package-does-not-exist)
        (multiple-value-bind (symbol status)
          (find-symbol symbol-name package)
          (cond ((null status)
                 (make-symbol-information (intern symbol-name package)
                                          'symbol-does-not-exist))
                 ((eq status :internal)
                  (make-symbol-information symbol
                                           'symbol-is-not-external))
                 (t (make-symbol-information symbol)))))))

(define-condition feature-expression-malformed () ())

(defmethod eclector.reader:check-feature-expression
  ((client symbol-location-client) (feature-expression t))
  (declare (ignore client))
  (unless (or (symbol-information-p feature-expression)
              (and (consp feature-expression) (every #'symbol-information-p
                                                     feature-expression)))
    (error 'feature-expression-malformed)))
