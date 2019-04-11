;;;; cl-navigate-sc.lisp

(in-package #:cl-navigate-sc)

(setf *print-circle* T)

;;working with hunchentoot as an example
(ql:quickload :hunchentoot)

(declaim (optimize (speed 0) (space 0) (debug 3)))

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
  ((source-filename :initarg :source-filename
                    :accessor source-location-source-file
                    :documentation "The filename of the source file.")))

(defun calculate-line-breaks (is)
  "Calculates the file positions in IS where line breaks occur."
  (let ((cur-position (file-position is))
        (res '()))
    (unless (file-position is :start)
      (error 'file-position-error))
    (do ((c (read-char is) (read-char is nil 'eof))
         (cnt 0 (incf cnt)))
        ((not (characterp c)))
       (when (or (eq c #\return) (eq c #\newline))
         (push cnt res)))
    (unless (file-position is cur-position)
      (error 'file-position-error))
    (nreverse res)))

#+(or)
(defparameter filepath #P"./tmp/lisp-test-file.lisp")
#+(or)
(defparameter filepath2 #P"./tmp/request.lisp")
#+(or)
(with-open-file (is filepath)
  (calculate-line-breaks is))

(defun find-if-consecutive (fn lst)
  "Applies FN on consecutive pairs of LST and returns the first element,
   position in LST if FN returns true (generilized boolean) and nil, nil
   otherwise."
  (loop for (a b) on lst while b
        for i = 1 then (1+ i)
        when (funcall fn a b)
        do (return (values a i))))

#+(or)
(find-if-consecutive #'(lambda (x y) (declare (ignore x)) (< 7 y)) '(1 3 6 8))

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
           (cons linenumber (- position last-line-break))
           ;;last line case
           (cons (length line-breaks)
                 (- position (car (last line-breaks))))))))

#+(or)
(file-position-to-location 69 '(1 45))

(defun create-file-position-to-file-location-function (is)
  "Takes a input stream IS and returns a function that given a file-position
   returns a line . column pair. Does not modify IS."
  (let ((line-breaks (calculate-line-breaks is)))
    #'(lambda (position)
        (file-position-to-location position line-breaks))))

#+(or)
(with-open-file (is filepath)
  (let ((fn (create-file-position-to-file-location-function is)))
    (funcall fn 209)))

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

(defclass symbol-information ()
  ((symbol :initarg :symbol
           :accessor symbol-information-symbol
           :documentation "The symbol value.")
   (error :initarg :error
          :accessor symbol-information-error
          :documentation "T if no error. One of the following otherwise:
                          'package-does-not-exist
                          'symbol-does-not-exist
                          'symbol-is-not-external")))

(defun symbol-information-p (symbol-information)
  (eq (type-of symbol-information) 'symbol-information))

(defun make-symbol-information (symbol &optional (error T))
  (make-instance 'symbol-information
                 :symbol symbol
                 :error error))

(defparameter *temporary-package* (make-package 'temporary))

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

#+(or)
(defparameter program "(1 #|comment|# ;;test
                       \"string\")")
#+(or)
(defparameter program2 "(let ((a '#1=(10 . #1#))) (nth 42 a))")
#+(or)
(defparameter program3 "(flatten '(1 (2 3) 4))")

#+(or)
(find-method #'eclector.reader:interpret-symbol '()
             (mapcar #'find-class '(symbol-location-client T T T T)))

#+(or)
(remove-method #'eclector.reader:interpret-symbol *)

#+(or)
(with-input-from-string (is program3)
  (eclector.parse-result:read
    (make-instance 'symbol-location-client
                   :file-position-to-file-location
                   (create-file-position-to-file-location-function is))
    is))

(defun read-program (is)
  "Reads a source-file from the stream IS. Assumes all dependent packages have
   been loaded."
  (alexandria:with-gensyms (eof)
    (let ((client
            (make-instance 'symbol-location-client
                           :file-position-to-file-location
                           (create-file-position-to-file-location-function is))))
     (loop for object = (eclector.parse-result:read client is nil eof)
           while (not (equal eof object))
           collect object))))

(defun parse-from-file (filepath)
  (with-open-file (st filepath)
    (read-program st)))

#+(or)
(parse-from-file filepath2)
