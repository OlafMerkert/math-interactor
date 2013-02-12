(defpackage :math-interactor
  (:nicknames :mi)
  (:use :clim-lisp :clim :ol :iterate)
  (:export))

(in-package :math-interactor)

(define-application-frame math-interactor ()
  ()
  (:panes (app :application
               :width 700 :height 600
               :incremental-redisplay t)
          (int :interactor
               :width 500 :height 200)
          (bin :application
               :width 300 :height 500
               :incremental-redisplay t))
  (:layouts (default
                (horizontally ()
                  bin
                  (vertically ()
                    app int)))))

(defun math-interactor ()
  (run-frame-top-level (make-instance 'math-interactor)))

;; define a command to exit
(define-math-interactor-command (com-quit :menu t :name "Beenden") ()
  (frame-exit *application-frame*))

(defmacro with-pane ((pane-name) &body body)
  `(let ((*standard-output* (get-frame-pane *application-frame* ',pane-name)))
     ,@body))


(defclass math-output-record (output-record)
  ((math-object :initarg :math-object
                :accessor math-object)))

(defgeneric math-output (math-object stream))

(defclass number-math-output-record (math-output-record
                                     standard-sequence-output-record)
  ())

(defmethod math-output ((number number) stream)
  (with-output-to-output-record (stream 'number-math-output-record new-record
                                        :math-object number)
    (princ number stream)))

(defclass fraction-math-output-record (math-output-record
                                       standard-sequence-output-record)
  ((numerator   :initarg :numerator
                :initform nil
                :accessor numerator)
   (denominator :initarg :denominator
                :initform nil
                :accessor denominator)))


(shadow 'numerator)
(shadow 'denominator)

(defclass fraction ()
  ((numerator   :initarg :numerator
                :initform 0
                :reader numerator)
   (denominator :initarg :denominator
                :initform 1
                :reader denominator))
  (:documentation "TODO"))

(defun calc-offset (l1 l2)
  (if (< l1 l2)
      (values (/ (- l2 l1) 2) 0)
      (values 0 (/ (- l1 l2) 2))))

(defparameter *math-vertical-spacing* 5)

(defmethod math-output ((fraction fraction) stream)
  (with-output-to-output-record (stream 'fraction-math-output-record new-record
                                        :math-object fraction)
    (with-slots (numerator denominator) new-record
      (setf numerator   (math-output (numerator fraction) stream)
            denominator (math-output (denominator fraction) stream))
      (stream-add-output-record stream numerator)
      (stream-add-output-record stream denominator)
      ;; calculate dimensions
      (let ((n-w (rectangle-width  numerator))
            (n-h (rectangle-height numerator))
            (d-w (rectangle-width  denominator))
            ;;(d-h (rectangle-height denominator))
            )
        ;; align n and d horizontally and move d down.
        (multiple-value-bind (n-x d-x) (calc-offset n-w d-w)
          (setf (output-record-position numerator)
                (values n-x 0)
                (output-record-position denominator)
                (values d-x (+ *math-vertical-spacing*
                               n-h))))
        ;; draw the line
        (draw-line* stream
                    0 #1=(+ (floor *math-vertical-spacing* 2) n-h)
                    (max n-w d-w) #1#)))))

(define-math-interactor-command (com-output-test :menu t :name "Ausgabe testen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (format stream "~&Hallo~%")))

;; TODO make this behave more nicely with other output to the stream.
;; TODO figure out how to make the output appears sequentially
(defun stream-add-math-output (stream math-output &optional (move-cursor t))
  (stream-add-output-record stream math-output)
  (when move-cursor
    (setf (stream-cursor-position stream)
         (output-record-end-cursor-position math-output)))
  math-output)


(define-math-interactor-command (com-fraction :menu t :name "Bruch anzeigen")
    ()
  (let ((numer 3182)
        (denom 326)
        (stream (get-frame-pane *application-frame* 'app)))
    ;; TODO move cursor forward.
    (stream-add-math-output stream
                              (math-output (make-instance 'fraction
                                                          :denominator denom
                                                          :numerator numer) stream))
    (stream-replay stream)))

