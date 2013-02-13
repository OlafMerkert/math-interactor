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
(defparameter *math-horizontal-spacing* 5)

(defun align-output-records (records horizontal-align-fn vertical-align-fn
                             &optional (x-combinate #'+) (y-combinate x-combinate))
  (let ((h-pos (funcall horizontal-align-fn (mapcar #'rectangle-width records) :horizontal))
        (v-pos (funcall vertical-align-fn (mapcar #'rectangle-height records) :vertical)))
    (mapc (lambda (record h-pos v-pos)
            (multiple-value-bind (x y) (output-record-position record)
                (setf (output-record-position record)
                      (values (funcall x-combinate x h-pos)
                              (funcall y-combinate y v-pos)))))
          records h-pos v-pos)
    ;; TODO return center coordinates??
    ))

(defun centering-align (lengths direction)
  (declare (ignore direction))
  (let ((m (reduce #'max lengths)))
    (mapcar (lambda (l) (/ (- m l) 2)) lengths)))

(defun stacking-align (lengths direction)
  (let ((position 0)
        (offset (case direction
                  (:vertical *math-vertical-spacing*)
                  (:horizontal *math-horizontal-spacing*)
                  (t 0))))
    (mapcar (lambda (l)
              (prog1 position
                (incf position (+ l offset))))
            lengths)))


(defmethod math-output ((fraction fraction) stream)
  (with-output-to-output-record (stream 'fraction-math-output-record new-record
                                        :math-object fraction)
    (with-slots (numerator denominator) new-record
      (setf numerator   (math-output (numerator fraction) stream)
            denominator (math-output (denominator fraction) stream))
      (stream-add-output-record stream numerator)
      (stream-add-output-record stream denominator)
      ;; calculate dimensions
      (align-output-records (list numerator denominator)
                            #'centering-align
                            #'stacking-align)
      (multiple-value-bind (x y) (output-record-position new-record)
        (let* ((w (rectangle-width new-record))
               (h (rectangle-width new-record))
               (middle (+ y (floor h 2) 1)))
          (draw-line* stream
                      x middle (+ x w) middle)))
)))

(define-math-interactor-command (com-output-test :menu t :name "Ausgabe testen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (format stream "~&Hallo~%")))

;; TODO make this behave more nicely with other output to the stream.
;; TODO fresh-line does not take into account that math-output might
;; be higher than text.
(defun stream-add-math-output (stream math-output &optional (move-cursor t))
  (stream-add-output-record stream math-output)
  (setf (output-record-position math-output) (stream-cursor-position stream))
  (when move-cursor
    (multiple-value-bind (x y) (stream-cursor-position stream)
      (setf (stream-cursor-position stream)
            (values (+ x (rectangle-width math-output) *math-horizontal-spacing*) y))))
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

