(defpackage :math-interactor
  (:nicknames :mi)
  (:use :clim-lisp :clim :ol :iterate)
  (:shadow :numerator :denominator)
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
                                       standard-sequence-output-record
                                       fraction)
  ())


(defclass fraction ()
  ((numerator   :initarg :numerator
                :initform nil
                :reader numerator)
   (denominator :initarg :denominator
                :initform nil
                :reader denominator))
  (:documentation "TODO"))

(defparameter *math-vertical-spacing* 5)
(defparameter *math-horizontal-spacing* 5)

(defun nth-arg (n)
  "Build a function that simply returns it's NTH argument."
  (lambda (&rest args)
    (nth n args )))

(defun align-output-records (records horizontal-align-fn vertical-align-fn
                             &optional (x-combinate (nth-arg 1)) (y-combinate x-combinate))
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
        (let* ((w (rectangle-width  new-record))
               (h (rectangle-height new-record))
               (middle (+ y (floor h 2) 1)))
          (draw-line* stream
                      x middle (+ x w) middle))))))

(defclass finite-sum ()
  ((summands :initarg :summands
         :initform nil
         :accessor summands)))

(defclass finite-sum-math-output-record (math-output-record
                                         standard-sequence-output-record
                                         finite-sum)
  ((operators :initarg :operators
              :initform nil
              :accessor operators)))

(defclass operator-math-output-record (math-output-record
                                       standard-sequence-output-record)
  ())

(defun insert-operators (arguments operators)
  "Given '(a b c) and '(+ -), create the output '(a + b - c)."
  (list* (first arguments)
         (alternate operators (rest arguments))))


(defmethod math-output ((finite-sum finite-sum) stream)
  (with-output-to-output-record (stream 'finite-sum-math-output-record new-record
                                        :math-object finite-sum)
    (with-slots (summands operators) new-record
      (setf summands
            (mapcar (lambda (s) (aprog1 (math-output s stream)
                             (stream-add-output-record stream it)))
                    (summands finite-sum))
            operators
            (mapcar (ilambda (s)
                      (with-new-output-record (stream 'operator-math-output-record new-operator)
                        (princ #\+ stream)
                        new-operator))
                    (rest (summands finite-sum))))
      (align-output-records (insert-operators summands operators)
                            #'stacking-align
                            #'centering-align))
    new-record))

(defclass finite-continued-fraction ()
  ((partial-quotients :initarg :partial-quotients
         :initform nil
         :accessor partial-quotients)))

(defun simple-cf-p (finite-continued-fraction)
  (length=1 (partial-quotients finite-continued-fraction)))


(defmethod math-output ((finite-continued-fraction finite-continued-fraction) stream)
  (math-output
   (if (simple-cf-p finite-continued-fraction)
       (first #1=(partial-quotients finite-continued-fraction))
       (make-instance
        'finite-sum
        :summands (list (first #1#)
                        (make-instance
                         'fraction
                         :numerator 1
                         :denominator
                         (make-instance
                          'finite-continued-fraction
                          :partial-quotients (rest #1#))))))
   stream))



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

(define-math-interactor-command (com-sum :menu t :name "Summe anzeigen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output (make-instance 'finite-sum
                                                        :summands (list 1 2 3 4 5 6
                                                                        (make-instance 'fraction :numerator 17 :denominator 1329846)))
                                         stream))
    (stream-replay stream)))

(define-math-interactor-command (com-cf :menu t :name "Kettenbruch anzeigen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output (make-instance 'finite-continued-fraction
                                                        :partial-quotients (list 1 2 3 4 5 6 7))
                                         stream))
    (stream-replay stream)))
