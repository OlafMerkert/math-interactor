(in-package :math-interactor)

;;; some helper functions
(declaim (inline math-output-to-record
                 princ-to-stream))

(defun math-output-to-record (stream)
  (lambda (s) (aprog1 (math-output s stream)
           (stream-add-output-record stream it))))

(defun princ-to-stream (stream &optional (record-type 'operator-math-output-record))
  (lambda (s)
    (with-new-output-record (stream record-type new-operator)
      (princ s stream)
      new-operator)))

(defmacro define-math-output-method ((type output-record-type) &body body)
  `(defmethod math-output ((,type ,type) stream)
     (with-output-to-output-record (stream ',output-record-type new-record
                                           :math-object ,type)
       ,@body)))


;;; output of ordinary numbers
(defclass number-math-output-record (math-output-record
                                     standard-sequence-output-record)
  ())

(define-math-output-method (number number-math-output-record)
  
  (princ number stream))

;;; output of (explicit) fractions
(defclass fraction ()
  ((numerator   :initarg :numerator
                :initform nil
                :reader numerator)
   (denominator :initarg :denominator
                :initform nil
                :reader denominator))
  (:documentation "TODO"))

(defclass fraction-math-output-record (math-output-record/with-center
                                       standard-sequence-output-record
                                       fraction)
  ())

(define-math-output-method (fraction fraction-math-output-record)
  (with-slots (numerator denominator) new-record
    (setf numerator   (math-output (numerator fraction) stream)
          denominator (math-output (denominator fraction) stream))
    (stream-add-output-record stream numerator)
    (stream-add-output-record stream denominator)
    ;; calculate dimensions and move n/d in position
    (align-output-records (list numerator denominator)
                          #'centering-align
                          #'stacking-align)
    ;; draw the line of the fraction at the appropriate place
    (multiple-value-bind (x y) (output-record-position new-record)
      (let* ((w (rectangle-width  new-record))
             (h (rectangle-height numerator))
             (center (+ y h (floor *math-vertical-spacing* 2))))
        (draw-line* stream
                    x center (+ x w) center)
        ;; mark center offset
        (setf (center-offset new-record)
              (values (floor w 2) (- center y)))))))

;;; finite sum expressions
(defclass finite-sum ()
  ((summands  :initarg :summands
              :initform nil
              :accessor summands)
   (operators :initarg :operators
              :initform nil
              :accessor operators)))

(defclass finite-sum-math-output-record (math-output-record
                                         standard-sequence-output-record
                                         finite-sum)
  ())

(defmethod initialize-instance :after ((sum finite-sum-math-output-record) &key)
  (with-slots (summands operators) sum
    (cond ((= (length summands) (+ 1 (length operators))))
          ((null operators)
           (setf operators (mapcar (constantly '+) (rest summands))))
          (t (error "finite-sum: length of summands (~A) and operators (~A) do not match."
                    (length summands) (length operators))))))

(define-math-output-method (finite-sum finite-sum-math-output-record-sum)
    (with-slots (summands operators) new-record
    (setf summands
          (mapcar (math-output-to-record stream)
                  (summands finite-sum))
          operators
          (mapcar (princ-to-stream stream 'operator-math-output-record)
                  (operators finite-sum)))
    (align-output-records (insert-operators summands operators)
                          #'stacking-align
                          #'centering-align))
  new-record)
