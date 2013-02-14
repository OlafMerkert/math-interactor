(in-package :math-interactor)

;;; output of ordinary numbers
(defclass number-math-output-record (math-output-record
                                     standard-sequence-output-record)
  ())

(defmethod math-output ((number number) stream)
  (with-output-to-output-record (stream 'number-math-output-record new-record
                                        :math-object number)
    (princ number stream)))

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

(defmethod math-output ((fraction fraction) stream)
  (with-output-to-output-record (stream 'fraction-math-output-record new-record
                                        :math-object fraction)
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
                (values (floor w 2) (- center y))))))))

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
                        (princ s stream)
                        new-operator))
                    (operators finite-sum)))
      (align-output-records (insert-operators summands operators)
                            #'stacking-align
                            #'centering-align))
    new-record))
