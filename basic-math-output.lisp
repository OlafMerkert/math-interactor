(in-package :math-interactor)

;;; some helper functions
(declaim (inline math-output-to-record
                 princ-to-stream))

(defun math-output-to-record (stream)
  (lambda (s) (math-output s stream)))

(defun princ-to-stream (stream &optional (record-type 'operator-math-output-record))
  (lambda (s)
    (with-new-output-record (stream record-type new-operator)
      (princ s stream)
      new-operator)))

;;; macros to take care of the boilerplate for establishing new basic types
(defmacro define-basic-math-output-class (type slots &key centering primitive)
  "If CENTERING, "
  (let ((mo-classname (symb type '-math-output-record)))
    `(progn
       ,@(unless primitive
                 `((defclass ,type ()
                     ,(mapcar #`(,a1 :initarg ,(keyw a1)
                                     :initform nil
                                     :reader ,a1) ; TODO maybe use accessor here
                              slots))))
       (defclass ,mo-classname
           (,(if centering
                 'math-output-record/with-center
                 'math-output-record)
             standard-sequence-output-record
             ,@(unless primitive
                       (list type)))
         ()))))

(defmacro define-basic-math-output-method ((type output-record-type) &body body)
  "Anaphoric variables: STREAM, NEW-RECORD."
  `(defmethod math-output ((,type ,type) stream)
     (with-new-output-record (stream ',output-record-type new-record
                                           :math-object ,type)
       ,@body
       new-record)))

(defmacro define-basic-math-output ((type slots &key centering primitive) &body body)
  `(progn
     (define-basic-math-output-class ,type ,slots :centering ,centering :primitive ,primitive)
     (define-basic-math-output-method (,type ,(symb type '-math-output-record))
       ,@(if primitive body
             `((with-slots ,slots new-record ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output of ordinary numbers
(define-basic-math-output (number nil :primitive t)
  (princ number stream))

;;; output of (explicit) fractions
(define-basic-math-output (fraction (numerator denominator) :centering t)
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
              (values (floor w 2) (- center y))))))

;;; finite sum expressions
(define-basic-math-output (finite-sum (summands operators))
    (setf summands
          (mapcar (math-output-to-record stream)
                  (summands finite-sum))
          operators
          (mapcar (princ-to-stream stream 'operator-math-output-record)
                  (operators finite-sum)))
    (align-output-records (insert-operators summands operators)
                          #'stacking-align
                          #'centering-align))

(defmethod initialize-instance :after ((sum finite-sum) &key)
  (unless (typep sum 'math-output-record)
    (with-slots (summands operators) sum
      (cond ((= (length summands) (+ 1 (length operators))))
            ((null operators)
             (setf operators (mapcar (constantly '+) (rest summands))))
            (t (error "finite-sum: length of summands (~A) and operators (~A) do not match."
                      (length summands) (length operators)))))))


;;; finite product expressions
(define-basic-math-output (finite-product (factors))
  (setf factors (mapcar (math-output-to-record stream)
                        (factors finite-product)))
  (align-output-records factors
                        #'stacking-align
                        #'centering-align))

