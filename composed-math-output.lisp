(in-package :math-interactor)

;;; output of finite continued fractions


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
       (finite-sum
        (list (first #1#)
              (fraction
               1
               (make-instance
                'finite-continued-fraction
                :partial-quotients (rest #1#))))))
   stream))

(defun finite-continued-fraction (partial-quotients)
  (make-instance 'finite-continued-fraction :partial-quotients partial-quotients))

(defclass finite-tuple ()
  ((coordinates :initarg :coordinates
                :initform nil
                :accessor coordinates)))

(defmethod math-output ((finite-tuple finite-tuple) stream)
  (math-output
   (parens (finite-sum (coordinates finite-tuple)
                       (n-copies (- (length (coordinates finite-tuple)) 1)
                                 #\,)))
   stream))

(defun finite-tuple (coordinates)
  (make-instance 'finite-tuple :coordinates coordinates))

(defun tuple (&rest coordinates)
  (finite-tuple coordinates))
