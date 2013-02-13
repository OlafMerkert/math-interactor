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
