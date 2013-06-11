(in-package :generic-math-output-implementation)

;; create a nice output for polynomials (and series at the same
;; time)

(implement-printer-method (print-math-object math-output-printer) (object)
  (math-output-prepare object))

(implement-printer-method (print-superscript math-output-printer) (base exponent)
  ;; todo may need to call math-output-prepare on these
  (superscript (math-output-prepare base) exponent))

(implement-printer-method (print-product math-output-printer) (factors)
  (finite-product factors))

(implement-printer-method (print-sum math-output-printer) (summands-with-sign)
  (finite-sum (mapcar #'first summands-with-sign)
              (rest (mapcar #'second summands-with-sign))))

(implement-printer-method (print-sum+ellipsis math-output-printer) (summands-with-sign)
  (finite-sum (append1 (mapcar #'first summands-with-sign) "...")
              (append1 (rest (mapcar #'second summands-with-sign)) '+)))


;; presentations
(def-mo-pres-type polynomial)
(def-mo-pres-type power-series)



;; TODO command history is not working properly
;; TODO the minus is a little small atm


;;; TODO output from applying valuations on math object

(defmethod math-object-presentation
    ((valuations-coeff:polynomial-values
      valuations-coeff:polynomial-values))
  'valuations-coeff:polynomial-values)

(defmethod math-object-presentation
    ((valuations-coeff:power-series-values
      valuations-coeff:power-series-values))
  'valuations-coeff:power-series-values)

(define-presentation-method present (object (type vc:polynomial-values) stream view &key)
  (stream-add-math-output stream (math-output object stream)
                          :line-break t))

(define-presentation-method present (object (type vc:power-series-values) stream view &key)
  (stream-add-math-output stream (math-output object stream)
                          :line-break t))
