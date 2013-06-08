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

(def-math-output-prepare (polynomial)
  (let ((*current-printer* 'math-output-printer))
    (format-polynomial polynomial)))

(def-math-output-prepare (power-series)
  (let ((*current-printer* 'math-output-printer))
    (format-power-series power-series)))

;; TODO command history is not working properly
;; TODO the minus is a little small atm

;; custom commands for polys and power series
(def-gm-method% degree polynomials:degree
  1 (or polynomial-presentation power-series-presentation))

(def-gm-method% leading-coefficient polynomials:leading-coefficient
  1  (or polynomial-presentation power-series-presentation))

(def-gm-method% truncate power-series:series-truncate 1 power-series-presentation)
(def-gm-method% remainder power-series:series-remainder 1 power-series-presentation)

;;; TODO output from applying valuations on math object
(def-math-output-prepare (vc:polynomial-values)
  (let ((*current-printer* 'math-output-printer))
    (format-polynomial/all vc:polynomial-values)))

(def-math-output-prepare (vc:power-series-values)
  (let ((*current-printer* 'math-output-printer))
    (format-power-series/all vc:power-series-values)))

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
