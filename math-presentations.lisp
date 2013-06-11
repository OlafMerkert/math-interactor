(in-package :math-interactor)

(defmacro def-mo-pres-type (mo-type &optional description)
  (unless description
    (setf description
          (string-downcase
           (substitute #\space #\-
                       (symbol-name mo-type)))))
  `(progn
     (define-presentation-type ,(symb mo-type '-presentation) ()
       :inherit-from '(and math-object-presentation ,mo-type)
       :description ,description)
     (ew (setf (gethash ',mo-type math-object-presentation-table)
               ',(symb mo-type '-presentation)))
     (defmethod math-object-presentation ((,mo-type ,mo-type))
       ',(symb mo-type '-presentation))))

(def-mo-pres-type number)
(ew (setf (gethash 'integer math-object-presentation-table) 'number-presentation
          (gethash 'rational math-object-presentation-table) 'number-presentation))

;; presentations
(def-mo-pres-type finite-fields:integer-mod)

(def-mo-pres-type polynomial)
(def-mo-pres-type power-series)

(def-mo-pres-type ec-ws:ec-point-ws)

(def-mo-pres-type fractions:fraction)

;; cont frac
(def-mo-pres-type cf-ps:continued-fraction)


;;;  output from applying valuations on math object

(defmethod math-object-presentation
    ((valuations-coeff:polynomial-values
      valuations-coeff:polynomial-values))
  'valuations-coeff:polynomial-values)

(defmethod math-object-presentation
    ((valuations-coeff:power-series-values
      valuations-coeff:power-series-values))
  'valuations-coeff:power-series-values)

