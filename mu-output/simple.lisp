(in-package :generic-math-output-implementation)

;; output of various very simple data-structures

;; also make rational stuff look nicer

(defmethod math-output-prepare ((integer integer)) integer)

(def-math-output-prepare (rational)
  (fraction (cl:numerator rational) (cl:denominator rational)))

(def-math-output-prepare (finite-fields:integer-mod)
  (superscript (finite-fields:remainder)
               ;; TODO add some brackets
               (finite-fields:modulus)))

;; now pack everything up in presentations.

(defmacro def-mo-pres-type (mo-type)
  `(progn
     (define-presentation-type ,(symb mo-type '-presentation) ()
       :inherit-from '(and math-object-presentation ,mo-type))
     (defmethod math-object-presentation ((,mo-type ,mo-type))
       ',(symb mo-type '-presentation))))

(def-mo-pres-type number)

;; TODO perhaps here we want to profit from different views.
(define-presentation-method present (object (type math-object-presentation) stream view &key)
  (stream-add-math-output stream (math-output object stream)
                          :line-break t))
