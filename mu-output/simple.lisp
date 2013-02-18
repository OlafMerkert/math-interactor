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
