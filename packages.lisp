(in-package :cl-user)

(defpackage :math-interactor
  (:nicknames :mi)
  (:use :clim-lisp :clim :ol :iterate)
  (:shadow :numerator :denominator :princ-to-stream)
  (:export
   #:finite-sum
   #:fraction
   #:finite-continued-fraction
   #:numerator
   #:denominator
   #:summands
   #:operators
   #:finite-product
   #:factors
   #:partial-quotients
   #:subscript
   #:superscript
   #:base
   #:index
   #:exponent
   #:math-output
   #:math-output-prepare))

(defpackage :generic-math-output-implementation
  (:nicknames :gmo)
  (:shadowing-import-from :math-interactor :numerator :denominator)
  (:shadow :finish)
  (:use :cl :ol :iterate :math-interactor
        :polynomial-series-printing
        :polynomials
        :power-series)
  (:export
   #:math-output-prepare))
