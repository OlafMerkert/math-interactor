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
   #:math-output-prepare
   #:def-gm-method
   #:math-object-presentation
   #:put-result
   #:stream-add-math-output
   #:define-math-interactor-command
   #:add-mi-hook
   #:insert-operators
   #:def-gm-method%
   #:explicit-presentation
   #:math-object-presentation-table
   #:math-interactor
   #:basic-math-output-p
   #:_
   #:^
   #:formula-with-math-objects
   #:formula-prepare
   #:put-result/formula
   #:as-int
   #:parens
   #:left
   #:right
   #:center))

(defpackage :generic-math-output-implementation
  (:nicknames :gmo)
  (:shadowing-import-from :math-interactor :numerator :denominator)
  (:shadow :finish)
  (:use :clim-lisp :clim :ol :iterate
        :infinite-math
        :math-interactor
        :polynomial-series-printing
        :polynomials
        :power-series)
  (:export
   #:math-output-prepare
   #:*integer-output-mode*
   #:formula-with-math-objects
   #:_
   #:^
   #:formula-prepare))
