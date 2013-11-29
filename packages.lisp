(in-package :cl-user)

(defpackage :math-interactor
  (:nicknames :mi)
  (:use :clim-lisp :clim :ol :iterate
        :formulas)
  (:import-from :infinite-sequence #:sref)
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
   #:center
   #:coordinates
   #:finite-tuple
   #:tuple
   #:interactive-object
   #:math-object
   #:poly/series))

(defpackage :render-to-clim
  (:nicknames :rtc)
  (:use :clim :clim-lisp :ol :iterate)
  (:export
   #:render
   #:advance-cursor
   #:reset-cursor
   ))

(defpackage :generic-math-output-implementation
  (:nicknames :gmo)
  (:shadowing-import-from :math-interactor :numerator :denominator)
  (:shadow :finish)
  (:use :clim-lisp :clim :ol :iterate
        :infinite-math
        :math-interactor
        :polynomials
        :power-series)
  (:export
   #:math-output-prepare
   #:*integer-output-mode*
   #:formula-with-math-objects
   #:_
   #:^
   #:formula-prepare))
