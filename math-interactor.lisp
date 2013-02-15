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

(in-package :math-interactor)

(define-application-frame math-interactor ()
  ()
  (:panes (app :application
               :width 700 :height 600
               :incremental-redisplay t)
          (int :interactor
               :width 500 :height 200)
          (bin :application
               :width 300 :height 500
               :incremental-redisplay t))
  (:layouts (default
                (horizontally ()
                  bin
                  (vertically ()
                    app int)))))

(defun math-interactor ()
  (run-frame-top-level (make-instance 'math-interactor)))

;; define a command to exit
(define-math-interactor-command (com-quit :menu t :name "Beenden") ()
  (frame-exit *application-frame*))
