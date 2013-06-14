(in-package :math-interactor)

;;; math types from math-utils
(define-presentation-type gm:generic-math-object ())

(define-presentation-type polynomials:polynomial () :inherit-from 'gm:generic-math-object)
(define-presentation-type fractions:fraction () :inherit-from 'gm:generic-math-object)
(define-presentation-type power-series:power-series () :inherit-from 'gm:generic-math-object)
(define-presentation-type power-series:constant-series () :inherit-from 'power-series:power-series)
(define-presentation-type finite-fields:integer-mod () :inherit-from 'gm:generic-math-object)
(define-presentation-type ec-ws:point-2 () :inherit-from 'gm:generic-math-object)
(define-presentation-type ec-ws:ec-point-ws () :inherit-from 'ec-ws:point-2)
(define-presentation-type ec-ws:ec-point-infinity () :inherit-from 'gm:generic-math-object)

;;; combined types
(define-presentation-type math-object ()
  :inherit-from '(or gm:generic-math-object
                  number
                  integer
                  rational))

(define-presentation-type poly/series ()
  :inherit-from '(or polynomials:polynomial power-series:power-series))

;;; continued fractions
(define-presentation-type cf-ps:continued-fraction ())
(define-presentation-type cf-ps:quadratic-continued-fraction () :inherit-from 'cf-ps:continued-fraction)
(define-presentation-type cf-ps:sqrt-continued-fraction () :inherit-from 'cf-ps:continued-fraction)
#|(define-presentation-type cf-ps:alternative-continued-fraction () :inherit-from 'cf-ps:continued-fraction)|#
