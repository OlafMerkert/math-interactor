(in-package :math-interactor)

(define-presentation-type gm:generic-math-object ())

(define-presentation-type polynomials:polynomial () :inherit-from 'gm:generic-math-object)
(define-presentation-type fractions:fraction () :inherit-from 'gm:generic-math-object)
(define-presentation-type power-series:power-series () :inherit-from 'gm:generic-math-object)
(define-presentation-type power-series:constant-series () :inherit-from 'power-series:power-series)
(define-presentation-type finite-fields:integer-mod () :inherit-from 'gm:generic-math-object)
(define-presentation-type ec-ws:point-2 () :inherit-from 'gm:generic-math-object)
(define-presentation-type ec-ws:ec-point-ws () :inherit-from 'ec-ws:point-2)
(define-presentation-type ec-ws:ec-point-infinity () :inherit-from 'gm:generic-math-object)

(define-presentation-type math-object ()
  :inherit-from '(or gm:generic-math-object
                  number
                  integer
                  rational))

(define-presentation-type poly/series ()
  :inherit-from '(or polynomials:polynomial power-series:power-series))
