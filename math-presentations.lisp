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
(define-presentation-type-abbreviation math-object ()
  '(or
    integer
    rational
    number
    gm:generic-math-object))

(define-presentation-type-abbreviation poly/series ()
  '(or polynomials:polynomial power-series:power-series))

;;; continued fractions
(define-presentation-type cf-ps:continued-fraction ())
(define-presentation-type cf-ps:quadratic-continued-fraction () :inherit-from 'cf-ps:continued-fraction)
(define-presentation-type cf-ps:sqrt-continued-fraction () :inherit-from 'cf-ps:continued-fraction)
#|(define-presentation-type cf-ps:alternative-continued-fraction () :inherit-from 'cf-ps:continued-fraction)|#

;;; anything we want to interact with
(define-presentation-type-abbreviation interactive-object ()
  '(or math-object
    cf-ps:continued-fraction))


;;; passing complicated objects through the interactive dialog
(defvar math-object-store (make-hash-table :test 'equal))

(defun get-string-representation (math-object)
  (subseq
   (with-output-to-string (stream)
     (print-unreadable-object (math-object stream :type t :identity t)))
   1)) ; discard the #

(defun from-store (string-repr)
  (gethash string-repr math-object-store))

(defun to-store (math-object)
  (let ((repr (get-string-representation math-object)))
    (gethash/c repr math-object-store math-object)
    repr))

(bind-multi ((object-type gm:generic-math-object cf-ps:continued-fraction))
  (define-presentation-method present (object (type object-type) stream (view textual-dialog-view) &key)
    (princ (to-store object) stream))

  (define-presentation-method accept ((type object-type) stream (view textual-dialog-view) &key)
    (from-store (read-line stream))))

