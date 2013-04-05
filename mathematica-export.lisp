(defpackage :mathematica-export
  (:use :cl :ol :iterate )
  (:export))

(in-package :mathematica-export)

(defgeneric mathematica-export (math-object stream)
  (:documentation "Generate a string that can be pasted into
  Mathematica directly."))

(defmethod mathematica-export ((obj number) stream)
  (format stream "~A" obj))

(defmethod mathematica-export ((obj string) stream)
  (format stream "~A" obj))

(defmethod mathematica-export ((obj symbol) stream)
  (format stream "~A" obj))

(defmethod mathematica-export (obj stream)
  (mathematica-export (mi:math-output-prepare obj) stream))

(defmethod mathematica-export ((obj mi:fraction) stream)
  (princ "(" stream) 
  (mathematica-export (mi:numerator obj) stream)
  (princ ")/(" stream)
  (mathematica-export (mi:denominator obj) stream)
  (princ ")" stream))

(defmethod mathematica-export ((obj mi:superscript) stream)
  (princ "(" stream) 
  (mathematica-export (mi:base obj) stream)
  (princ ")^(" stream)
  (mathematica-export (mi:exponent obj) stream)
  (princ ")" stream))

(defmethod mathematica-export ((obj mi:subscript) stream)
  (mathematica-export (mi:base obj) stream))

(defmethod mathematica-export ((obj mi:finite-sum) stream)
  (if (null (mi:summands obj))
      (princ "0" stream)
      (mapc (lambda (x) (mathematica-export x stream))
            (mi:insert-operators (mi:summands obj) (mi:operators obj)))))

(defmethod mathematica-export ((obj mi:finite-product) stream)
  (if (null (mi:factors obj))
      (princ "1" stream)
      (mapc (lambda (x) (mathematica-export x stream))
            (splice-in " " (mi:factors obj)))))

(mi:define-math-interactor-command (com-mathematica-export :name t :menu t)
    ((math-object 'mi:math-object-presentation))
  (let ((stream
         (clim:get-frame-pane clim:*application-frame* 'mi::int)))
    (clim:with-text-family (stream :fix)
      (fresh-line stream)
      (mathematica-export math-object stream)
      (fresh-line stream))))
