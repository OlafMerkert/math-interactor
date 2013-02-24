(defpackage :pell-examples
  (:nicknames :pell)
  (:shadowing-import-from :math-interactor :numerator :denominator)
  (:use :clim :clim-lisp
        :ol :iterate
        :math-interactor
        :polynomials)
  (:export))

(in-package :pell-examples)

(defparameter examples
  (list (make-polynomial 1 -8 -42 424 -119)
        (make-polynomial 1 0 0 0 0 1 1) 
        (make-polynomial 1 14 393 -184 1072)
        (make-polynomial 1 94 10113 8608 14464)))

(defun load-examples-in-bin ()
  (dolist (ex examples)
    (put-result ex t)))

(add-mi-hook 'load-examples-in-bin)
