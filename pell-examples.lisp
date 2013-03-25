(defpackage :pell-examples
  (:nicknames :pell)
  (:shadowing-import-from :math-interactor :numerator :denominator :fraction)
  (:use :clim :clim-lisp
        :ol :iterate
        :math-interactor
        :polynomials
        :fractions)
  (:export))

(in-package :pell-examples)

(defparameter examples
  (mapcar
   (lambda (x) (apply #'make-polynomial x))
   '(
     ;; degree 2
     (1 0 1)
     (1 0 -1)
     ;; degree 4
     ( 1 0 0 1 1)
     (1 -8 -42 424 -119)             ; self computed, with order 9
     (1 14 393 -184 1072)            ; self computed, with order 10
     ;; degree 6
     (1 0 0 0 0 1 1) 
     (1 94 10113 8608 14464)          ; self computed times
                                        ; square factor
     (4 -60 249 -192 90 -36 9)       ; Leprévost
     ;; random stuff
     (1 0 7 8 9 0 5)
     )))

(defun load-examples-in-bin ()
  (dolist (ex examples)
    (put-result ex t))
  (dolist (ex other-examples)
    (put-result ex t)))

(add-mi-hook 'load-examples-in-bin)

(defun poly-helper (coeff-table)
  (make-instance
   'polynomial
   :coefficients (map 'vector
                      (lambda (coeffs)
                        (frac (make-instance
                               'polynomial :var 'u
                               :coefficients (list->array (mklist coeffs)))))
                      coeff-table)))

(defparameter other-examples
  (mapcar
   #'poly-helper
   '((1 0 0 1 (1 0))
     (1 0 0 0 0 1 (1 0)))))


