(in-package :generic-math-output-implementation)

;; also make rational stuff look nicer

(defparameter *integer-output-mode* nil
  "possible values are NIL for default output, T for factorised
  output, or a prime integer P for splitting off it's powers.")

(defun prepare-extract-p (rational p)
  (multiple-value-bind (order remfactors) (nt:ord-p p rational)
    (let ((*integer-output-mode* nil))
     (if (zerop order)
         (math-output-prepare remfactors)
         (finite-product (list (superscript p order)
                               (math-output-prepare remfactors)))))))

(defun prepare-factorised (integer)
  (if (= 1 integer)
      integer
      (finite-product
       (mapcar (lambda (x)
                 (if (consp x)
                     (superscript (car x) (cdr x))
                     x))
               (nt:factorise integer)))))




