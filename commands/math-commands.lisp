(in-package :math-interactor)

;;; helper macros to define wrappers around generic math ops
(defmacro def-gm-method% (com-name op-name
                         &optional (num-args 1)
                                   (presentation-type 'math-object))
  `(def-gm-method ,com-name ,op-name ,@(n-copies num-args presentation-type)))

(defmacro def-gm-method (com-name op-name &rest parameter-types)
  (let ((arguments (list->gensyms :arg parameter-types)))
    `(define-math-interactor-command (,(symb 'com- com-name) :name t )
         ,(mapcar #2`(,a1 ',a2) arguments parameter-types)
       (put-result (,op-name ,@arguments)))))

;;; standard arithmetic
(def-gm-method% plus gm:+ 2)

(def-gm-method% times gm:* 2)

(def-gm-method% divide gm:/ 2)

;; for polynomials, first convert to a power series
(defun special-invert (object)
  (gm:/ (if (typep object 'polynomials:polynomial)
            (gm:-> 'power-series:power-series object)
            object)))

(def-gm-method% invert special-invert 1)

(def-gm-method% sqrt gm:sqrt 1)

(def-gm-method% minus gm:- 1)


(def-gm-method% ggt fractions:ggt 2)

;;; valuations
(define-math-interactor-command (com-reduce-modp :name "reduce mod p")
    ((math-object 'math-object) (p 'integer :default 3 :prompt "prime"))
  ;; check that p is prime
  (assert (nt:prime-p p))
  (put-result (gm:-> 'finite-fields:integer-mod math-object :mod p)))

(define-math-interactor-command (com-valuate-coeff :name "Valuate coefficientwise")
    ((math-object 'math-object)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result/formula ((o (vc:valuate-exp valuation math-object))
                       (v valuation))
                      `(= (_ ord ,v) ,o)))

(define-math-interactor-command (com-valuate :name "Valuate")
    ((math-object 'math-object)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result/formula ((o (vv:valuate-exp valuation math-object))
                       (v valuation))
                      `(= (_ ord ,v) ,o)))

;;; polynomials and power series
(def-gm-method% degree polynomials:degree
  1 poly/series)

(def-gm-method% leading-coefficient polynomials:leading-coefficient
  1 poly/series)

(def-gm-method% truncate power-series:series-truncate 1 power-series:power-series)
(def-gm-method% remainder power-series:series-remainder 1 power-series:power-series)
