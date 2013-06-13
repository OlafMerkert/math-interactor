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

(define-math-interactor-command (com-order-p :name "order of p")
    ((math-object 'math-object) (p 'integer :default 3 :prompt "prime"))
  (assert (nt:prime-p p))
  (multiple-value-bind (bound comment)
      (vv:valuate-exp p math-object)
    (put-result/formula (bound)
     `(= (_ ord ,p) ,bound))
    (when (eq comment :unbounded)
      (format (get-frame-pane *application-frame* 'app) " unbounded~%"))))

(define-math-interactor-command (com-valuate-coeff :name "Valuate coefficientwise")
    ((math-object 'math-object)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result (vc:valuate-exp valuation math-object)))

(define-math-interactor-command (com-valuate :name "Valuate")
    ((math-object 'math-object)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result (vv:valuate-exp valuation math-object)))

;;; polynomials and power series
(def-gm-method% degree polynomials:degree
  1 (or polynomials:polynomial power-series:power-series))

(def-gm-method% leading-coefficient polynomials:leading-coefficient
  1  (or polynomial-presentation power-series-presentation))

(def-gm-method% truncate power-series:series-truncate 1 power-series-presentation)
(def-gm-method% remainder power-series:series-remainder 1 power-series-presentation)
