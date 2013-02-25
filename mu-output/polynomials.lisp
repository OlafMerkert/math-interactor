(in-package :generic-math-output-implementation)

;; create a nice output for polynomials (and series at the same
;; time)

;; essentially, we just need to implement the interface from
;; polynomial-series-printing, we are of course leaking abstractions
;; badly

(defclass mo-printer ()
  ((incoming-stack :initarg :incoming-stack
         :initform nil
         :accessor incoming-stack)
   (incoming-mode :initarg :incoming-mode
         :initform nil
         :accessor incoming-mode)
   (incoming-count :initarg :incoming-count
         :initform nil
         :accessor incoming-count)))

(defmethod print-number      ((printer mo-printer) number)
  (push (math-output-prepare number)
        (incoming-stack printer)))

(defmethod print-superscript ((printer mo-printer) base exponent)
  (push (superscript base exponent) (incoming-stack printer)))

(defmethod print-variable    ((printer mo-printer) variable)
  (push variable (incoming-stack printer)))

(defmethod print-spacer      ((printer mo-printer))
  (cond ((eq :product (car (incoming-mode printer)))
         (incf (first (incoming-count printer))))
        (t
         (push 2 (incoming-count printer)) ; we start off with two factors.
         (push :product (incoming-mode printer)))))

(defmethod print-operator    ((printer mo-printer) operator)
  (assert (member operator '(+ -)))
  (cond ((eq :sum (car (incoming-mode printer)))
         (push operator (incoming-stack printer))
         (incf (car (incoming-count printer)) 2))
        ((eq :product (car (incoming-mode printer)))
         (finish-product printer)
         ;; recurse
         (print-operator printer operator))
        (t ;; start a new sum
         (push operator (incoming-stack printer))
         (push 3 (incoming-count printer)) ; we start off with two
                                ; summands and one operator
         (push :sum (incoming-mode printer)))))


(defmethod print-ellipsis    ((printer mo-printer))
  (push "..." (incoming-stack printer)))

(defun finish-product (printer)
  (unless (eq :product (pop (incoming-mode printer)))
    (error "Illegal call to FINISH-PRODUCT."))
  (let* ((factors (pop (incoming-count printer)))
         (factors (reverse (popn factors (incoming-stack printer)))))
    (push (finite-product  factors)
          (incoming-stack printer))))

(defun finish-sum (printer)
  (unless (eq :sum (pop (incoming-mode printer)))
    (error "Illegal call to FINISH-SUM."))
  (let* ((summands+operators (pop (incoming-count printer)))
         (summands+operators (reverse (popn summands+operators (incoming-stack printer)))))
    (multiple-value-bind (summands operators) (splitn summands+operators 2)
      (push (finite-sum summands operators)
            (incoming-stack printer)))))

(defun finish (printer)
  (case (car (incoming-mode printer))
    (:product (finish-product printer)
              (finish printer))
    (:sum (finish-sum printer)
          (finish printer))))


(def-math-output-prepare (polynomial)
  (let ((printer (make-instance 'mo-printer)))
    (print-polynomial printer polynomial)
    (finish printer)
    ;; FIXME problem with zero polynomial
    (assert (length=1 (incoming-stack printer)))
    (first (incoming-stack printer))))

;; todo what about presentations?

(def-math-output-prepare (power-series)
  (let ((printer (make-instance 'mo-printer)))
    (print-power-series printer power-series)
    (finish printer)
    (assert (length=1 (incoming-stack printer)))
    (first (incoming-stack printer))))

;; TODO command history is not working properly
;; TODO the minus is a little small atm

;; presentations
(def-mo-pres-type polynomial)
(def-mo-pres-type power-series)

;; custom commands for polys and power series
(def-gm-method degree polynomials:degree
  1 (or polynomial-presentation power-series-presentation))

(def-gm-method leading-coefficient polynomials:leading-coefficient
  1  (or polynomial-presentation power-series-presentation))

(def-gm-method truncate power-series:series-truncate 1 power-series-presentation)
(def-gm-method remainder power-series:series-remainder 1 power-series-presentation)

