(in-package :math-interactor)

;;; first the different variants of CF expansion
(define-math-interactor-command (com-create-cf :name "CF expansion")
    ((series 'power-series:power-series))
  (let ((cf (make-instance 'cf-ps:continued-fraction
                           :starting series)))
    (put-result/formula (series) `(= alpha ,series))
    (put-result/formula (cf) `(= (cf alpha) ,cf))))

(define-math-interactor-command (com-create-cf-sqrt :name "CF expansion SQRT")
    ((poly 'polynomials:polynomial))
  (let ((cf (make-instance 'cf-ps:sqrt-continued-fraction
                           :radicand poly)))
    (put-result/formula (poly) `(= D ,poly))
    (put-result/formula ((sqrt (cf-ps:starting cf)))
                        `(= (sqrt D) ,sqrt))
    (put-result/formula (cf)
                        `(= (cf (sqrt D)) ,cf))))

(define-math-interactor-command (com-create-cf-quadratic :name "CF expansion quadratic")
    ((poly 'polynomials:polynomial :prompt "d")
     (a 'polynomials:polynomial :prompt "a")
     (b 'polynomials:polynomial :prompt "b")
     (c 'polynomials:polynomial :prompt "c"))
  (let ((cf (make-instance 'cf-ps:quadratic-continued-fraction
                           :radicand poly
                           :a a :b b :c c)))
    (put-result/formula ((d poly) a b c (alpha0 (cf-ps:starting cf)))
                        `(= alpha
                            (/ (+ a (* b (sqrt d)))
                               c)
                            (/ (+ ,a (* ,b (sqrt ,d)))
                               ,c)
                            ,alpha0))
    (put-result/formula (cf)
                        `(= (cf alpha) ,cf))))

;; and finally a weird version of continued fractions
(define-math-interactor-command (com-create-alternative-cf :name "Alternative CF expansion")
    ((poly 'polynomials:polynomial))
  (let* ((radicand (gm:sqrt poly))
         (cf (make-instance 'cf-ps-a:alternative-continued-fraction
                            :starting radicand)))
    (put-result/formula (poly) `(= D ,poly))
    (put-result/formula (radicand) `(= (sqrt D) ,radicand))
    (put-result/formula (cf) `(= (cf D) ,cf))))

;;; now the operations we do on CFs

(define-math-interactor-command (com-check-quasi-period :name "Find (quasi)period")
    ((cf 'cf-ps:continued-fraction)
     (bound 'integer :default 40 :prompt "bound"))
  (multiple-value-bind (period-length sn)
      (cf-ps:find-pure-quasiperiod-length cf :length-bound bound)
    (cond ((not period-length)
           (put-result/formula (bound) `(> quasi-period-length ,bound)))
          ((gm:one-p sn)
           (put-result/formula (period-length) `(= period-length ,period-length)))
          (t
           (put-result/formula (period-length) `(= quasi-period-length ,period-length))
           (put-result/formula ((pl2 (* 2 period-length)))
                               `(= period-length ,pl2))))))

;;; partial and complete quotients
(define-math-interactor-command (com-list-partial-quotients :name "Partial quotients")
    ((cf 'cf-ps:continued-fraction) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf cf
    (iter (for index from start to end)
          (put-result/formula ((an (sref cf-ps:an index)))
                              `(= (_ a ,index) ,an)))))

(define-math-interactor-command (com-list-complete-quotients :name "Complete quotients")
    ((cf 'cf-ps:continued-fraction) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf cf
    (iter (for index from start to end)
          (put-result/formula ((alphan (sref cf-ps:alphan index)))
                              `(= (_ alpha ,index) ,alphan)))))

(define-math-interactor-command (com-list-complete-quotients-sqrt :name "Complete quotients SQRT")
    ((cf 'cf-ps:continued-fraction) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf2 cf
    (iter (for index from start to end)
          (put-result/formula ((rn (sref cf-ps:rn index)))
                              `(= (_ r ,index) ,rn))
          (put-result/formula ((sn (sref cf-ps:sn index)))
                              `(= (_ s ,index) ,sn)))))

;;; compute continuants and things
(define-math-interactor-command (com-continuants :name "Continuants")
    ((cf 'cf-ps:continued-fraction) (index 'integer :prompt "n+1"))
  (cf-ps:with-cf cf
    (decf index)
    (let ((p (sref cf-ps:pn index))
          (q (sref cf-ps:qn index)))
      ;; typeset equation with symbols on the left.
      (put-result/formula (p) `(= (_ p ,index) ,p))
      (put-result/formula (q) `(= (_ q ,index) ,q)))))

(define-math-interactor-command (com-continuants-sqrt :name "Continuants + Pell")
    ((cf 'cf-ps:continued-fraction) (index 'integer :prompt "n+1"))
  (cf-ps:with-cf2 cf
    (decf index)
    (let ((p (sref cf-ps:pn index))
          (q (sref cf-ps:qn index)))
      ;; typeset equation with symbols on the left.
      (put-result/formula (p) `(= (_ p ,index) ,p))
      (put-result/formula (q) `(= (_ q ,index) ,q))
      (put-result/formula ((pell (gm:- (gm:expt p 2) (gm:* cf-ps:d q q))))
                          `(= (- (^ p 2) (* D (^ q 2))) ,pell)))))

;; integration formula
(define-math-interactor-command (com-integration-formula :name "Integration formula")
    ((cf 'cf-ps:continued-fraction) (index 'integer :prompt "n"))
  (cf-ps:with-cf2 cf
    (let ((p (sref cf-ps:pn index))
          (q (sref cf-ps:qn index)))
      (put-result/formula ((f (if (gm:zero-p q) 0
                                  (gm:/ (polynomials:derivative p) q))))
                          `(= f ,f)))))


;; check for torsion point on associated elliptic curve for degree 4
(define-math-interactor-command (com-check-torsion :name "Check torsion")
    ((object '(or cf-ps:continued-fraction polynomials:polynomial)
             :prompt "polynomial or continued fraction"))
  (multiple-value-bind (order point curve) (cf-ps:check-torsion-divisor object)
    ;; this stuff might be interesting to some people
    (put-result/formula (point
                         (a (ec-ws:ws-a curve))
                         (b (ec-ws:ws-b curve)))
                        `(nil ,point " on "
                              (= (^ y 2) (+ (^ x 3) (* ,a x) ,b))))
    (put-result/formula ((order (if order order :infinity+)))
                        `(= (ord (- (_ O 1) (_ O 2)))
                            ,order))))
