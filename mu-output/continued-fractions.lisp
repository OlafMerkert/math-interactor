(in-package :generic-math-output-implementation)

(defparameter *continued-fraction-display-length* 5)

(def-math-output-prepare (cf-ps:continued-fraction)
  (finite-continued-fraction
   (append1
    (map 'list
         #'math-output-prepare
         (lazy-array-take (cf-ps:partial-quotients cf-ps:continued-fraction)
                          *continued-fraction-display-length*
                          nil))
    "...")))

(def-mo-pres-type cf-ps:continued-fraction)

;; provide error messages if we don't have a suitable gm:xxx method
;; for the given object

(define-math-interactor-command (com-create-cf :name "CF expansion" :menu t)
    ((poly 'polynomial-presentation))
  (let ((cf (make-instance 'cf-ps:sqrt-continued-fraction
                           :radicand poly)))
    (put-result/formula (poly) `(= D ,poly))
    (put-result/formula ((sqrt (cf-ps:starting cf)))
                        `(= (sqrt D) ,sqrt))
    (put-result/formula (cf)
                        `(= (cf D) ,cf))))

(define-math-interactor-command (com-check-quasi-period :name "Find (quasi)period" :menu t)
    ((cf 'continued-fraction-presentation)
     (bound 'integer :default 40 :prompt "bound"))
  (let ((stream (get-frame-pane *application-frame* 'mi::app)))
   (multiple-value-bind (period-length sn)
       (cf-ps:find-pure-quasiperiod-length cf :length-bound bound)
     (cond ((not period-length)
            (format stream "~&No quasiperiod up to ~A found.~%" bound))
           ((gm:one-p sn)
            (format stream "~&Period length: ~A~%" period-length))
           (t
            (format stream "~&Quasi-period length: ~A   Period length: ~A~%"
                    period-length (* 2 period-length)))))))

(define-math-interactor-command (com-continuants :name "Show continuants" :menu t)
    ((cf 'continued-fraction-presentation) (index 'integer :prompt "n"))
  (cf-ps:with-cf2 cf
    (let ((p (lazy-aref cf-ps:pn index))
          (q (lazy-aref cf-ps:qn index)))
      ;; TODO typeset equation with symbols on the left.
      (put-result/formula (p) `(= p ,p))
      (put-result/formula (q) `(= q ,q))
      (put-result/formula ((pell (gm:- (gm:expt p 2) (gm:* cf-ps:d q q))))
                          `(= (- (^ p 2) (* D (^ q 2))) ,pell)))))

;; partial and complete quotients
(define-math-interactor-command (com-list-partial-quotients :name "Partial quotients")
    ((cf 'continued-fraction-presentation) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf2 cf
    (iter (for index from start to end)
          (put-result/formula ((an (lazy-aref cf-ps:an index)))
                              `(= (_ a ,index) ,an)))))

(define-math-interactor-command (com-list-complete-quotients-sqrt :name "Complete Sqrt quotients")
    ((cf 'continued-fraction-presentation) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf2 cf
    (iter (for index from start to end)
          (put-result/formula ((rn (lazy-aref cf-ps:rn index)))
                              `(= (_ r ,index) ,rn))
          (put-result/formula ((sn (lazy-aref cf-ps:sn index)))
                              `(= (_ s ,index) ,sn)))))

(define-math-interactor-command (com-list-complete-quotients :name "Complete quotients")
    ((cf 'continued-fraction-presentation) (start 'integer :default 0 :prompt "start") (end 'integer :prompt "end"))
  (cf-ps:with-cf2 cf
    (iter (for index from start to end)
          (put-result/formula ((alphan (lazy-aref cf-ps:alphan index)))
                              `(= (_ alpha ,index) ,alphan)))))

;; integration formula
(define-math-interactor-command (com-integration-formula :name "Integration formula")
    ((cf 'continued-fraction-presentation) (index 'integer :prompt "n"))
  (cf-ps:with-cf2 cf
    (let ((p (lazy-aref cf-ps:pn index))
          (q (lazy-aref cf-ps:qn index)))
      (put-result/formula ((f (if (gm:zero-p q) 0
                                  (gm:/ (derivative p) q))))
                          `(= f ,f)))))

;; check for torsion point
(define-math-interactor-command (com-check-torsion :name "Check torsion")
    ((object '(or continued-fraction-presentation
               polynomial-presentation)
             :prompt "polynomial or continued fraction"))
  (let ((stream (get-frame-pane *application-frame* 'mi::app)))
    (multiple-value-bind (order point curve) (cf-ps:check-torsion-divisor object)
      ;; this stuff might be interesting to some people
      ;; TODO use formula output
      ;; TODO figure out how to mix text and math stuff
      (format stream "~&Point (~A, ~A) on Y^2 = X^3 + ~A X + ~A~%"
              (ec-ws:x point) (ec-ws:y point)
              (ec-ws:ws-a curve) (ec-ws:ws-b curve))
      (if order
          (format stream "~&[O_1] - [O_2] has finite order ~A.~%" order)
          (format stream "~&[O_1] - [O_2] is not torsion.~%")))))

;;; TODO allow exporting the app view of the math-interactor to TeX or something.
;;; TODO replace format calls wiht put-result/formula, even for text
