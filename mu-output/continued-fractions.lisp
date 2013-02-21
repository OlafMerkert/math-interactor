(in-package :generic-math-output-implementation)

(defparameter *continued-fraction-display-length* 10)

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
    (put-result (cf-ps:starting cf))
    (put-result cf)))

(define-math-interactor-command (com-check-quasi-period :name "Find (quasi)period" :menu t)
    ((cf 'continued-fraction-presentation)
     (bound 'integer :default 40))
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
    ((cf 'continued-fraction-presentation) (index 'integer))
  (cf-ps:with-cf2 cf
    (let ((p (lazy-aref cf-ps:pn index))
          (q (lazy-aref cf-ps:qn index)))
      (put-result p)
      (put-result q)
      (put-result (gm:- (gm:expt p 2) (gm:* cf-ps:d q q))))))

