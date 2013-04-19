(in-package :generic-math-output-implementation)

(define-math-interactor-command (com-create-alternative-cf :name "Alternative CF expansion")
    ((poly 'polynomial-presentation))
  (let* ((radicand (gm:sqrt poly))
         (cf (make-instance 'cf-ps-a:alternative-continued-fraction
                            :starting radicand)))
    (put-result/formula (poly) `(= D ,poly))
    (put-result/formula (radicand) `(= (sqrt D) ,radicand))
    (put-result/formula (cf) `(= (cf D) ,cf))))
