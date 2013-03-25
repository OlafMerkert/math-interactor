(in-package :generic-math-output-implementation)

(define-math-interactor-command (com-create-alternative-cf :name "Alternative CF expansion")
    ((poly 'polynomial-presentation))
  (let* ((radicand (gm:sqrt poly))
         (cf (make-instance 'cf-ps-a:alternative-continued-fraction
                            :starting radicand)))
    (princ "D = ")
    (put-result poly)
    (princ "(sqrt D) = ")
    (put-result radicand)
    (princ "(cf D) = ")
    (put-result cf)))
