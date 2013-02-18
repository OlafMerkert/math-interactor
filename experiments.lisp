(in-package :math-interactor)

;;; some tests and examples of the math-interactor

(define-math-interactor-command (com-output-test :menu t :name "Ausgabe testen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (format stream "~&Hallo~%")))

(defun define-example% (math-object)
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output math-object stream)
                            :line-break t)
    (stream-replay stream)))

(defmacro! define-example (name math-object-spec)
  `(define-math-interactor-command (,(symb 'com- (string-upcase name)) :menu t :name ,name)
       ()
     (define-example% ,math-object-spec)))


;; (define-example "Bruch anzeigen"
;;     (fraction 1231
;;               987))

;; (define-example "Summe anzeigen"
;;     (finite-sum (list 1 2 3 4 5 6
;;                       (fraction 17 1329846))))

;; (define-example  "Kettenbruch anzeigen"
;;     (finite-continued-fraction
;;      (list 1 2 3 4 5 6 7)))

;; (define-example "Produkt anzeigen"
;;     (finite-product (list 78 29 13)))

;; (define-example "Potenz anzeigen"
;;     (finite-sum (list 8 (superscript 99 3)
;;                       (fraction 8 10)
;;                       (superscript (fraction 7 3) 20))))

;; (define-example "Index anzeigen"
;;     (subscript 99 3))

(define-example "Polynom anzeigen"
    (generic-math-output-implementation:math-output-prepare
     (polynomials:make-polynomial 1 -2 3 -4 5)))

(define-example "Polynom 2 anzeigen"
    (generic-math-output-implementation:math-output-prepare
     (polynomials:make-polynomial 1 0 -2 3 0 -4 5)))

 (define-example "Potenzreihe (statisch) anzeigen"
    (generic-math-output-implementation:math-output-prepare
     (power-series:make-power-series 12 1 -2 3 -4 5 32 78 0 1 7 3 81 83 1 7 89 8 7 1 7 88 8)))

(define-example "Potenzreihe (dynamisch) anzeigen"
    (generic-math-output-implementation:math-output-prepare
     (gm:sqrt (polynomials:make-polynomial 4 2 1 8 2 ))))

(define-example "Kettenbruch anzeigen"
    (generic-math-output-implementation:math-output-prepare
     (make-instance 'cf-ps:continued-fraction :starting (gm:sqrt (polynomials:make-polynomial 1 0 0 1 1)))))
;; TODO figure out line breaking and horizontal scrolling
