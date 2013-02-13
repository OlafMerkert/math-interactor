(in-package :math-interactor)

;;; some tests and examples of the math-interactor

(define-math-interactor-command (com-output-test :menu t :name "Ausgabe testen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (format stream "~&Hallo~%")))

(define-math-interactor-command (com-fraction :menu t :name "Bruch anzeigen")
    ()
  (let ((numer 3182)
        (denom 326)
        (stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output (make-instance 'fraction
                                                        :denominator denom
                                                        :numerator numer) stream)
                            :line-break t)
    (stream-replay stream)))

(define-math-interactor-command (com-sum :menu t :name "Summe anzeigen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output (make-instance 'finite-sum
                                                        :summands (list 1 2 3 4 5 6
                                                                        (make-instance 'fraction :numerator 17 :denominator 1329846)))
                                         stream)
                            :line-break t)
    (stream-replay stream)))

(define-math-interactor-command (com-cf :menu t :name "Kettenbruch anzeigen")
    ()
  (let ((stream (get-frame-pane *application-frame* 'app)))
    (stream-add-math-output stream
                            (math-output (make-instance 'finite-continued-fraction
                                                        :partial-quotients (list 1 2 3 4 5 6 7))
                                         stream)
                            :line-break t)
    (stream-replay stream)))
