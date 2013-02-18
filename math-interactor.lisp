(in-package :math-interactor)

(define-application-frame math-interactor ()
  ()
  (:panes (app :application
               :width 700 :height 600
               :incremental-redisplay t)
          (int :interactor
               :width 500 :height 200)
          (bin :application
               :width 300 :height 500
               :incremental-redisplay t))
  (:layouts (default
                (horizontally ()
                  bin
                  (vertically ()
                    app int)))))

(defun math-interactor ()
  (run-frame-top-level (make-instance 'math-interactor)))

;; define a command to exit
(define-math-interactor-command (com-quit :menu t :name "Beenden") ()
  (frame-exit *application-frame*))
