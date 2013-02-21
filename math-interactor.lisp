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
                    app
                    int)))))

(defun math-interactor ()
  (run-frame-top-level (make-instance 'math-interactor)))

;; define a command to exit
(define-math-interactor-command (com-quit :menu t :name "Quit") ()
  (frame-exit *application-frame*))

;;; define commands for math-utils operations
(defun put-result (math-object &optional to-bin)
  ;; for now, just dump stuff to app output
  (let ((stream (get-frame-pane *application-frame* (if to-bin 'bin 'app))))
    (present math-object (math-object-presentation math-object)
             :stream stream)
    (stream-replay stream)))

(define-math-interactor-command (com-put-to-bin :menu t :name "Save object to bin")
    ((object 'math-object-presentation))
  (put-result object t))

;; start off with the generic stuff.
(define-presentation-type math-object-presentation ())

(defgeneric math-object-presentation (object))

(defmacro def-gm-method (com-name op-name
                         &optional (num-args 1)
                                   (presentation-type 'math-object-presentation))
  (let ((arguments (iter (for i from 1 to num-args)
                         (collect (gensym "ARG")))))
    `(define-math-interactor-command (,(symb 'com- com-name) :name t )
         ,(mapcar #`(,a1 ',presentation-type) arguments)
       (put-result (,op-name ,@arguments)))))

(def-gm-method plus gm:+ 2)

(def-gm-method times gm:* 2)

(def-gm-method divide gm:/ 2)

;; for polynomials, first convert to a power series
(defun special-invert (object)
  (gm:/ (if (typep object 'polynomials:polynomial)
            (gm:-> 'power-series:power-series object)
            object)))

(def-gm-method invert special-invert 1)

(def-gm-method sqrt gm:sqrt 1)

(def-gm-method minus gm:- 1)

;; allow on the fly input of new stuff
(define-math-interactor-command (com-enter-polynomial :name t :menu t)
    ((coeff-string 'string))
  (let ((coeffs (read-from-string (concatenate 'string "(" coeff-string ")"))))
    (if (length=1 coeffs)
        (put-result (first coeffs))
        (put-result (apply #'polynomials:make-polynomial coeffs)))))

(define-math-interactor-command (com-clear-app :name "Clear" :menu t)
    ()
  (window-clear (get-frame-pane *application-frame* 'app)))
