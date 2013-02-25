(in-package :math-interactor)

(define-application-frame math-interactor ()
  ()
  (:panes (app :application
               :width 800 :height 600
               :incremental-redisplay t
               :scroll-bars t)
          (int :interactor
               :width 800 :height 200)
          (bin :application
               :width 200 :height 500
               :incremental-redisplay t
               :scroll-bars t))
  (:layouts (default
                (vertically ()
                  (horizontally ()
                    bin
                    app)
                  int))))

(defparameter *math-interactor-load-data-hooks* nil)

(defun add-mi-hook (fn)
  (pushnew fn *math-interactor-load-data-hooks*))


(defun math-interactor ()
  (run-frame-top-level (make-instance 'math-interactor)))

;; define a command to exit
(define-math-interactor-command (com-quit :menu nil :name "Quit") ()
  (frame-exit *application-frame*))

(define-math-interactor-command (com-run-hooks :menu t :name "Load data")
    ()
  (mapc #'funcall (reverse *math-interactor-load-data-hooks*)))

;; make the bin persistent
(defvar *bin* nil)

;;; define commands for math-utils operations
(defun put-result% (math-object pane-id)
  ;; for now, just dump stuff to app output
  (let ((stream (get-frame-pane *application-frame* pane-id)))
    (present math-object (math-object-presentation math-object)
             :stream stream)
    (stream-replay stream)))

(defun put-result (math-object &optional to-bin)
  (if (not to-bin)
      (put-result% math-object 'app)
      (unless (member math-object *bin* :test #'gm:=)
        (push math-object *bin*)
        (put-result% math-object 'bin))))

;; load stuff from bin on making new window
(defun populate-from-bin ()
  (dolist (mo (reverse *bin*))
    (put-result% mo 'bin)))

(add-mi-hook 'populate-from-bin)

(define-math-interactor-command (com-put-to-bin :menu t :name "Copy/bin")
    ((object 'math-object-presentation))
  (put-result object t))

(define-math-interactor-command (com-put-to-app :menu t :name "Copy/app")
    ((object 'math-object-presentation))
  (put-result object))

(define-math-interactor-command (com-clear-bin :name "Clear bin")
    ()
  (setf *bin* nil)
  (window-clear (get-frame-pane *application-frame* 'bin)))

(define-math-interactor-command (com-clear-app :name "Clear" :menu t)
    ()
  (window-clear (get-frame-pane *application-frame* 'app)))

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

(define-math-interactor-command (com-reduce-modp :name "reduce mod p")
    ((math-object 'math-object-presentation) (p 'integer :default 3))
  ;; TODO check that p is prime
  (put-result (gm:-> 'finite-fields:integer-mod math-object :mod p)))

;; allow on the fly input of new stuff
(define-math-interactor-command (com-enter-polynomial :name t :menu t)
    ((coeff-string 'string))
  (let ((coeffs (read-from-string (concatenate 'string "(" coeff-string ")"))))
    (if (length=1 coeffs)
        (put-result (first coeffs))
        (put-result (apply #'polynomials:make-polynomial coeffs)))))



(define-math-interactor-command (com-series-term :name "Set series output precision")
    ((additional-terms 'integer :default 5))
  (setf polynomial-series-printing:print-additional-terms
        additional-terms))
