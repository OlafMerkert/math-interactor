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
  ;; remove old displayed stuff
  (window-clear (get-frame-pane *application-frame* 'bin))
  ;; show anything stored in the *BIN* again
  (mapc #'funcall (reverse *math-interactor-load-data-hooks*)))

;; make the bin persistent
(defvar *bin* nil)

;;; define commands for math-utils operations
(defun put-result% (math-object pane-id)
  ;; for now, just dump stuff to app output
  (let ((stream (get-frame-pane *application-frame* pane-id)))
    ;; explicitly call prepare, so e.g. integers get enriched with presentations
    (stream-add-math-output stream (math-output (math-output-prepare math-object) stream)
                            :line-break t)
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

(define-math-interactor-command (com-clear-bin :name "Clear/bin")
    ()
  (setf *bin* nil)
  (window-clear (get-frame-pane *application-frame* 'bin)))

(define-math-interactor-command (com-clear-app :name "Clear/app" :menu t)
    ()
  (window-clear (get-frame-pane *application-frame* 'app)))

;; start off with the generic stuff.
(define-presentation-type math-object-presentation ()
  :description "math object")

(ew (defvar math-object-presentation-table (make-hash-table)))

(defgeneric math-object-presentation (object))

(defmacro def-gm-method% (com-name op-name
                         &optional (num-args 1)
                                   (presentation-type 'math-object-presentation))
  `(def-gm-method ,com-name ,op-name ,@(n-copies num-args presentation-type)))

(defmacro def-gm-method (com-name op-name &rest parameter-types)
  (let ((arguments (list->gensyms :arg parameter-types)))
    `(define-math-interactor-command (,(symb 'com- com-name) :name t )
         ,(mapcar #2`(,a1 ',a2) arguments parameter-types)
       (put-result (,op-name ,@arguments)))))

(def-gm-method% plus gm:+ 2)

(def-gm-method% times gm:* 2)

(def-gm-method% divide gm:/ 2)

;; for polynomials, first convert to a power series
(defun special-invert (object)
  (gm:/ (if (typep object 'polynomials:polynomial)
            (gm:-> 'power-series:power-series object)
            object)))

(def-gm-method% invert special-invert 1)

(def-gm-method% sqrt gm:sqrt 1)

(def-gm-method% minus gm:- 1)

(define-math-interactor-command (com-reduce-modp :name "reduce mod p")
    ((math-object 'math-object-presentation) (p 'integer :default 3 :prompt "prime"))
  ;; check that p is prime
  (assert (nt:prime-p p))
  (put-result (gm:-> 'finite-fields:integer-mod math-object :mod p)))

(define-math-interactor-command (com-order-p :name "order of p")
    ((math-object 'math-object-presentation) (p 'integer :default 3 :prompt "prime"))
  (assert (nt:prime-p p))
  (format #1=(get-frame-pane *application-frame* 'app) "ord_~A = " p)
  (multiple-value-bind (bound comment)
      (vv:valuate-exp p math-object)
    (put-result bound)
    (when (eq comment :unbounded)
      (format #1# " unbounded~%"))))

;; allow on the fly input of new stuff
(define-math-interactor-command (com-enter-polynomial :name t :menu t)
    ((coeff-string 'string :prompt "coefficients"))
  ;; TODO make polynomial input less hackish
  (let ((coeffs (read-from-string (concatenate 'string "(" coeff-string ")"))))
    (if (length=1 coeffs)
        (put-result (first coeffs))
        (put-result (apply #'polynomials:make-polynomial coeffs)))))

(define-math-interactor-command (com-series-term :name "Set series output precision")
    ((additional-terms 'integer :default 5 :prompt "nr of additional terms"))
  (setf polynomial-series-printing:print-additional-terms
        additional-terms)
  (format (get-frame-pane *application-frame* 'int)
          "~&Show ~A negative coefficients of laurent series.~%"
          additional-terms))

;;; show fractions factorised

;;; todo allow factorisation only over a specified set of places
(define-math-interactor-command (com-toggle-integer-display :name "Select integer display.")
    ((integer 'integer :default 0 :prompt "0 for no factorisation, -1 for complete factorisation or any single factor."))
  (format (get-frame-pane *application-frame* 'int)
          (cond ((zerop integer)
                 (setf gmo:*integer-output-mode* nil)
                 "~&No factorisation.~%")
                ((minusp integer)
                 (setf gmo:*integer-output-mode* t)
                 "~&Complete factorisation.~%")
                (t
                 (setf gmo:*integer-output-mode* integer)
                 (mkstr "~&Factor out just "
                        integer
                        ".~%")))))

(define-math-interactor-command (com-valuate-coeff :name "Valuate coefficientwise" :menu t)
    ((math-object 'math-object-presentation)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result (vc:valuate-exp valuation math-object)))

(define-math-interactor-command (com-valuate :name "Valuate")
    ((math-object 'math-object-presentation)
     (valuation 'integer :prompt "valuation (prime number)"))
  (put-result (vv:valuate-exp valuation math-object)))

(def-gm-method% ggt fractions:ggt 2)

