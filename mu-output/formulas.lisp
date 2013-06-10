(in-package :generic-math-output-implementation)

;;; support for rendering formal lisp expressions to the
;;; math-interactor, this is the analogue of math-output-prepare for
;;; S-expressions

(defun formula-prepare (formula)
  (cond ((consp formula) (formula-prepare% (car formula) (cdr formula)))
        ;; allow insertion of prepared math-objects into formulas 
        ((basic-math-output-p formula) formula)
        ((atom formula) formula)))

(defgeneric formula-prepare% (function-symbol arguments))

(defmacro def-formula-prepare (head &body body)
  `(defmethod formula-prepare% ((head (eql ',head)) arguments)
     (let ((n (length arguments)))
       (declare (ignorable n))
       ,@body)))

(bind-multi ((op + = >= <= > <))
  (def-formula-prepare op
    (finite-sum (mapcar #'formula-prepare arguments)
                (n-copies (- n 1) 'op))))

(def-formula-prepare * 
  (finite-product (mapcar #'formula-prepare arguments)))

(def-formula-prepare nil
  (finite-product (mapcar #'formula-prepare arguments)))

(def-formula-prepare / 
  (case n
    (1 (fraction 1 (formula-prepare (first arguments))))
    (2 (fraction (formula-prepare (first arguments))
                 (formula-prepare (second arguments))))
    (t (fraction (formula-prepare (first arguments))
                 (finite-product (mapcar #'formula-prepare (rest arguments)))))))

(def-formula-prepare - 
  ;; TODO handle single argument correctly
  (finite-sum (mapcar #'formula-prepare arguments)
              (n-copies (- n 1) '-)))

(def-formula-prepare ^
  (superscript (formula-prepare (first arguments))
               (formula-prepare (second arguments))))

(def-formula-prepare _
  (subscript (formula-prepare (first arguments))
             (formula-prepare (second arguments))))

(defmethod formula-prepare% ((function-symbol symbol) arguments)
  ;; TODO put the arguments in brackets, perhaps separated by commas
  (finite-product (list function-symbol (parens (finite-product arguments)))))


(defmacro formula-with-math-objects (math-objects formula)
  `(let ,(mapcar (compose #`(,(first a1) (math-output-prepare ,(second a1)))
                          (lambda (mo-spec)
                            (cond ((consp mo-spec) mo-spec)
                                  ((symbolp mo-spec) (list mo-spec mo-spec))
                                  (t (error "invalid math-object-spec ~A" mo-spec)))))
                 math-objects)
     (formula-prepare ,formula)))

;; TODO spacers
;; TODO text mixed into formulas.
