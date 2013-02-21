(in-package :generic-math-output-implementation)

;; output of various very simple data-structures

;; also make rational stuff look nicer

(defmethod math-output-prepare ((integer integer)) integer)

(def-math-output-prepare (rational)
  (fraction (cl:numerator rational) (cl:denominator rational)))

(def-math-output-prepare (finite-fields:integer-mod)
  (superscript (finite-fields:remainder finite-fields:integer-mod)
               ;; TODO add some brackets
               (finite-fields:modulus finite-fields:integer-mod)))

;; now pack everything up in presentations.

(defmacro def-mo-pres-type (mo-type)
  `(progn
     (define-presentation-type ,(symb mo-type '-presentation) ()
       :inherit-from '(and math-object-presentation ,mo-type))
     (defmethod math-object-presentation ((,mo-type ,mo-type))
       ',(symb mo-type '-presentation))))

(def-mo-pres-type number)

;; TODO perhaps here we want to profit from different views.
(define-presentation-method present (object (type math-object-presentation) stream view &key)
  (stream-add-math-output stream (math-output object stream)
                          :line-break t))

;; a small hack to get math-objects into input fields for commmands
(defvar math-object-store (make-hash-table :test 'equal))

;; perhaps make this look nicer.
(defun get-string-representation (math-object)
  (subseq
   (with-output-to-string (stream)
     (print-unreadable-object (math-object stream :type t :identity t)))
   1)) ; discard the #

(defun from-store (string-repr)
  (gethash string-repr math-object-store))

(defun to-store (math-object)
  (let ((repr (get-string-representation math-object)))
    (gethash/c repr math-object-store math-object)
    repr))

(define-presentation-method present (object (type math-object-presentation) stream (view textual-dialog-view) &key)
  (princ (to-store object) stream))

(define-presentation-method accept ((type math-object-presentation) stream (view textual-dialog-view) &key)
  (from-store (read-line stream)))


