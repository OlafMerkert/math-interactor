(in-package :generic-math-output-implementation)

;; also make rational stuff look nicer

(defparameter *integer-output-mode* nil
  "possible values are NIL for default output, T for factorised
  output, or a prime integer P for splitting off it's powers.")

(defun prepare-extract-p (rational p)
  (multiple-value-bind (order remfactors) (nt:ord-p p rational)
    (let ((*integer-output-mode* nil))
     (if (zerop order)
         (math-output-prepare remfactors)
         (finite-product (list (superscript p order)
                               (math-output-prepare remfactors)))))))

(defun prepare-factorised (integer)
  (if (= 1 integer)
      integer
      (finite-product
       (mapcar (lambda (x)
                 (if (consp x)
                     (superscript (car x) (cdr x))
                     x))
               (nt:factorise integer)))))

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

;;; TODO output of symbols for infinity // generally unicode output

