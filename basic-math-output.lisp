(in-package :math-interactor)

;;; some helper functions
(declaim (inline math-output-to-record
                 princ-to-stream))

(defun math-output-to-record (stream)
  (lambda (s) (aprog1 (math-output s stream)
           (stream-add-output-record stream it))))

(defun princ-to-stream (stream &optional (record-type 'operator-math-output-record))
  (lambda (s)
    (with-new-output-record (stream record-type new-operator)
      (princ s stream)
      new-operator)))

;;; macros to take care of the boilerplate for establishing new basic types
(defmacro define-basic-math-output-class (type slots+ &key centering primitive)
  "If CENTERING, "
  (let ((mo-classname (symb type '-math-output-record))
        (slots (args->names slots+)))
    `(progn
       ,@(unless primitive
                 `((defclass ,type ()
                     ,(mapcar #`(,a1 :initarg ,(keyw a1)
                                     :initform nil
                                     :reader ,a1) ; TODO maybe use accessor here
                              slots))
                   ;; provide a shorthand constructor
                   (defun ,type ,slots+
                     (make-instance ',type
                                    ,@(mapcan #`(,(keyw a1) ,a1) slots)))))
       (defclass ,mo-classname
           (,(if centering
                 'math-output-record/with-center
                 'math-output-record)
             standard-sequence-output-record
             ,@(unless primitive
                       (list type)))
         ()))))

(defmacro define-basic-math-output-method ((type output-record-type) &body body)
  "Anaphoric variables: STREAM, NEW-RECORD."
  `(defmethod math-output ((,type ,type) stream)
     ;; it's important to use this one, not WITH-NEW-OUTPUT-RECORD,
     ;; otherwise we get problems with attaching stuff.
     (with-output-to-output-record (stream ',output-record-type new-record
                                           ;; :math-object ,type
                                           )
       ,@body
       new-record)))

(defmacro define-basic-math-output ((type slots &key centering primitive) &body body)
  `(progn
     (define-basic-math-output-class ,type ,slots :centering ,centering :primitive ,primitive)
     (define-basic-math-output-method (,type ,(symb type '-math-output-record))
       ,@(if primitive body
             `((with-slots ,(args->names slots) new-record ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output of ordinary numbers
(define-basic-math-output (number () :primitive t)
  (princ number stream))

;; TODO more general support for colouring stuff.
(define-basic-math-output (integer () :primitive t)
  (with-drawing-options (stream :ink (cond ((minusp integer) +red+)
                                           ((plusp integer) +blue+)
                                           (t +foreground-ink+)))
    (princ integer stream)))

;;; output of symbols and string
(define-basic-math-output (string () :primitive t)
  (princ string stream))

(define-basic-math-output (symbol () :primitive t)
  (with-drawing-options (stream :ink (cond ((string-equal (symbol-name symbol) "U")
                                            +green+)
                                           (t +foreground-ink+)))
    (princ symbol stream)))

;;; output of (explicit) fractions
(define-basic-math-output (fraction (numerator denominator) :centering t)
    (setf numerator   (math-output (numerator fraction) stream)
          denominator (math-output (denominator fraction) stream))
    (stream-add-output-record stream numerator)
    (stream-add-output-record stream denominator)
    ;; calculate dimensions and move n/d in position
    (align-output-records (list numerator denominator)
                          #'centering-align
                          #'stacking-align)
    ;; draw the line of the fraction at the appropriate place
    (multiple-value-bind (x y) (output-record-position new-record)
      (let* ((w (rectangle-width  new-record))
             (h (rectangle-height numerator))
             (center (+ y h (floor *math-vertical-spacing* 2))))
        (draw-line* stream
                    x center (+ x w) center)
        ;; mark center offset
        (setf (center-offset new-record)
              (values (floor w 2) (- center y))))))

;;; finite sum expressions
(define-basic-math-output (finite-sum (summands &optional operators) :centering t)
  (setf summands
        (mapcar (math-output-to-record stream)
                (summands finite-sum))
        operators
        (mapcar (princ-to-stream stream 'operator-math-output-record)
                (operators finite-sum)))
  (setf (center-offset new-record)
        (align-output-records (insert-operators summands operators)
                              #'stacking-align
                              #'centering-align)))

(defmethod initialize-instance :after ((sum finite-sum) &key)
  (unless (typep sum 'math-output-record)
    (with-slots (summands operators) sum
      (cond ((= (length summands) (+ 1 (length operators))))
            ((null operators)
             (setf operators (n-copies (- (length summands) 1) '+)))
            (t (error "finite-sum: length of summands (~A) and operators (~A) do not match."
                      (length summands) (length operators)))))))


;;; finite product expressions
(define-basic-math-output (finite-product (factors) :centering t)
  (setf factors (mapcar (math-output-to-record stream)
                        (factors finite-product)))
  ;; stretch spacing a little bit
  (let ((*math-horizontal-spacing* (ceiling (* 1.5 *math-horizontal-spacing*))))
    ;; TODO do we want small multiplication dots or x ??
    (setf (center-offset new-record)
          (align-output-records factors
                                #'stacking-align
                                #'centering-align))))

(defun math-output/smaller (object stream)
  (with-text-size (stream :smaller)
    (math-output object stream)))

;;; subscript
(define-basic-math-output (subscript (base index) :centering t)
  (setf base (math-output (base subscript) stream)
        ;; downscaling the index
        index (math-output/smaller (index subscript) stream))
  (stream-add-output-record stream base)
  (stream-add-output-record stream index)
  (let ((b-w (rectangle-width base))
        (i-w (rectangle-width index))
        (b-h (rectangle-height base))
        (b-o (nth-value 1 (center-offset base)))
        (i-o (nth-value 1 (center-offset index))))
    (setf (output-record-position base) (values 0 0))
    ;; move the index right and down, s.t. its left border touches the right
    ;; border of the base, and it's center is aligned with the bottom
    ;; of the base.
    (setf (output-record-position index) (values b-w (- b-h i-o)))
    ;; the vertical center should be the same as what the base has; the
    ;; horizontal center is just the usual center.
    (setf (center-offset new-record) (values (floor (+ b-w i-w) 2)
                                             b-o))))
;;; superscript
(define-basic-math-output (superscript (base exponent) :centering t)
  (setf base (math-output (base superscript) stream)
        ;; downscaling the exponent
        exponent (math-output/smaller (exponent superscript) stream))
  (stream-add-output-record stream base)
  (stream-add-output-record stream exponent)
  (let ((b-w (rectangle-width base))
        (e-w (rectangle-width exponent))
        (b-o (nth-value 1 (center-offset base)))
        (e-o (nth-value 1 (center-offset exponent))))
    ;; move base down, s.t. the top is vertically aligned with the
    ;; center of the exponent.
    (setf (output-record-position base) (values 0 e-o))
    ;; move the exponent right, s.t. its left border touches the right
    ;; border of the base
    (setf (output-record-position exponent) (values b-w 0))
    ;; the vertical center should be the same as what the base has; the
    ;; horizontal center is just the usual center.
    (setf (center-offset new-record) (values (floor (+ b-w e-w) 2)
                                             (+ b-o e-o)))))

;; TODO superscript and fractions don't work well together at the
;; moment (ambiguity) 

;;; TODO parentheses and grouping

;;; explicit marking of presentations
(define-basic-math-output (explicit-presentation (rendering underlying-object presentation-type) :centering t)
  (setf rendering (math-output (rendering explicit-presentation) stream))
  (with-output-as-presentation (stream (underlying-object explicit-presentation)
                                       (presentation-type explicit-presentation))
    (stream-add-output-record stream rendering))
  (setf (center-offset new-record) (center-offset rendering)))
