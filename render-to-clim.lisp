(in-package :render-to-clim)

(defgeneric render (object stream))

(defmacro def-render-method ((format &key center) &body body)
  `(defmethod render ((,format ,(symb+ :mft format)) stream)
     (with-new-output-record (stream
                              ',(if center
                                    'math-output-record/with-center
                                    'math-output-record)
                              new-record)
       (flet ((pr (x) (princ x stream))
              (fmt (&rest args) (apply #'format stream args))
              (rndr (x) (render x stream)))
         ;; apply scaling and colour
         (with-drawing-options (stream :text-size (compute-size (mft:scaling ,format))
                                       :ink (compute-colour (mft:colour ,format)))
           ,@body)
         new-record))))

(defparameter *default-math-text-size* 11)

(defun compute-size (integer)
  (signcase integer :smaller nil :larger))

(defun compute-colour (colour)
  (if (colorp colour)
      colour
      +foreground-ink+))

(defmacro def-render-methods (&body specs)
  `(progn
     ,@(mapcar #`(def-render-method ,@a1) (group specs 2))))


;; placeholders
(def-render-methods
  (ellipsis) (pr "...")
  (infinity) (pr "oo"))

;; primitives
(def-render-methods
  (integer) (pr (mft:n integer))
  (number) (pr (mft:n number))
  (variable) (pr (mft:name variable))
  (text) (pr (mft:content text)))

;; composed
(def-render-method (fraction :center t)
  (let ((numerator   (rndr (mft:numerator fraction)))
        (denominator (rndr (mft:denominator fraction))))
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
              (values (floor w 2) (- center y)))))))

;; TODO scaling of exponent and index 
(def-render-method (superscript :center t)
  (let ((base (rndr (mft:base superscript)))
        (exponent (rndr (mft:exponent superscript))))
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
                                               (+ b-o e-o))))))

(def-render-method (subscript :center t)
  (let ((base (rndr (mft:base subscript)))
        (index (rndr (mft:index subscript)) ))
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
                                               b-o)))))

(defun px->pp (x) x)

(defun as-output-record (string stream)
  (with-new-output-record (stream 'math-output-record new-record)
    (princ string stream)))

(def-render-method (parentheses :center t)
  (let ((center (rndr (mft:body parentheses))))
    (with-text-size (stream (px->pp (rectangle-height center)))
      (let ((left (as-output-record (mft:open parentheses) stream))
            (right (as-output-record  (mft:close parentheses) stream))
            (*math-horizontal-spacing* 0))
        (setf (center-offset new-record)
              (align-output-records (list left center right)
                                    #'stacking-align
                                    #'centering-align))))))

(def-render-method (infix-expression :center t)
  (let ((ops+args
         (iter (for op in (mft:operators infix-expression))
               (for arg in (mft:arguments infix-expression))
               (when op
                 (collect (as-output-record op stream)))
               (collect (rndr arg)))))
    (setf (center-offset new-record)
          (align-output-records ops+args
                                #'stacking-align
                                #'centering-align))))

(def-render-method (grid2 :center t)
  (let ((elements (map-array1 #'rndr (mft:elements grid2))))
    (setf (center-offset new-record)
          (align-grid-output-records elements))))

(defun presentable (x)
  (or (typep x 'number)
      (typep x 'gm:generic-math-object)
      (typep x 'cf-ps:continued-fraction)))

(defun math-presentation-type-of (x)
  (cond ((integerp x) 'integer)
        ((rationalp x) 'rational)
        ((numberp x) 'number)
        (t (unbox (type-of x)))))

(defmethod render ((object-data mft:object-data) stream)
  (if (presentable (mft:object object-data))
      (with-new-output-record (stream 'math-output-presentation new-record
                                      :object (mft:object object-data)
                                      :type (expand-presentation-type-abbreviation 
                                             (math-presentation-type-of (mft:object object-data)))
                                      :single-box :highlighting)
        (let ((clim-internals::*allow-sensitive-inferiors* t))
          (setf (center-offset new-record)
                (center-offset (render (mft:body object-data) stream)))))
      (render (mft:body object-data) stream)))

(defun advance-cursor (math-output stream &key (line-break nil) (move-cursor t))
  "Output the computed output-record `math-output' into `stream'. If
  `line-break' is t, move the stream-cursor to the beginning of the
  \"next line\". Otherwise, if `move-cursor' is t, move the
  stream-cursor to the end of the \"current line\". Otherwise, do not
  chnage the cursor position."
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (setf (output-record-position math-output) (values 0 y))
    (cond (line-break
           (setf (stream-cursor-position stream)
                 (values 0 (+ y (rectangle-height math-output)))))
          ;; TODO move-cursor is currently broken
          (move-cursor
           (setf (stream-cursor-position stream)
                 (values (+ x (rectangle-width math-output) *math-horizontal-spacing*) y)))))
  math-output)

(defun reset-cursor (stream)
  "Move the cursor in `stream' to the beginning of the line."
  (mvbind (x y) (stream-cursor-position stream)
    (declare (ignorable x))
    (setf (stream-cursor-position stream)
          (values 0 y))))

;;; automatic colouring


;; for integers
(defmethod initialize-instance :after ((i mft:integer) &key)
  (setf (mft:colour i)
        (signcase (mft:n i) +red+ +black+ +blue+)))

(defparameter variable-colours
  (map-on-car #'mkstr
              `((u . ,+green+))))

(defmethod initialize-instance :after ((var mft:variable) &key)
  (setf (mft:colour var)
        (assoc1 (mkstr (mft:name var)) variable-colours nil :test 'string-equal)))

