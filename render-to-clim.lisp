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
                                           (fmt  (&rest args) (apply #'format stream args))
              (rndr (x) (render x stream)))
         ;; TODO apply scaling and colour 
         ,@body
         new-record))))

(defmacro def-render-methods (&body specs)
  `(progn
     ,@(mapcar #`(def-render-method ,@a1) (group specs 2))))


;; placeholders
(def-render-methods
  (ellipsis) (pr "...")
  (infinity) (pr "∞"))

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
        (exponent (rndr (mft:exponent superscript :center t))))
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

(defun presentable (x)
  (or (typep x 'number)
      (typep x 'gm:generic-math-object)))

(defun math-presentation-type-of (x)
  (unbox (type-of x)))

(def-render-method (object-data :center t)
  (if (presentable (mft:object object-data))
      (with-output-as-presentation (stream
                                    (mft:object object-data)
                                    (math-presentation-type-of (mft:object object-data)))
        (setf (center-offset new-record)
              (center-offset (rndr (mft:body object-data)))))
      (rndr (mft:body object-data))))

