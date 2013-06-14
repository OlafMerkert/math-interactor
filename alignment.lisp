(in-package :render-to-clim)

;;; custom classes for the output records
(defclass math-output-record (standard-sequence-output-record)
  ())

(defclass math-output-record/with-center (math-output-record)
  ((center-x :initform 0)
   (center-y :initform 0))
  (:documentation "Keep track of explicit center coordinates of the
  output-record. This is important for compound objects."))

(defclass math-output-presentation (clim-internals::presentation-mixin
                                    math-output-record/with-center)
  ())

;; which are mainly used for keeping track of centers for alignment
(defgeneric center-offset (math-output-record)
  (:documentation "As two values return the center coordinates of the
  given record. Keep in mind that these coordinates are absolute."))
;; TODO is the center-offset relative or absolute ?

(defmethod center-offset ((math-output-record math-output-record))
  "For ordinary output-record, just compute the barycenter."
  (values (floor (rectangle-width math-output-record) 2)
          (floor (rectangle-height math-output-record) 2)))

(defmethod center-offset ((math-output-record math-output-record/with-center))
  "For this, center coordinates are given explicitly."
  (values (slot-value math-output-record 'center-x)
          (slot-value math-output-record 'center-y)))

(clim-internals::defgeneric* (setf center-offset) (x y math-output))

(clim-internals::defmethod* (setf center-offset)
  (x y (math-output math-output-record/with-center))
  "provide a SETF method to alter center coordinates."
  (with-slots (center-x center-y) math-output
    (setf center-x x
          center-y y)))

;;; and provide some alignments functions
(defun insert-operators (arguments operators)
  "Given '(a b c) and '(+ -), create the output '(a + b - c)."
  (list* (first arguments)
         (alternate operators (rest arguments))))

;;; a few helper functions for laying out math objects
(defparameter *math-vertical-spacing* 5)
(defparameter *math-horizontal-spacing* 3)

(defun nth-arg (n)
  "Build a function that simply returns it's NTH argument."
  (lambda (&rest args)
    (nth n args )))

(defun align-output-records (records horizontal-align-fn vertical-align-fn
                             &optional (x-combinate (nth-arg 1)) (y-combinate x-combinate))
  "Set the positions of the output-`records', where
`horizontal-align-fn' computes the horizontal coordinate and
`vertical-align-fn' computes the vertical coordinate. Moreover, the
functions `x-combinate' and `y-combinate' take as input the current
and computed coordinate, so may be used to factor in current
coordinates. Return the center of the grouping of `records'."
  (multiple-value-bind (h-pos x-off) (funcall horizontal-align-fn records :horizontal)
    (multiple-value-bind (v-pos y-off) (funcall vertical-align-fn records :vertical)
      (mapc (lambda (record h-pos v-pos)
              (multiple-value-bind (x y) (output-record-position record)
                (setf (output-record-position record)
                      (values (funcall x-combinate x h-pos)
                              (funcall y-combinate y v-pos)))))
            records h-pos v-pos)
      ;; return center coordinates
      (values x-off y-off))))

(defun centering-align (records direction)
  "Compute the positions of `records' (either in
`direction' :HORIZONTAL or :VERTICAL), with everything centered on the
same position (taking into account the `center-offset'). As second
value returns the center-offset of the grouping of `records'."
  (let* ((offsets (mapcar (ecase direction
                            (:horizontal (lambda (r) (nth-value 0 (center-offset r))))
                            (:vertical   (lambda (r) (nth-value 1 (center-offset r)))))
                          records))
         ;; find the largest center offset.
         (o (reduce #'max offsets)))
    (values (mapcar (lambda (x) (- o x)) offsets)
            o)))

(defun stacking-align (records direction)
  "Compute the positions of `records' (either in
`direction' :HORIZONTAL or :VERTICAL), with everything beside each
other, with offset `*math-horizontal-spacing*' or
`*math-vertical-spacing*' between the records. As second value returns
the center-offset of the grouping of `records'."
  (let ((lengths (mapcar (ecase direction
                           (:horizontal #'rectangle-width)
                           (:vertical   #'rectangle-height))
                         records))
        (position 0)
        (offset (ecase direction
                  (:vertical   *math-vertical-spacing*)
                  (:horizontal *math-horizontal-spacing*))))
    (values (mapcar (lambda (l)
                      (prog1 position
                        (incf position (+ l offset))))
                    lengths)
            ;; calculate the center too
            (floor (- position offset) 2))))
