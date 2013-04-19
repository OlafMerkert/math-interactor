(in-package :math-interactor)

;;; the general protocoll for formatted math-output

(defclass math-output-record (output-record)
  (#|(math-object :initarg :math-object
                :accessor math-object)|#))

(defclass math-output-record/with-center (math-output-record)
  ((center-x :initform 0)
   (center-y :initform 0)))

(defgeneric math-output (math-object stream))

(defmethod math-output :before (math-object stream)
  (when (typep math-object 'math-output-record)
    (error "Cannot call math-output on output-records!")))

(defgeneric center-offset (math-output-record))

(defmethod center-offset ((math-output-record math-output-record))
  (values (floor (rectangle-width math-output-record) 2)
          (floor (rectangle-height math-output-record) 2)))

(defmethod center-offset ((math-output-record math-output-record/with-center))
  (values (slot-value math-output-record 'center-x)
          (slot-value math-output-record 'center-y)))

(clim-internals::defgeneric* (setf center-offset) (x y math-output))

(clim-internals::defmethod* (setf center-offset)
    (x y (math-output math-output-record/with-center))
  (with-slots (center-x center-y) math-output
    (setf center-x x
          center-y y)))

;;; treat math operators specially
(defclass operator-math-output-record (math-output-record
                                       standard-sequence-output-record)
  ())

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
  (let* ((offsets (mapcar (ecase direction
                            (:horizontal (lambda (r) (nth-value 0 (center-offset r))))
                            (:vertical   (lambda (r) (nth-value 1 (center-offset r)))))
                          records))
         ;; find the largest center offset.
         (o (reduce #'max offsets)))
    (values (mapcar (lambda (x) (- o x)) offsets)
            o)))

(defun stacking-align (records direction)
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

;;; helper functions for managing cursor advancement
(defun stream-add-math-output (stream math-output
                               &key (line-break nil) (move-cursor t))
  (stream-add-output-record stream math-output)
  (setf (output-record-position math-output) (stream-cursor-position stream))
  (cond (line-break
         (multiple-value-bind (x y) (stream-cursor-position stream)
           (declare (ignore x))
           (setf (stream-cursor-position stream)
                 (values 0 (+ y (rectangle-height math-output))))))
        (move-cursor
         (multiple-value-bind (x y) (stream-cursor-position stream)
           (setf (stream-cursor-position stream)
                 (values (+ x (rectangle-width math-output) *math-horizontal-spacing*) y)))))
  math-output)

;; TODO figure out a proper line-breaking protocol
;; TODO make this behave more nicely with other output to the stream.
