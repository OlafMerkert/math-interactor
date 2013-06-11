(in-package :math-interactor)

;;; helper functions for managing cursor advancement
(defun stream-add-math-output (stream math-output
                               &key (line-break nil) (move-cursor t))
  "Output the computed output-record `math-output' into `stream'. If
  `line-break' is t, move the stream-cursor to the beginning of the
  \"next line\". Otherwise, if `move-cursor' is t, move the
  stream-cursor to the end of the \"current line\". Otherwise, do not
  chnage the cursor position."
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
