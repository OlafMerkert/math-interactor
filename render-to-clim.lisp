(in-package :render-to-clim)

(defgeneric render (object stream))

(defmacro def-render-method ((format) &body body)
  `(defmethod render ((,format ,(symb+ :mft format)) stream)
     (flet ((pr (x) (princ x stream))
            (fmt  (&rest args) (apply #'format stream args))
            (rndr (x) (render x stream)))
       ,@body)))

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
(def-render-method (fraction)
  (rndr (mft:numerator fraction))
  (pr "/")
  (rndr (mft:denominator fraction)))

(def-render-method (superscript)
  (rndr (mft:base superscript))
  (pr "^")
  (rndr (mft:exponent superscript)))

(def-render-method (subscript)
  (rndr (mft:base subscript))
  (pr "_")
  (rndr (mft:index subscript)))

(def-render-method (parentheses)
  (pr (mft:open parentheses))
  (rndr (mft:body parentheses))
  (pr (mft:close parentheses)))

(def-render-method (infix-expression)
  (iter (for op in (mft:operators infix-expression))
        (for arg in (mft:arguments infix-expression))
        (unless (first-iteration-p)
          (pr " "))
        (when op
          (pr op)
          (pr " "))
        (rndr arg)))


(def-render-method (object-data)
  (rndr (mft:body object-data)))

