(in-package :generic-math-output-implementation)

;; what does this math-output prepare actually do
(defgeneric math-output-prepare (object))

(defmethod math-output-prepare (object)
  object)

(defmacro def-math-output-prepare ((type &key (standard-redirection t)) &body body)
  `(progn
     (defmethod math-output-prepare ((,type ,type))
       ,@body)
     ,@(when standard-redirection
             `((defmethod math-output ((,type ,type) stream)
                 (math-output (math-output-prepare ,type) stream))))))
