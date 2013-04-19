(in-package :generic-math-output-implementation)

;; what does this math-output prepare actually do
(defgeneric math-output-prepare (object))

(defmethod math-output-prepare (object)
  object)

(defmacro def-math-output-prepare ((type &key (standard-redirection t)
                                         (presentation t)) &body body)
  `(progn
     (defmethod math-output-prepare ((,type ,type))
       ;; automatically enrich math-output with presentation
       ,@(cond ((not presentation) body)
               ((eq presentation t)
                `((explicit-presentation (progn ,@body) ,type ',(gethash type math-object-presentation-table))))
               (t
                `((explicit-presentation (progn ,@body) ,type ',presentation)))))
     ,@(when standard-redirection
             `((defmethod math-output ((,type ,type) stream)
                 (math-output (math-output-prepare ,type) stream))))))
