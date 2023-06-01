(cl:in-package #:common-macros)

(defmacro cmd:multiple-value-bind
    ((&rest variables) values-form &body body)
  (let ((rest-variable (gensym)))
    `(multiple-value-call
         (lambda (&optional ,@variables &rest ,rest-variable)
           (declare (ignore ,rest-variable))
           ,@body)
       ,values-form)))
                
