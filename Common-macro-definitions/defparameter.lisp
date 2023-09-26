(cl:in-package #:common-macro-definitions)

(defmacro defparameter (name initial-value 
                        &optional (documentation nil documentation-p))
  `(progn (declaim (special ,name))
          (setf (symbol-value ',name) ,initial-value)
          ,(when documentation-p
             `(setf (documentation ',name 'variable) ',documentation))
          ',name))
