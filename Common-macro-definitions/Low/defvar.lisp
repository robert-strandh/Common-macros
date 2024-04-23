(cl:in-package #:common-macro-definitions)

(defmacro defvar (name &optional
                         (initial-value nil initial-value-p)
                         (documentation nil documentation-p))
  `(progn (declaim (special ,name))
          ,(when initial-value-p
             `(unless (boundp ',name)
                (setf (symbol-value ',name) ,initial-value)))
          ,(when documentation-p
             `(setf (documentation ',name 'variable) ',documentation))
          ',name))
