(cl:in-package #:common-macros)

;;; This code is taken from the dictionary page on DEFPARAMETER in the
;;; Common Lisp standard.
(defmacro defparameter
    (name initial-value &optional (documentation nil documentation-p))
  `(progn (declaim (special ,name))
          (setf (symbol-value ',name) ,initial-value)
          ,(when documentation-p
             `(setf (documentation ',name 'variable) ',documentation))
          ',name))
