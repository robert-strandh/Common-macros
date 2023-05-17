(cl:in-package #:common-macros)

;;; This code is taken from the dictionary page on DEFPARAMETER in the
;;; Common Lisp standard.
(defmacro defparameter
    (name initial-value &optional (documentation nil documentation-p))
  `(progn (cl:declaim (special ,name))
          (cl:setf (symbol-value ',name) ,initial-value)
          ,(cl:when documentation-p
             `(cl:setf (documentation ',name 'variable) ',documentation))
          ',name))
