(cl:in-package #:common-macros)

;;; This code is taken from the dictionary page on DEFVAR in the
;;; Common Lisp standard.
(defmacro defvar
    (name &optional
            (initial-value nil initial-value-p)
            (documentation nil documentation-p))
  `(progn (cl:declaim (special ,name))
          ,(cl:when initial-value-p
             `(cl:unless (boundp ',name)
                (cl:setf (symbol-value ',name) ,initial-value)))
          ,(cl:when documentation-p
             `(cl:setf (documentation ',name 'variable) ',documentation))
          ',name))
