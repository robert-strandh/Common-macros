(cl:in-package #:common-macro-definitions)

(defun expand-declaim (declaration-specifiers proclaim)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for declaration-specifier in declaration-specifiers
             collect (funcall proclaim declaration-specifier))))
