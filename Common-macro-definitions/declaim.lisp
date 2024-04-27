(cl:in-package #:common-macro-definitions)

(defgeneric proclaim (client declaration-specifier environment))

(defun expand-declaim (declaration-specifiers proclaim)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for declaration-specifier in declaration-specifiers
             collect (funcall proclaim declaration-specifier))))

(defmacro declaim (&environment environment &rest declaration-specifiers)
  (expand-declaim
   declaration-specifiers
   (lambda (declaration-specifier)
     (proclaim *client* declaration-specifier environment))))
