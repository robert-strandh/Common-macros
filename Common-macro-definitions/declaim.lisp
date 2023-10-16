(cl:in-package #:common-macro-definitions)

(defgeneric proclaim (client declaration-specifier environment))

(defmacro declaim (&environment environment declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for declaration-specifier in declaration-specifiers
             collect (proclaim *client* declaration-specifier environment))))
