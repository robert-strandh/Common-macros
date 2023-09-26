(cl:in-package #:common-macro-definitions)

(defgeneric proclaim (declaration-specifier))

(defmacro declaim (declaration-specifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for declaration-specifier in declaration-specifiers
             collect (proclaim declaration-specifier))))
