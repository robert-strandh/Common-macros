(cl:in-package #:common-macro-definitions)

(defgeneric proclaim (client declaration-specifier environment))

(defmacro declaim (&environment environment &rest declaration-specifiers)
  (expand-declaim
   declaration-specifiers
   (lambda (declaration-specifier)
     (proclaim *client* declaration-specifier environment))))
