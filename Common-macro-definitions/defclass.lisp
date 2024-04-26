(cl:in-package #:common-macro-definitions)

(defgeneric defclass-compile-time-action
    (client name superclass-names metaclass-name environment))

(defgeneric ensure-class-name (client))

(defmacro defclass
    (&environment environment
     name superclass-names slot-specifiers &rest options)
  (expand-defclass
   name superclass-names slot-specifiers options
   (lambda (name canonicalized-superclass-names metaclass-name)
     (defclass-compile-time-action
       *client* name canonicalized-superclass-names
       metaclass-name environment))
   (ensure-class-name *client*)))
