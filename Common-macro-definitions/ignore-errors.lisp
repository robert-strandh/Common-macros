(cl:in-package #:common-macro-definitions)

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
