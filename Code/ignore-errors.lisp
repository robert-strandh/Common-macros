(cl:in-package #:common-macros)

(defmacro cmd:ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition)
       (values nil condition))))
