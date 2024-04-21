(cl:in-package #:common-macro-definitions)

(defmacro when (test &body forms)
  `(if ,test
       (progn ,@forms)
       nil))
