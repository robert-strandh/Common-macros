(cl:in-package #:common-macros)

(defmacro when (test &rest forms)
  `(if ,test
       (progn ,@forms)))
