(cl:in-package #:common-macros)

(defmacro cmd:when (test &rest forms)
  `(if ,test
       (progn ,@forms)))
