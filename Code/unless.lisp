(cl:in-package #:common-macros)

(defmacro cmd:unless (test &rest forms)
  `(if ,test
       nil
       (progn ,@forms)))
