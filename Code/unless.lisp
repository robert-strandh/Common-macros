(cl:in-package #:common-macros)

(defmacro unless (test &rest forms)
  `(if ,test
       nil
       (progn ,@forms)))
