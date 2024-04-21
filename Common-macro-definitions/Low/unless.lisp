(cl:in-package #:common-macro-definitions)

(defmacro unless (test &body forms)
  `(if ,test
       nil
       (progn ,@forms)))
