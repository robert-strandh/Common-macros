(cl:in-package #:common-macros)

(defmacro cmd:multiple-value-list (form)
  `(multiple-value-call #'list ,form))
