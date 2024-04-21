(cl:in-package #:common-macro-definitions)

(defmacro multiple-value-list (form)
  `(multiple-value-call #'list ,form))
