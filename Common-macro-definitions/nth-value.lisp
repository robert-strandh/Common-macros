(cl:in-package #:common-macro-definitions)

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
