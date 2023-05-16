(cl:in-package #:common-macros)

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
