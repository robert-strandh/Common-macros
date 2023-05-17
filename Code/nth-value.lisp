(cl:in-package #:common-macros)

(defmacro cmd:nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
