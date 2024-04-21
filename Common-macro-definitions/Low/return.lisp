(cl:in-package #:common-macro-definitions)

(defmacro return (&optional form)
  `(return-from nil ,form))
