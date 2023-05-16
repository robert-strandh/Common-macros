(cl:in-package #:common-macros)

(defmacro return (form)
  `(return-from nil ,form))
