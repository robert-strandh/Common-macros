(cl:in-package #:common-macros)

(defmacro cmd:return (form)
  `(return-from nil ,form))
