(cl:in-package #:common-macro-definitions)

(defmacro psetq (&rest pairs)
  `(psetf ,@pairs))
