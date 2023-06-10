(cl:in-package #:common-macros)

(defmacro cmd:psetq (&rest pairs)
  `(psetf ,@pairs))
