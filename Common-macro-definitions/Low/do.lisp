(cl:in-package #:common-macro-definitions)

(defmacro do (variable-clauses end-test &body body)
  (do-dostar-expander 'let 'psetq variable-clauses end-test body))
