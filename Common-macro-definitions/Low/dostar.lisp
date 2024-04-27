(cl:in-package #:common-macro-definitions)

(defmacro do* (variable-clauses end-test &body body)
  (do-dostar-expander 'let* 'setq variable-clauses end-test body))
