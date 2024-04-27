(cl:in-package #:common-macro-definitions)

(defmacro destructuring-bind (lambda-list form &body body)
  (ecc:parse-destructuring-bind lambda-list form body))
