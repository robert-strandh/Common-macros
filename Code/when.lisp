(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:when-ast) environment)
  (declare (ignore client environment))
  (node* (:if)
    (1 :test (ico:test-ast ast))
    (1 :then (aprogn (ico:form-asts ast)))
    (1 :else (make-unparsed-form-ast 'nil))))
