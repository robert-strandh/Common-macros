(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:unless-ast) environment)
  (declare (ignore client environment))
  (node* (:if)
    (1 :test (ico:test-ast ast))
    (1 :then (make-unparsed-form-ast 'nil))
    (1 :else (aprogn (ico:form-asts ast)))))
