(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:when-ast) environment)
  (declare (ignore client environment))
  (aif (ico:test-ast ast)
       (aprogn (ico:form-asts ast))
       (make-unparsed-form-ast 'nil)))
