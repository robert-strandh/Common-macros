(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:unless-ast) environment)
  (declare (ignore client environment))
  (aif (ico:test-ast ast)
       (make-unparsed-form-ast 'nil)
       (aprogn (ico:form-asts ast))))
