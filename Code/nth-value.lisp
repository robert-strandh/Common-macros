(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:nth-value-ast) environment)
  (declare (ignore client environment))
  (application
   'nth
   (ico:n-ast ast)
   (application 'multiple-value-list (ico:form-ast ast))))
