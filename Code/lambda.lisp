(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:lambda-ast) environment)
  (declare (ignore client environment))
  (node* (:function)
    (1 :name (node* (:lambda-expression)
               (1 :lambda-list (ico:lambda-list-ast ast))
               (* :form (ico:form-asts ast))))))
