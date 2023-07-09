(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:multiple-value-list-ast) environment)
  (declare (ignore client environment))
  (application
   'multiple-value-call
   (node* (:function) (1 :name (node* (:function-name :name 'list))))
   (ico:form-ast ast)))
