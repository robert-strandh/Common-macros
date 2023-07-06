(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:multiple-value-list-ast) environment)
  (declare (ignore client environment))
  (node* (:application)
    (1 :function-name (node* (:function-name :name 'multiple-value-call)))
    (1 :argument
       (node* (:function) (1 :name (node* (:function-name :name 'list)))))
    (1 :argument (ico:form-ast ast))))
