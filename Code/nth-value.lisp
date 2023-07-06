(cl:in-package #:common-macros)

(defmethod expand ((ast ico:nth-value-ast) environment)
  (declare (ignore environment))
  (with-builder
    (node* (:application)
      (1 :function-name (node* (:function-name :name 'nth)))
      (1 :argument (ico:n-ast ast))
      (1 :argument
         (node* (:application)
           (1 :function-name
              (node* (:function-name :name 'multiple-value-list)))
           (1 :argument (ico:form-ast ast)))))))
