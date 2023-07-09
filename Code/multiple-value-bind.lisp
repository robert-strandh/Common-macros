(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:multiple-value-bind-ast) environment)
  (declare (ignore client environment))
  (let ((rest (gensym)))
    (application
     'multiple-value-call
     (node* (:lambda)
       (1 :ordinary-lambda-list
          (node* (:ordinary-lambda-list)
            (1 :optional-section
               (node* (:optional-section)
                 (1 :keyword
                    (node* (:lambda-list-keyword :name '&optional)))
                 (* :parameter
                    (ico:variable-name-asts ast))))
            (1 :rest-section
               (node* (:rest-section)
                 (1 :keyword
                    (node* (:lambda-list-keyword :name '&rest)))
                 (1 :parameter (make-variable-name-ast rest))))))
       (1 :declaration
          (node* (:declaration)
            (1 :declaration-specifier
               (node* (:declaration-specifier :kind 'ignore)
                 (1 :name (make-variable-name-ast rest))))))
       (* :form (ico:form-asts ast)))
     (ico:values-ast ast))))
