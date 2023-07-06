(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:ignore-errors-ast) environment)
  (declare (ignore environment))
  (node* (:handler-case)
    (* :form (ico:form-asts ast))
    (1 :clause
       (node* (:handler-clause)
         (1 :type
            (node* (:atomic-type-specifier)
              (1 :name (node* (:name :name 'error)))))
         (1 :variable (make-variable-name-ast 'condition))
         (1 :form
            (node* (:appliation)
              (1 :function (node* (:function-name :name 'values)))
              (1 :argument
                 (node* (:unparsed :context :form :expression nil)))
              (1 :argument (make-variable-name-ast 'condition))))))))
