(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:ignore-errors-ast) environment)
  (declare (ignore client environment))
  (node* (:handler-case)
    (* :form (ico:form-asts ast))
    (1 :clause
       (node* (:handler-clause)
         (1 :type
            (node* (:atomic-type-specifier)
              (1 :name (node* (:name :name 'error)))))
         (1 :variable (make-variable-name-ast 'condition))
         (1 :form
            (node* (:application)
              (1 :function (make-function-name-ast 'values))
              (1 :argument (make-unparsed-form-ast 'nil))
              (1 :argument (make-variable-name-ast 'condition))))))))
