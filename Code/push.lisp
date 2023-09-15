(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:push-ast) environment)
  (let* ((item-definition (make-instance 'ico:variable-definition-ast
                            :name (gensym)))
         (item-reference (make-variable-reference-ast item-definition)))
    (multiple-value-bind
          (temporary-asts form-asts store-variable-asts store-ast read-ast)
        (ast-setf-expansion client (ico:place-ast ast) environment)
      (make-instance 'ico:let-ast
        :binding-asts
        (cons (make-let-binding-ast item-definition (ico:item-ast ast))
              (mapcar #'make-let-binding-ast temporary-asts form-asts))
        :form-asts
        (make-instance 'ico:let-ast
          :binding-asts
          (list (make-let-binding-ast
                 (first store-variable-asts)
                 (application 'cons item-reference read-ast)))
          :form-asts (list store-ast))))))
