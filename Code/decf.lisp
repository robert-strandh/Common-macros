(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:decf-ast) environment)
  (multiple-value-bind
        (temporary-asts form-asts store-variable-asts store-ast read-ast)
      (ast-setf-expansion client (ico:place-ast ast) environment)
    (let ((delta-ast (if (null (ico:delta-ast ast))
                         (make-instance 'ico:literal-ast :literal '1)
                         (ico:delta-ast ast))))
      (make-instance 'ico:let-ast
        :binding-asts
        (mapcar #'make-let-binding-ast temporary-asts form-asts)
        :form-asts
        (list (make-instance 'ico:let-ast
                :binding-asts
                (list (make-let-binding-ast
                       (first store-variable-asts)
                       (application '- read-ast delta-ast)))
                :form-asts (list store-ast)))))))
