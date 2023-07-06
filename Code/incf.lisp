(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:decf-ast) environment)
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast))
    (let* ((variable-name-ast (first store-variable-asts))
           (delta-ast
             (if (null (ico:delta-ast ast))
                 (node* (:unparsed :context :form :expression '1))
                 (ico:delta-ast ast)))
           (application-ast
             (node* (:application)
               (1 :function-name
                  (node* (:function-name :name '+)))
               (1 :argument read-ast)
               (1 :argument delta-ast)))
           (let-binding-ast
             (make-let-binding-ast variable-name-ast application-ast)))
      (node* (:let)
        (1 :binding let-binding-ast)
        (* :binding binding-asts)
        (1 :form store-ast)))))
