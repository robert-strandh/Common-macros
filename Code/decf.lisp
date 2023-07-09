(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:decf-ast) environment)
  (declare (ignore client))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast) environment)
    (let* ((variable-name-ast (first store-variable-asts))
           (delta-ast
             (if (null (ico:delta-ast ast))
                 (make-unparsed-form-ast '1)
                 (ico:delta-ast ast)))
           (application-ast
             (node* (:application)
               (1 :function-name (make-function-name-ast '-))
               (1 :argument read-ast)
               (1 :argument delta-ast))))
      (alet ((b variable-name-ast application-ast)
             binding-asts)
        store-ast))))
