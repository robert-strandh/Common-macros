(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:decf-ast) environment)
  (declare (ignore client))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast) environment)
    (let ((delta-ast
            (if (null (ico:delta-ast ast))
                (make-unparsed-form-ast '1)
                (ico:delta-ast ast))))
      (alet* (binding-asts
              (b (first store-variable-asts)
                 (application '+ read-ast delta-ast)))
        store-ast))))
