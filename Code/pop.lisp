(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:pop-ast) environment)
  (declare (ignore client))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast) environment)
    (alet* (binding-asts
            (b (first store-variable-asts) read-ast))
      (node* (:prog1)
        (1 :first (first store-variable-asts))
        (1 :form
           (node* (:setq)
             (1 :name (first store-variable-asts))
             (1 :value
                (application 'cdr (first store-variable-asts)))))
        (1 :form store-ast)))))
