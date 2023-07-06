(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:pop-ast) environment)
  (declare (ignore environment))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast))
    (node* (:let*)
      (* :binding binding-asts)
      (1 :binding
         (make-let-binding-ast (first store-variable-asts) read-ast))
      (1 :form
         (node* (:prog1)
           (1 :first (first store-variable-asts))
           (1 :form
              (node* (:setq)
                (1 :name (first store-variable-asts))
                (1 :value
                   (node* (:application)
                     (1 :function-name :name 'cdr)
                     (1 :argument (first store-variable-asts))))))
           (1 :form store-ast))))))
