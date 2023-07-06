(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-open-stream-ast) environment)
  (declare (ignore client environment))
  (node* (:let)
    (1 :binding
       (node* (:value-binding)
         (1 :variable (ico:var-ast ast))
         (1 :value (ico:stream-ast ast))))
    (* :declaration (ico:declaration-asts ast))
    (1 :form
       (node* (:unwind-protect)
         (1 :protected
            (node* (:progn) (* :form (ico:form-asts ast))))
         (1 :form
            (node* (:application)
              (1 :function-name (node* (:function-name :name 'close)))
              (1 :argument (ico:var-ast ast))))))))
