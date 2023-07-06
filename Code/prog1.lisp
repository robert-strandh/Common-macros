(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog1-ast) environment)
  (declare (ignore environment))
  (let ((name (gensym)))
    (node* (:let)
      (1 :binding
         (node* (:value-binding)
           (1 :name (make-variable-name-ast name))
           (1 :value (ico:first-form-ast ast))))
      (* :form (ico:form-asts ast))
      (1 :form (make-variable-name-ast name)))))
