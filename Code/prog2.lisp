(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog2-ast) environment)
  (declare (ignore environment))
  (let ((name (gensym)))
    (node* (:progn)
      (1 :form (ico:first-form-ast ast))
      (1 :form
         (node* (:let)
           (1 :binding
              (node* (:value-binding)
                (1 :name (make-variable-name-ast name))
                (1 :value (ico:second-form-ast ast))))
           (* :form (ico:form-asts ast))
           (1 :form (make-variable-name-ast name)))))))
