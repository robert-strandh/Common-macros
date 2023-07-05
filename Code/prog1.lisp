(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog1-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (let ((variable-name (gensym)))
      (flet ((make-variable-name-ast ()
               (node* (:variable-name :name variable-name))))
        (abp:with-builder ((make-instance 'bld:builder))
          (node* (:let)
            (1 :binding
               (node* (:value-binding)
                 (1 :name (make-variable-name-ast))
                 (1 :value (ico:first-form-ast ast))))
            (* :form (ico:form-asts ast))
            (1 :form (make-variable-name-ast))))))))
