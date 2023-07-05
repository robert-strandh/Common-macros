(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog2-ast) environment)
  (declare (ignore environment))
  (let ((origin (ico:origin ast))
        (variable-name (gensym)))
    (flet ((make-variable-name-ast ()
             (abp:node* (:variable-name
                         :source origin
                         :name variable-name))))
      (abp:with-builder ((make-instance 'bld:builder))
        (abp:node* (:progn)
          (1 :form (ico:first-form-ast ast))
          (1 :form
             (abp:node* (:let :source origin)
               (1 :binding
                  (abp:node* (:value-binding :source origin)
                    (1 :name (make-variable-name-ast))
                    (1 :value (ico:second-form-ast ast))))
               (* :form (ico:form-asts ast))
               (1 :form (make-variable-name-ast)))))))))
