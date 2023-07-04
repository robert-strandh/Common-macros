(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog1-ast))
  (let ((form-asts (ico:form-asts ast))
        (origin (ico:origin ast))
        (variable-name (gensym)))
    (flet ((make-variable-name-ast ()
             (abp:node* (:variable-name
                         :source origin
                         :name variable-name))))
    (abp:with-builder ((make-instance 'bld:builder))
      (abp:node* (:let :source origin)
        (1 :binding
           (abp:node* (:value-binding :source origin)
             (1 :name (make-variable-name-ast))
             (1 :value (first form-asts))))
        (* :form (rest form-asts))
        (1 :form (make-variable-name-ast)))))))
