(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (abp:with-builder ((make-instance 'bld:builder))
      (wrap-in-block-ast
       'nil
       (list (node* (:let)
               (* :binding (ico:binding-asts ast))
               (* :declaration (ico:declaration-asts ast))
               (1 :form
                  (node* (:tagbody)
                    (* :segment (ico:segment-asts ast))))))))))
