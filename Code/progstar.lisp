(cl:in-package #:common-macros)

(defmethod expand ((ast ico:prog-ast))
  (let ((origin (ico:origin ast)))
    (abp:with-builder ((make-instance 'bld:builder))
      (wrap-in-block-ast
       origin
       'nil
       (list (abp:node* (:let* :source origin)
               (* :binding (ico:binding-asts ast))
               (* :declaration (ico:declaration-asts ast))
               (1 :form
                  (abp:node* (:tagbody :source origin)
                    (* :segment (ico:segment-asts ast))))))))))
