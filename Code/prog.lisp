(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog-ast) environment)
  (declare (ignore client environment))
  (wrap-in-block-ast
   'nil
   (list (node* (:let)
           (* :binding (ico:binding-asts ast))
           (* :declaration (ico:declaration-asts ast))
           (1 :form
              (node* (:tagbody) (* :segment (ico:segment-asts ast))))))))
