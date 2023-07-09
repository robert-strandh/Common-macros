(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog*-ast) environment)
  (declare (ignore client environment))
  (ablock 'nil
    (alet ((ico:binding-asts ast))
      (ico:declaration-asts ast)
      (node* (:tagbody) (* :segment (ico:segment-asts ast))))))
