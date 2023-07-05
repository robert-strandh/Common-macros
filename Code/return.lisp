(cl:in-package #:common-macros)

(defmethod expand ((ast ico:return-ast) environment)
  (declare (ignore environment))
  (with-builder
    (node* (:return-from)
      (1 :name (node* (:block-name :name 'nil)))
      (abp:? :result (ico:result-ast ast)))))
