(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:return-ast) environment)
  (declare (ignore client environment))
  (node* (:return-from)
    (1 :name (node* (:block-name :name 'nil)))
    (abp:? :result (ico:result-ast ast))))
