(cl:in-package #:common-macros)

(defmethod expand ((ast ico:return-ast))
  (let ((origin (ico:origin ast)))
    (abp:with-builder ((make-instance 'bld:builder))
      (abp:node* (:return-from :source origin)
        (1 :name (abp:node* (:block-name :source origin :name 'nil)))
        (? :result (ico:result-ast ast))))))
