(cl:in-package #:common-macros)

(defmethod expand ((ast ico:when-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (abp:with-builder ((make-instance 'builder))
      (node* (:if)
        (1 :test (ico:test-ast ast))
        (1 :then (node* (:progn) (* :form (ico:form-asts ast))))
        (1 :else (node* (:unparsed :expression 'nil)))))))
