(cl:in-package #:common-macros)

(defmethod expand ((ast ico:unless-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (abp:with-builder ((make-instance 'builder))
      (node* (:if)
        (1 :test (ico:test-ast ast))
        (1 :then (node* (:unparsed :expression 'nil)))
        (1 :else (node* (:progn) (* :form (ico:form-asts ast))))))))
