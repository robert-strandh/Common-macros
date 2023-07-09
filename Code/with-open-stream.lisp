(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-open-stream-ast) environment)
  (declare (ignore client environment))
  (alet ((b (ico:var-ast ast) (ico:stream-ast ast)))
    (ico:declaration-asts ast)
    (node* (:unwind-protect)
      (1 :protected
         (node* (:progn) (* :form (ico:form-asts ast))))
      (1 :form (application 'close (ico:var-ast ast))))))
