(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog2-ast) environment)
  (declare (ignore client environment))
  (let ((name (gensym)))
    (node* (:progn)
      (1 :form (ico:first-form-ast ast))
      (1 :form
         (alet ((b (make-variable-name-ast name) (ico:second-form-ast ast)))
           (ico:form-asts ast)
           (make-variable-name-ast name))))))
