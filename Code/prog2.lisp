(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog2-ast) environment)
  (declare (ignore client environment))
  (let ((name (gensym)))
    (aprogn 
     (ico:first-form-ast ast)
     (alet ((b (make-variable-name-ast name) (ico:second-form-ast ast)))
       (ico:form-asts ast)
       (make-variable-name-ast name)))))
