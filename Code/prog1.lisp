(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:prog1-ast) environment)
  (declare (ignore client environment))
  (let ((name (gensym)))
    (alet ((b (make-variable-name-ast name) (ico:first-form-ast ast)))
      (ico:form-asts ast)
      (make-variable-name-ast name))))
