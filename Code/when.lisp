(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:when-ast) environment)
  (declare (ignore client environment))
  (aif (ico:test-ast ast)
       (aprogn (ico:form-asts ast))
       (make-unparsed-form-ast 'nil)))

(defmethod unparse-expand (builder (ast ico:when-ast))
  `(if ,(ses:unparse builder t (ico:test-ast ast))
       (progn ,@(loop for ast in (ico:form-asts ast)
                      collect (ses:unparse builder t ast)))
       nil))
