(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:unless-ast) environment)
  (declare (ignore client environment))
  (aif (ico:test-ast ast)
       (make-unparsed-form-ast 'nil)
       (aprogn (ico:form-asts ast))))

(defmethod unparse-expand (builder (ast ico:unless-ast))
  `(if ,(ses:unparse builder t (ico:test-ast ast))
       nil
       (progn ,@(loop for ast in (ico:form-asts ast)
                      collect (ses:unparse builder t ast)))))
