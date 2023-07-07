(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-output-to-string-ast) environment)
  (declare (ignore client environment))
  (with-accessors ((string-ast ico:string-ast)
                   (element-type-ast ico:element-type-ast))
      ast
    (node* (:with-open-stream)
      (1 :var (ico:var-ast ast))
      (1 :stream
         (node* (:application)
           (1 :function-name
              (node* (:function-name :name 'make-string-output-stream)))
           (* :argument (if (null string-ast)
                            '()
                            (list (make-unparsed-form-ast ':string)
                                  string-ast)))
           (* :argument (if (null element-type-ast)
                            '()
                            (list (make-unparsed-form-ast ':element-type)
                                  element-type-ast)))))
      (* :declaration (ico:declaration-asts ast))
      (* :form (ico:form-asts ast)))))           
