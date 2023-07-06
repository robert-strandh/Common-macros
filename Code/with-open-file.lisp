(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-open-file-ast) environment)
  (declare (ignore client environment))
  (let ((abort-variable (gensym)))
    (node* (:let)
      (1 :binding
         (node* (:value-binding)
           (1 :variable (ico:stream-ast ast))
           (1 :value
              (node* (:application)
                (1 :function-name (node* (:function-name :name 'open)))
                (1 :argument (ico:filespec-ast ast))
                (* :argument (ico:option-asts ast)))))
         (make-let-binding-ast
          (make-variable-name-ast abort-variable)
          (node* (:unparsed :context :form :expression 't))))
      (* :declaration (ico:declaration-asts ast))
      (1 :form
         (node* (:unwind-protect)
           (1 :protected
              (node* (:multiple-value-prog1)
                (1 :values (node* (:progn) (* :form (ico:form-asts ast))))
                (1 :form
                   (node* (:setq)
                     (1 :variable (make-variable-name-ast abort-variable))
                     (1 :value
                        (node* (:unparsed :context :form :expression 'nil)))))))
           (1 :form
              (node* (:unless)
                (1 :test
                   (node* (:applicaton)
                     (1 :function-name (node* (:function-name :name 'null)))
                     (1 :argument (ico:stream-ast ast))))
                (1 :form
                   (node* (:application)
                     (1 :function-name (node* (:function-name :name 'close)))
                     (1 :argument (ico:stream-ast ast))
                     (1 :argument
                        (node* (:unparsed :context :form :expression ':abort)))
                     (1 :argument (make-variable-name-ast abort-variable)))))))))))
