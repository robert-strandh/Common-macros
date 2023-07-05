(cl:in-package #:common-macros)

(defmethod expand ((ast ico:decf-ast) environment)
  (let ((place (ico:place (ico:place-ast ast))))
    (with-builder
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion place environment)
        (let* ((variable-name-ast
                 (node* (:variable-name
                         :name (first store-vars))))
               (delta-ast
                 (if (null (ico:delta-ast ast))
                     (node* (:unparsed
                             :context :form
                             :expression '1))
                     (ico:delta-ast ast)))
               (reader-form-ast
                 (node* (:unparsed
                         :context :form
                         :expression reader-form)))
               (writer-form-ast
                 (node* (:unparsed
                         :context :form
                         :expression writer-form)))
               (application-ast
                 (node* (:application)
                   (1 :function-name
                      (node* (:function-name :name '-)))
                   (1 :argument reader-form-ast)
                   (1 :argument delta-ast)))
               (let-binding-ast
                 (make-let-binding-ast variable-name-ast application-ast)))
          (if (null vars)
              (node* (:let)
                (1 :binding let-binding-ast)
                (1 :form writer-form-ast))
              (node* (:let)
                (1 :binding let-binding-ast)
                (* :binding
                   (loop for var in vars
                         for val in vals
                         for variable-name-ast
                           = (node* (:variable-name
                                     :name var))
                         for value-ast
                           = (node* (:unparsed
                                     :context :form
                                     :expression val))
                         collect
                         (make-let-binding-ast variable-name-ast value-ast)))
                (1 :form writer-form-ast))))))))
