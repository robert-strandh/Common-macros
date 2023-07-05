(cl:in-package #:common-macros)

(defmethod expand ((ast ico:decf-ast) environment)
  (let ((origin (ico:origin ast))
        (place (ico:place (ico:place-ast ast))))
    (abp:with-builder ((make-instance 'bld:builder))
      (multiple-value-bind (vars vals store-vars writer-form reader-form)
          (get-setf-expansion place environment)
        (let* ((variable-name-ast
                 (abp:node* (:variable-name
                             :source origin
                             :name (first store-vars))))
               (delta-ast
                 (if (null (ico:delta-ast ast))
                     (abp:node* (:unparsed
                                 :source origin
                                 :context :form
                                 :expression '1))
                     (ico:delta-ast ast)))
               (reader-form-ast
                 (abp:node* (:unparsed
                             :source origin
                             :context :form
                             :expression reader-form)))
               (writer-form-ast
                 (abp:node* (:unparsed
                             :source origin
                             :context :form
                             :expression writer-form)))
               (application-ast
                 (abp:node* (:application :source origin)
                   (1 :function-name
                      (abp:node* (:function-name :source origin :name '-)))
                   (1 :argument reader-form-ast)
                   (1 :argument delta-ast)))
               (let-binding-ast
                 (make-let-binding-ast
                  origin variable-name-ast application-ast)))
          (if (null vars)
              (abp:node* (:let :source origin)
                (1 :binding let-binding-ast)
                (1 :form writer-form-ast))
              (abp:node* (:let :source origin)
                (1 :binding let-binding-ast)
                (* :binding
                   (loop for var in vars
                         for val in vals
                         for variable-name-ast
                           = (abp:node* (:variable-name
                                         :source origin
                                         :name var))
                         for value-ast
                         = (abp:node* (:unparsed
                                       :source origin
                                       :context :form
                                       :expression val))
                         collect
                         (make-let-binding-ast
                          origin variable-name-ast value-ast)))
                (1 :form writer-form-ast))))))))
