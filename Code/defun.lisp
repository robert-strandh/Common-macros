(cl:in-package #:common-macros)

(defmethod expand ((ast ico:defun-ast))
  (let* ((name (ico:name (ico:name-ast ast)))
         (block-name (if (symbolp name) name (second name))))
    (abp:with-builder ((make-instance 'builder))
      (abp:node* (:progn)
        (* :form
           (abp:node* (:eval-when)
             (* :situation
                (abp:node* (:eval-when-situation :situation :compile-toplevel)))
             ;; Add compile-time stuff here.
             )
           (abp:node* (:eval-when)
             (* :situation
                (make-eval-when-situation-asts
                 origin :load-toplevel :execute))
             (* :form
                (list 
                 (abp:node* (:setf)
                   (1 :place
                      (abp:node* (:place)
                        (* :place
                           (abp:node* (:application)
                             (1 :function-name
                                (abp:node* (:function-name :name 'fdefinitions)))
                             (* :argument (make-quote-ast origin name))))))
                   (1 :new-value 
                      (abp:node* (:lambda
                                   :lambda-list-ast (ico:lambda-list-ast ast)
                                   :declaration-asts (ico:declaration-asts ast)
                                   :documentation-ast (ico:documentation-ast ast))
                        (1 :form
                           (wrap-in-block-ast
                            origin block-name (ico:form-asts ast))))))
                 (abp:node* (:literal :literal name))))))))))
