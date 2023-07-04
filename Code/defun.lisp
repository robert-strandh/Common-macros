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
                (list 
                 (abp:node* (:eval-when-situation :situation :load-toplevel))
                 (abp:node* (:eval-when-situation :situation :execute))))
             (* :form
                (list 
                 (abp:node* (:setf)
                   (1 :place
                      (abp:node* (:place)
                        (* :place
                           (abp:node* (:application)
                             (1 :function-name
                                (abp:node* (:function-name :name 'fdefinitions)))
                             (* :argument
                                (abp:node* (:quote)
                                  (1 :object
                                     (abp:node* (:literal :value name)))))))))
                   (1 :new-value 
                      (abp:node* (:lambda
                                   :lambda-list-ast (ico:lambda-list-ast ast)
                                   :declaration-asts (ico:declaration-asts ast)
                                   :documentation-ast (ico:documentation-ast ast))
                        (1 :form
                           (abp:node* (:block :form-asts (ico:form-asts ast))
                             (1 :block-name
                                (abp:node* (:block-name :name block-name))))))))
                 (abp:node* (:literal :literal name))))))))))
