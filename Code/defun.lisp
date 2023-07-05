(cl:in-package #:common-macros)

(defmethod expand ((ast ico:defun-ast) environment)
  (declare (ignore environment))
  (let* ((name (ico:name (ico:name-ast ast)))
         (origin (ico:origin ast))
         (block-name (if (symbolp name) name (second name))))
    (abp:with-builder ((make-instance 'bld:builder))
      (abp:node* (:progn)
        (1 :form
           (abp:node* (:eval-when)
             (* :situation
                (make-eval-when-situation-asts origin :compile-toplevel))
             ;; Add compile-time stuff here.
             ))
        (1 :form
           (abp:node* (:eval-when)
             (* :situation
                (make-eval-when-situation-asts
                 origin :load-toplevel :execute))
             (* :form
                (list 
                 (abp:node* (:setf)
                   (1 :place
                      (abp:node* (:unparsed
                                  :context :place
                                  :expression 
                                  (abp:node* (:application)
                                    (1 :function-name
                                       (abp:node* (:function-name
                                                   :name 'fdefinitions)))
                                    (1 :argument
                                       (make-quote-ast origin name))))))
                   (1 :value 
                      (abp:node* (:lambda)
                        (1 :lambda-list (ico:lambda-list-ast ast))
                        (* :declaration (ico:declaration-asts ast))
                        (abp:? :documentation (ico:documentation-ast ast))
                        (1 :form
                           (wrap-in-block-ast
                            origin block-name (ico:form-asts ast))))))
                 (abp:node* (:literal :literal name))))))))))
