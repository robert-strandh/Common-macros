(cl:in-package #:common-macros)

(defmethod expand ((ast ico:defun-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (let* ((name (ico:name (ico:name-ast ast)))
           (block-name (if (symbolp name) name (second name))))
      (with-builder
        (node* (:progn)
          (1 :form
             (node* (:eval-when)
               (* :situation
                  (make-eval-when-situation-asts :compile-toplevel))
               ;; Add compile-time stuff here.
               ))
          (1 :form
             (node* (:eval-when)
               (* :situation
                  (make-eval-when-situation-asts :load-toplevel :execute))
               (* :form
                  (list 
                   (node* (:setf)
                     (1 :place
                        (node* (:unparsed
                                :context :place
                                :expression 
                                (node* (:application)
                                  (1 :function-name
                                     (node* (:function-name
                                             :name 'fdefinitions)))
                                  (1 :argument
                                     (make-quote-ast name))))))
                     (1 :value 
                        (node* (:lambda)
                          (1 :lambda-list (ico:lambda-list-ast ast))
                          (* :declaration (ico:declaration-asts ast))
                          (abp:? :documentation (ico:documentation-ast ast))
                          (1 :form
                             (wrap-in-block-ast
                              block-name (ico:form-asts ast))))))
                   (node* (:literal :literal name)))))))))))
