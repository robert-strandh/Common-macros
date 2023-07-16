(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:defun-ast) environment)
  (declare (ignore client environment))
  (let* ((name (ico:name (ico:name-ast ast)))
         (block-name (if (symbolp name) name (second name))))
    (aprogn
     (node* (:eval-when)
       (* :situation
          (make-eval-when-situation-asts :compile-toplevel))
       ;; Add compile-time stuff here.
       )
     (node* (:eval-when)
       (* :situation
          (make-eval-when-situation-asts :load-toplevel :execute))
       (1 :form
          (node* (:setf)
            (1 :place
               (application 'fdefinition (aquote name)))
            (1 :value 
               (node* (:lambda)
                 (1 :lambda-list (ico:lambda-list-ast ast))
                 (* :declaration (ico:declaration-asts ast))
                 (abp:? :documentation (ico:documentation-ast ast))
                 (1 :form (ablock block-name (ico:form-asts ast)))))))
       (1 :form (aliteral name))))))
