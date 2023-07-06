(cl:in-package #:common-macros)

;;; This code is inspried by the dictionary page on DEFVAR in the
;;; Common Lisp standard.

(defmethod expand (client (ast ico:defvar-ast) environment)
  (declare (ignore client environment))
  (let ((variable-name-ast (ico:variable-name-ast ast))
        (form-ast (ico:form-ast ast))
        (documentation-ast (ico:documentation-ast ast)))
    (node* (:progn)
      (1 :form
         (node* (:declaim)
           (1 :declaration-specifier
              (node* (:declaration-specifier :kind 'special)
                (1 :argument variable-name-ast)))))
      (abp:? :form
            (if (null form-ast)
                nil
                (node* (:setf)
                  (1 :name variable-name-ast)
                  (1 :value form-ast))))
      (abp:? :form
             (if (null documentation-ast)
                 nil
                 (node* (:setf)
                   (1 :place
                      (node* (:place)
                        (1 :place
                           (node* (:application)
                             (1 :function-name
                                (node* (:function-name :name 'documentation)))
                             (1 :argument variable-name-ast)
                             (1 :argument (node* (:quote :object 'variable)))))))
                   (1 :value documentation-ast))))
      (1 :form variable-name-ast))))
