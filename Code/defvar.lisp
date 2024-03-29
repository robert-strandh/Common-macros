(cl:in-package #:common-macros)

;;; This code is inspried by the dictionary page on DEFVAR in the
;;; Common Lisp standard.

(defmethod expand (client (ast ico:defvar-ast) environment)
  (declare (ignore client environment))
  (let ((variable-name-ast (ico:variable-name-ast ast))
        (form-ast (ico:form-ast ast))
        (documentation-ast (ico:documentation-ast ast)))
    (aprogn
     (node* (:declaim)
       (1 :declaration-specifier
          (node* (:declaration-specifier :kind 'special)
            (1 :argument variable-name-ast))))
     (if (null form-ast)
         nil
         (node* (:setf)
           (1 :name variable-name-ast)
           (1 :value form-ast)))
     (if (null documentation-ast)
         nil
         (node* (:setf)
           (1 :place
              (application 'documentation variable-name-ast
                           (aquote 'variable)))
           (1 :value documentation-ast)))
      variable-name-ast)))
