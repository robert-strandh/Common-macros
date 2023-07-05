(cl:in-package #:common-macros)

(defmethod expand ((ast ico:or-ast) environment)
  (declare (ignore environment))
  (with-ast-origin ast
    (let ((form-asts (ico:form-asts ast)))
      (with-builder
        (cond ((null form-asts)
               (node* (:literal :literal 'nil)))
              ((null (rest form-asts))
               (first form-asts))
              (t
               (let ((variable-name (gensym)))
                 (flet ((make-variable-name-ast ()
                          (node* (:variable-name :name variable-name))))
                   (node* (:let)
                     (1 :binding
                        (make-let-binding-ast
                         (make-variable-name-ast) (first form-asts)))
                     (1 :form
                        (node* (:if)
                          (1 :test (make-variable-name-ast))
                          (1 :then (make-variable-name-ast))
                          (1 :else (node* (:or)
                                     (* :form (rest form-asts)))))))))))))))
