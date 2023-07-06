(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:or-ast) environment)
  (declare (ignore environment))
  (let ((form-asts (ico:form-asts ast)))
    (cond ((null form-asts)
           (node* (:literal :literal 'nil)))
          ((null (rest form-asts))
           (first form-asts))
          (t
           (let ((name (gensym)))
             (node* (:let)
               (1 :binding
                  (make-let-binding-ast
                   (make-variable-name-ast name) (first form-asts)))
               (1 :form
                  (node* (:if)
                    (1 :test (make-variable-name-ast name))
                    (1 :then (make-variable-name-ast name))
                    (1 :else (node* (:or)
                               (* :form (rest form-asts))))))))))))
