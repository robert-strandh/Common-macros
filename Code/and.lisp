(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:and-ast) environment)
  (declare (ignore client environment))
  (let ((form-asts (ico:form-asts ast)))
    (cond ((null form-asts)
           (node* (:literal :literal 't)))
          ((null (rest form-asts))
           (first form-asts))
          (t
           (aif (first form-asts)
                (node* (:and) (* :form (rest form-asts)))
                (node* (:literal :literal 'nil)))))))
