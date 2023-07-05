(cl:in-package #:common-macros)

(defmethod expand ((ast ico:and-ast) environment)
  (declare (ignore environment))
  (let ((form-asts (ico:form-asts ast)))
    (with-builder
      (cond ((null form-asts)
             (node* (:literal :literal 't)))
            ((null (rest form-asts))
             (first form-asts))
            (t
             (node* (:if)
               (1 :test (first form-asts))
               (1 :then (node* (:and) (* :form (rest form-asts))))
               (1 :else (node* (:literal :literal 'nil)))))))))
