(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:and-ast) environment)
  (declare (ignore client environment))
  (let ((form-asts (ico:form-asts ast)))
    (cond ((null form-asts)
           (aliteral 't))
          ((null (rest form-asts))
           (first form-asts))
          (t
           (aif (first form-asts)
                (node* (:and) (* :form (rest form-asts)))
                (aliteral 'nil))))))

(defmethod unparse-expand (builder (ast ico:and-ast))
  (let ((form-asts (ico:form-asts ast)))
    (labels ((unparse-form-asts (form-asts)
               (let ((first-form (ses:unparse builder t (first form-asts)))
                     (rest-form-asts (rest form-asts)))
                 (if (null rest-form-asts)
                     first-form
                     `(if ,first-form
                          ,(unparse-form-asts rest-form-asts)
                          nil)))))
      (if (null form-asts)
          't
          (unparse-form-asts form-asts)))))
