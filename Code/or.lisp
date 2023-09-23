(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:or-ast) environment)
  (declare (ignore client environment))
  (let ((form-asts (ico:form-asts ast)))
    (cond ((null form-asts)
           (aliteral 'nil))
          ((null (rest form-asts))
           (first form-asts))
          (t
           (let ((name (gensym)))
             (alet ((b (make-variable-name-ast name) (first form-asts)))
               (aif (make-variable-name-ast name)
                    (make-variable-name-ast name)
                    (node* (:or) (* :form (rest form-asts))))))))))

(defmethod unparse-expand (builder (ast ico:or-ast))
  (let ((form-asts (ico:form-asts ast)))
    (if (null form-asts)
        'nil
        (labels ((unparse-form-asts (form-asts)
                   (let ((first-form
                           (ses:unparse builder t (first form-asts)))
                         (rest-form-asts (rest form-asts))
                         (temp (gensym)))
                     (if (null rest-form-asts)
                         first-form
                         `(let ((,temp ,first-form))
                            (if ,temp
                                ,temp
                                ,(unparse-form-asts rest-form-asts)))))))
          (unparse-form-asts form-asts)))))
