(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:multiple-value-setq-ast) environment)
  (declare (ignore client environment))
  (node* (:application)
    (1 :function-name (node* (:function-name :name 'values)))
    (1 :argument
       (node* (:setf)
         (1 :place
            (node* (:place)
              (1 :place
                 (node* (:appliication)
                   (1 :function-name (node* (:function-name :name 'values)))
                   (* :argument (ico:name-asts ast))))
              (1 :form (ico:form-ast ast))))))))
