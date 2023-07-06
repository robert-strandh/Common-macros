(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:in-package-ast) environment)
  (declare (ignore client environment))
  (node* (:eval-when)
    (* :situation
       (make-eval-when-situation-asts
        :compile-toplevel :load-toplevel :execute))
    (1 :form
       (node* (:setq)
         (1 :variable-name (make-variable-name-ast '*package*))
         (1 :value
            (node* (:application)
              (1 :function-name
                 (node* (:function-name :name 'find-package)))
              (1 :argument
                 (node* (:quote)
                   (1 :object (ico:name (ico:name-ast ast)))))))))))
