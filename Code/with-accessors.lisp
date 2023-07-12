(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-accessors-ast) environment)
  (declare (ignore client environment))
  (let ((instance-var (gensym)))
    (alet ((b (make-variable-name-ast instance-var)
              (ico:instance-form-ast ast)))
      (node* (:symbol-macrolet)
        (* :symbol-expansion
           (loop for slot-entry-ast in (ico:slot-entry-asts ast)
                 collect
                 (node* (:symbol-expansion)
                   (1 :symbol (ico:variable-name-ast slot-entry-ast))
                   (1 :expansion
                      (node* (:application)
                        (1 :function-name
                           (ico:accessor-name-ast slot-entry-ast))
                        (1 :argument
                           (make-variable-name-ast instance-var)))))))
        (* :declaration (ico:declaration-asts ast))
        (* :form (ico:form-asts ast))))))
