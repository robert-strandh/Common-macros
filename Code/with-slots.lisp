(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-slots-ast) environment)
  (let ((instance-var (gensym)))
    (alet ((b (make-variable-name-ast instance-var)
              (ico:instance-form-ast ast)))
      (node* (:symbol-macrolet)
        (* :symbol-expansion-asts
           (loop for slot-entry-ast in (ico:slot-entry-asts ast)
                 for slot-name-ast
                   = (ico:slot-name-ast slot-entry-ast)
                 collect
                 (node* (:symbol-expansion)
                   (1 :symbol (ico:variable-name-ast slot-entry-ast))
                   (1 :expansion
                      (application
                       'slot-value
                       (make-variable-name-ast instance-var)
                       (make-quote-ast (ico:name slot-name-ast)))))))
        (* :declaration (ico:declaration-asts ast))
        (* :form (ico:form-asts ast)))))))
      
