(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:print-unreadable-object-ast) environment)
  (declare (ignore client environment))
  (let ((stream-var (gensym))
        (object-var (gensym)))
    (alet ((b (make-variable-name-ast stream-var) (ico:stream-ast ast))
           (b (make-variable-name-ast object-var) (ico:object-ast ast)))
      (application 'format (make-variable-name-ast stream-var) (aliteral '"#<"))
      (node* (:progn)
        (* :form
           (if (null (ico:type-ast ast))
               '()
               (list
                (application
                 'format
                 (make-variable-name-ast stream-var)
                 (aliteral '"~s ")
                 (application
                  'class-name
                  (application
                   'class-of
                   (make-variable-name-ast object-var)))))))
        (* :form
           (if (null (ico:identity-ast ast))
               '()
               (list
                (application
                 'format
                 (make-variable-name-ast stream-var)
                 ;; For now, just print ID.
                 (aliteral '"ID")))))
        (* :form (ico:form-asts ast)))
      (application 'format (make-variable-name-ast stream-var) (aliteral '">")))))
    
        

      
    
