(cl:in-package #:common-macros)

;;; This definition is from the Common Lisp standard.  The STRING
;;; argument is not used.
(defmethod expand (client (ast ico:check-type-ast) environment)
  (node* (:assert)
    (1 :test-form
       (node* (:application)
         (1 :function-name (make-function-name-ast 'typep))
         (1 :argument (ico:place-ast ast))
         (1 :argument
            (make-quote-ast
             (ses:unparse
              (make-instance 'bld:builder) t (ico:typespec-ast ast))))))
    (1 :place (ico:place-ast ast))
    (1 :datum (make-quote-ast 'type-error))
    (1 :argument (make-unparsed-form-ast ':datum))
    (1 :argument (ico:place-ast ast))
    (1 :argument (make-unparsed-form-ast ':expected-type))
    (1 :argument 
       (make-quote-ast
        (ses:unparse
         (make-instance 'bld:builder) t (ico:typespec-ast ast))))))
