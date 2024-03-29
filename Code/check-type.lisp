(cl:in-package #:common-macros)

;;; This definition is from the Common Lisp standard.  The STRING
;;; argument is not used.
(defmethod expand (client (ast ico:check-type-ast) environment)
  (let ((typespec-ast (unparse (ico:typespec-ast ast))))
    (node* (:assert)
      (1 :test-form
         (application 'typep (ico:place-ast ast) typespec-ast))
      (1 :place (ico:place-ast ast))
      (1 :datum (aquote 'type-error))
      (1 :argument (make-unparsed-form-ast ':datum))
      (1 :argument (ico:place-ast ast))
      (1 :argument (make-unparsed-form-ast ':expected-type))
      (1 :argument typespec-ast))))
