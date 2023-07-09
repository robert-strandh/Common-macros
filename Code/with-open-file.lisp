(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:with-open-file-ast) environment)
  (declare (ignore client environment))
  (let ((abort-variable (gensym)))
    (alet ((b (ico:stream-ast ast)
              (node* (:application)
                (1 :function-name (make-function-name-ast 'open))
                (1 :argument (ico:filespec-ast ast))
                (* :argument (ico:option-asts ast))))
           (b (make-variable-name-ast abort-variable)
              (make-unparsed-form-ast 't)))
      (ico:declaration-asts ast)
      (node* (:unwind-protect)
        (1 :protected
           (node* (:multiple-value-prog1)
             (1 :values (node* (:progn) (* :form (ico:form-asts ast))))
             (1 :form
                (node* (:setq)
                  (1 :variable (make-variable-name-ast abort-variable))
                  (1 :value (make-unparsed-form-ast 'nil))))))
        (1 :form
           (node* (:unless)
             (1 :test
                (node* (:applicaton)
                  (1 :function-name (make-function-name-ast 'null))
                  (1 :argument (ico:stream-ast ast))))
             (1 :form
                (application
                 'close
                 (ico:stream-ast ast)
                 (make-unparsed-form-ast ':abort)
                 (make-variable-name-ast abort-variable)))))))))
