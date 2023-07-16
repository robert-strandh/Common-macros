(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:case-ast) environment)
  (declare (ignore client environment))
  (let ((variable-name (gensym)))
    (flet ((make-cond-clause-ast (case-clause-ast)
             (if (typep case-clause-ast 'ico:case-normal-clause-ast)
                 (node* (:cond-clause)
                   (1 :test
                      (node* (:or)
                        (* :form
                           (loop for key-ast in (ico:key-asts case-clause-ast)
                                 collect
                                 (application
                                  'eql
                                  (make-variable-name-ast variable-name)
                                  (aquote (ico:key key-ast)))))))
                   (* :form (ico:form-asts case-clause-ast)))
                 (node* (:cond-clause)
                   (1 :test (aquote 't))
                   (* :form (ico:form-asts case-clause-ast))))))
      (alet ((b (make-variable-name-ast variable-name)
                (ico:keyform-ast ast)))
        (node* (:cond)
          (* :clause (mapcar #'make-cond-clause-ast (ico:clause-asts ast))))))))
