(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:typecase-ast) environment)
  (declare (ignore client environment))
  (let ((variable-name (gensym))
        ;; FIXME: store the builder in a special variable.
        (builder (make-instance 'bld:builder)))
    (flet ((make-cond-clause-ast (typecase-clause-ast)
             (if (typep typecase-clause-ast 'ico:typecase-normal-clause-ast)
                 (let* ((typespec-ast (ico:typespec-ast typecase-clause-ast))
                        (typespec (ses:unparse builder t typespec-ast)))
                   (node* (:cond-clause)
                     (1 :test
                        (application
                         'typep
                         (make-variable-name-ast variable-name)
                         (node* (:quote :object typespec))))
                     (* :form (ico:form-asts typecase-clause-ast))))
                 (node* (:cond-clause)
                   (1 :test (node* (:quote :object t)))
                   (* :form (ico:form-asts typecase-clause-ast))))))
      (alet ((b (make-variable-name-ast variable-name)
                (ico:keyform-ast ast)))
        (node* (:cond)
          (* :clause (mapcar #'make-cond-clause-ast (ico:clause-asts ast))))))))
