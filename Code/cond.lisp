(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:cond-ast) environment)
  (declare (ignore client environment))
  (let ((clause-asts (ico:clause-asts ast)))
    (if (null clause-asts)
        (make-unparsed-form-ast 'nil)
        (with-ast-origin (first clause-asts)
          (let* ((first-clause-ast (first clause-asts))
                 (test-ast (ico:test-ast first-clause-ast))
                 (form-asts (ico:form-asts first-clause-ast))
                 (remaining-cond-ast
                   (node* (:cond) (* :clause (rest clause-asts)))))
            (if (null form-asts)
                (node* (:or) (* :form (list test-ast remaining-cond-ast)))
                (aif test-ast
                     (aprogn form-asts)
                     remaining-cond-ast)))))))
