(cl:in-package #:common-macros)

(defmethod expand ((ast ico:cond-ast) environment)
  (declare (ignore environment))
  (let ((clause-asts (ico:clause-asts ast)))
    (with-ast-origin ast
      (with-builder
        (if (null clause-asts)
            (node* (:unparsed :expression 'nil))
            (with-ast-origin (first clause-asts)
              (let* ((first-clause-ast (first clause-asts))
                     (test-ast (ico:test-ast first-clause-ast))
                     (form-asts (ico:form-asts first-clause-ast))
                     (remaining-cond-ast
                       (node* (:cond) (* :clause (rest clause-asts)))))
                (if (null form-asts)
                    (node* (:or)
                      (* :form (list test-ast remaining-cond-ast)))
                    (node* (:if)
                      (1 :test test-ast)
                      (1 :then (node* (:progn) (* :form form-asts)))
                      (1 :else remaining-cond-ast))))))))))
