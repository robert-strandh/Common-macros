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

(defun unparse-form-asts (builder form-asts)
  `(progn ,@(loop for form-ast in form-asts
                  collect (ses:unparse builder t form-ast))))

(defmethod unparse-expand (builder (ast ico:cond-ast))
  (let ((clause-asts (ico:clause-asts ast)))
    (if (null clause-asts)
        'nil
        (labels ((expand-clause-asts (clause-asts)
                   (let* ((first-clause-ast (first clause-asts))
                          (rest-clause-asts (rest clause-asts))
                          (test-form-ast (ico:test-ast first-clause-ast))
                          (test-form (ses:unparse builder t test-form-ast))
                          (form-asts (ico:form-asts first-clause-ast))
                          (temp (gensym)))
                     (if (null rest-clause-asts)
                         `(let ((,temp ,test-form))
                            ,(if (null form-asts)
                                 temp
                                 `(if ,temp 
                                      ,(unparse-form-asts builder form-asts)
                                      nil)))
                         `(let ((,temp ,test-form))
                            (if ,temp
                                ,(if (null form-asts)
                                     temp
                                     (unparse-form-asts builder form-asts))
                                ,(expand-clause-asts rest-clause-asts)))))))
          (expand-clause-asts clause-asts)))))
