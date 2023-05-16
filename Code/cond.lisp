(cl:in-package #:common-macros)

(defmacro cond (&whole form &rest clauses)
  (declare (ignore clauses))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'cl:cond))
         (form-to-parse (cons 'cl:cond (cdr form)))
         (ast (ses:parse builder syntax form-to-parse)))
    (labels ((process-clauses (clause-asts)
               (if (null clause-asts)
                   'nil
                   (let* ((clause-ast (first clause-asts))
                          (test-ast (ico:test-ast clause-ast))
                          (test-form (ico:form test-ast))
                          (form-asts (ico:form-asts clause-ast))
                          (forms (mapcar #'ico:form form-asts)))
                     (if (null form-asts)
                         `(or ,test-form
                              ,(process-clauses (rest clause-asts)))
                         `(if ,test-form
                              (progn ,@forms)
                              ,(process-clauses (rest clause-asts))))))))
      (process-clauses (ico:clause-asts ast)))))

