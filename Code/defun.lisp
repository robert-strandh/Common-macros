(cl:in-package #:common-macros)

(defmacro cmd:defun (&whole form name lambda-list &rest body)
  (declare (ignore name lambda-list body))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defun))
         (ast (ses:parse builder syntax form)))
    `(setf (fdefinition ',(ico:name (ico:name-ast ast)))
           (lambda ,(ico:unparse-lambda-list-ast (ico:lambda-list-ast ast))
             ;; FIXME: generate declarations and documentation.
             ,@(mapcar #'ico:form (ico:form-asts ast))))))
