(cl:in-package #:common-macros)

;;; Do this when the structure for lambda lists is more
;;; stable in the s-expression-syntax library.
(defun generate-specialized-lambda-list (lambda-list-ast)
  (declare (ignore lambda-list-ast)))

(defun extract-specializers (lambda-list-ast)
  (declare (ignore lambda-list-ast)))

(defmacro cmd:defmethod
    (&whole form
     &environment environment
     &rest arguments)
  (declare (ignore arguments))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defmethod))
         (ast (ses:parse builder syntax form))
         (function-name (ico:name (ico:name-ast ast)))
         (qualifier-asts (ico:method-qualifier-asts ast))
         (qualifiers (mapcar #'ico:name qualifier-asts))
         (lambda-list-ast (ico:lambda-list-ast ast))
         (lambda-list (generate-specialized-lambda-list lambda-list-ast))
         (specializers (extract-specializers lambda-list-ast))
         (declarations '())
         (forms (mapcar #'ico:form (ico:form-asts ast))))
    (multiple-value-bind
          (generic-function-class-name method-class-name)
        (cmm:generic-function-class-names function-name environment)
      ;; FIXME: generate declarations and documentation
      (let ((method-lambda
              (cmm:make-method-lambda
               (cmm:class-prototype
                (find-class generic-function-class-name))
               (cmm:class-prototype
                (find-class method-class-name))
               `(lambda ,lambda-list
                  ,@declarations
                  (block ,(if (consp function-name)
                              (second function-name)
                              function-name)
                    ,@forms))
               environment)))
        `(cmm:ensure-method
          ',function-name
          :method-class ',method-class-name
          :lambda-list ',lambda-list
          :qualifiers ',qualifiers
          :specializers ',specializers
          :function ,method-lambda)))))
