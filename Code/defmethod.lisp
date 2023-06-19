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
         (lambda-list (ico:unparse-lambda-list-ast lambda-list-ast))
         (specializers (extract-specializers lambda-list-ast))
         (declarations '())
         (forms (mapcar #'ico:form (ico:form-asts ast))))
    ;; FIXME: generate declarations and documentation
    (let* ((lambda-expression
             `(lambda ,lambda-list
                ,@declarations
                (block ,(if (consp function-name)
                            (second function-name)
                            function-name)
                  ,@forms)))
           (method-lambda
             (let ((arguments (gensym))
                   (next-methods (gensym)))
               `(lambda ,(arguments ,next-methods)
                  (flet ((next-method-p ()
                           (not (null ,next-methods)))
                         (call-next-method (&rest args)
                           (when (null ,next-methods)
                             (error "no next method"))
                           (funcall (method-function (car ,next-methods))
                                    (or args ,args)
                                    (cdr ,next-methods))))
                    (declare (ignorable #'next-method-p
                                        #'call-next-method))
                    (apply ,lambda-expression
                           ,args))))))
      `(add-method (ensure-generic-function ',function-name)
                   `(make-instance 'standard-method
                      :function ,method-lambda
                      :lambda-list ,lambda-list
                      :qualifiers ,qualifiers
                      :specializers ,specializers
                      :declarations ,declarations)))))
