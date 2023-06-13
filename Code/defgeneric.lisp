(cl:in-package #:common-macros)

;;; FIXME: include declarations.
;;; FIXME: include argument precedence order
;;; FIXME: handle method combination

(defmacro cmd:defgeneric
    (&whole form name lambda-list &rest options-or-methods)
  (declare (ignore name lambda-list options-or-methods))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defclass))
         (ast (ses:parse builder syntax form)))
    `(ensure-generic-function
      ,(ico:name (ico:name-ast ast))
      :lambda-list
      ',(ico:unparse-lambda-list-ast (ico:lambda-list-ast ast))
      ,@(let ((documentation-ast (ico:documentation-ast ast)))
          (if (null documentation-ast)
              '()
              (list :documentation
                    (ico:documentation documentation-ast))))
      ,@(let ((generic-function-class-ast
                (ico:generic-function-class-ast ast)))
          (if (null generic-function-class-ast)
              '()
              (list :generic-function-class
                    (ico:name generic-function-class-ast))))
      ,@(let ((method-class-ast
                (ico:method-class-ast ast)))
          (if (null method-class-ast)
              '()
              (list :method-class
                    (ico:name method-class-ast)))))))
