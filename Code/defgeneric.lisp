(cl:in-package #:common-macros)

(defun generate-generic-function-lambda-list (lambda-list-ast)
  (append (generate-required (ico:required-parameter-asts lambda-list-ast))
          (generate-optional (ico:optional-parameter-asts lambda-list-ast))
          (generate-rest (ico:rest-parameter-ast lambda-list-ast))
          (generate-key (ico:key-parameter-asts lambda-list-ast))))

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
      ',(generate-generic-function-lambda-list
         (ico:lambda-list-ast ast))
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
