(cl:in-package #:common-macros)

(defvar *condition-class-metaclass-name*)

(defun generate-lambda-expression (lambda-expression-ast)
  `(lambda ,(ico:unparse-lambda-list-ast
             (ico:lambda-list-ast lambda-expression-ast))
     ;; FIXME: generate declarations.
     ,@(loop for form-ast in (ico:form-asts lambda-expression-ast)
             for form = (ico:form form-ast)
             collect form)))
