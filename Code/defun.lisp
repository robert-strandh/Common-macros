(cl:in-package #:common-macros)

(defun generate-required (required-asts)
  (loop for required-ast in required-asts
        collect (ico:name (ico:name-ast required-ast))))

(defun generate-optional (optional-asts)
  (if (null optional-asts)
      '()
      (cons '&optional
            (loop for ast in optional-asts
                  for name = (ico:name (ico:name-ast ast))
                  for init-form-ast = (ico:init-form-ast ast)
                  for supplied-p-ast = (ico:supplied-p-parameter-ast ast)
                  collect
                  (append (list name)
                          (if (null init-form-ast)
                              '()
                              (list (ico:form init-form-ast)))
                          (if (null supplied-p-ast)
                              '()
                              (list (ico:name supplied-p-ast))))))))

(defun generate-rest (rest-ast)
  (if (null rest-ast)
      '()
      (cons '&rest (ico:name rest-ast))))

(defun generate-key (key-asts)
  (if (null key-asts)
      '()
      (cons '&key
            (loop for ast in key-asts
                  for name = (ico:name (ico:name-ast ast))
                  for keyword-name-ast = (ico:keyword-name-ast ast)
                  for init-form-ast = (ico:init-form-ast ast)
                  for supplied-p-ast = (ico:supplied-p-parameter-ast ast)
                  collect
                  (cons (if (null keyword-name-ast)
                            name
                            `(,(ico:name keyword-name-ast) ,name))
                        (if (null supplied-p-ast)
                            (if (null init-form-ast)
                                name
                                `(,name ,(ico:form init-form-ast)))
                            `(,name
                              ,(ico:form init-form-ast)
                              ,(ico:name supplied-p-ast))))))))
          
(defun generate-aux (aux-asts)
  (if (null aux-asts)
      '()
      (cons '&aux
            (loop for ast in aux-asts
                  for name = (ico:name (ico:name-ast ast))
                  for init-form-ast = (ico:init-form-ast ast)
                  collect
                  (cons name
                        (if (null init-form-ast)
                            '()
                            (list (ico:form init-form-ast))))))))

(defun generate-ordinary-lambda-list (lambda-list-ast)
  (append (generate-required (ico:required-parameter-asts lambda-list-ast))
          (generate-optional (ico:optional-parameter-asts lambda-list-ast))
          (generate-rest (ico:rest-parameter-ast lambda-list-ast))
          (generate-key (ico:key-parameter-asts lambda-list-ast))
          (generate-aux (ico:aux-parameter-asts lambda-list-ast))))

(defmacro cmd:defun (&whole form name lambda-list &rest body)
  (declare (ignore name lambda-list body))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defun))
         (ast (ses:parse builder syntax form)))
    `(setf (fdefinition ',(ico:name (ico:name-ast ast)))
           (lambda ,(generate-ordinary-lambda-list ast)
             ;; FIXME: generate declarations and documentation.
             ,@(mapcar #'ico:form (ico:form-asts ast))))))
