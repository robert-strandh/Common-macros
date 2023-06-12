(cl:in-package #:common-macros)

(defun generate-required (required-section-ast)
  (loop for ast in (ico:parameter-asts required-section-ast)
        collect (ico:name (ico:name-ast ast))))

(defun generate-optional (optional-section-ast)
  (if (null optional-section-ast)
      '()
      (cons '&optional
            (loop for ast in (ico:parameter-asts optional-section-ast)
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

(defun generate-rest (rest-section-ast)
  (if (null rest-section-ast)
      '()
      (cons '&rest (ico:name (ico:parameter-ast rest-section-ast)))))

(defun generate-key (key-section-ast)
  (if (null key-section-ast)
      '()
      (cons '&key
            (loop for ast in (ico:parameter-asts key-section-ast)
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
          
(defun generate-aux (aux-section-ast)
  (if (null aux-section-ast)
      '()
      (cons '&aux
            (loop for ast in (ico:parameter-asts aux-section-ast)
                  for name = (ico:name (ico:name-ast ast))
                  for init-form-ast = (ico:init-form-ast ast)
                  collect
                  (cons name
                        (if (null init-form-ast)
                            '()
                            (list (ico:form init-form-ast))))))))

(defun generate-ordinary-lambda-list (lambda-list-ast)
  (append (generate-required (ico:required-section-ast lambda-list-ast))
          (generate-optional (ico:optional-section-ast lambda-list-ast))
          (generate-rest (ico:rest-section-ast lambda-list-ast))
          (generate-key (ico:key-section-ast lambda-list-ast))
          (generate-aux (ico:aux-section-ast lambda-list-ast))))

(defmacro cmd:defun (&whole form name lambda-list &rest body)
  (declare (ignore name lambda-list body))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defun))
         (ast (ses:parse builder syntax form)))
    `(setf (fdefinition ',(ico:name (ico:name-ast ast)))
           (lambda ,(generate-ordinary-lambda-list ast)
             ;; FIXME: generate declarations and documentation.
             ,@(mapcar #'ico:form (ico:form-asts ast))))))
