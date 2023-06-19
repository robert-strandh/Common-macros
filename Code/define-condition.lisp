(cl:in-package #:common-macros)

(defvar *condition-class-metaclass-name*)

(defun generate-lambda-expression (lambda-expression-ast)
  `(lambda ,(ico:unparse-lambda-list-ast
             (ico:lambda-list-ast lambda-expression-ast))
     ;; FIXME: generate declarations.
     ,@(loop for form-ast in (ico:form-asts lambda-expression-ast)
             for form = (ico:form form-ast)
             collect form)))

(defmacro cmd:define-condition
    (&whole form
     class-name
     superclass-names
     slot-specifiers
     &rest class-options)
  (declare (ignore class-name superclass-names)
           (ignore slot-specifiers class-options))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'define-condition))
         (ast (ses:parse builder syntax form))
         (name (ico:name (ico:name-ast ast))))
    `(progn   
       (make-instance (find-class 'condition)
         :name ,name
         :metaclass (class-of (find-class 'condition))
         :direct-superclasses
         ',(mapcar #'ico:name (ico:superclass-asts ast))
         :direct-slots
         (list ,@(mapcar #'canonicalize-slot-specifier-ast
                         (ico:slot-specifier-asts ast)))
         ,@(let ((default-initarg-asts (ico:default-initarg-asts ast)))
             (if (null default-initarg-asts)
                 '()
                 `(:direct-default-initargs
                   (list ,@(mapcar #'canonicalize-default-initarg-ast
                                   default-initarg-asts))))))
       ,(let ((report-ast (ico:report-ast ast)))
          (if (null report-ast)
              (let ((method-variable (gensym)))
                `(let ((,method-variable
                         (find-method #'print-object '() `(,name t) nil)))
                   (unless (null ,method-variable)
                     (remove-method #'print-object ,method-variable))))
              (let ((argument-ast (ico:argument-ast report-ast))
                    (condition-variable (gensym))
                    (stream-variable (gensym)))
                `(defmethod print-object
                     ((,condition-variable ,name) ,stream-variable)
                   ,(if (typep argument-ast 'ico:unparsed-form-ast)
                        `(write-string
                          ,(ico:form argument-ast) ,stream-variable)
                        `(funcall ,(generate-lambda-expression
                                    (ico:argument-ast report-ast))
                                  ,condition-variable ,stream-variable)))))))))
