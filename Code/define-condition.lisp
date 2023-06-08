(cl:in-package #:common-macros)

(defvar *condition-class-metaclass-name*)

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
       (cmm:ensure-class
        ,name
        :metaclass ,*condition-class-metaclass-name*
        :direct-superclasse s
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
                        ;; FIXME: generate the lambda expression
                        nil))))))))
