(cl:in-package #:common-macros)

;;; FIXME: figure out how to let the client determine
;;; the ENSURE-CLASS function of hte expansion.

;;; FIXME: Include :TYPE slot option in canonical slot specifier.

(defun canonicalize-slot-specifier-ast (ast)
  `(:name ,(ico:name (ico:name-ast ast))
          ,@(let ((initform-ast (ico:initform-ast ast)))
              (if (null initform-ast)
                  '()
                  `(:initform ,initform-ast
                    :initfunction (lambda () ,initform-ast))))
          :initargs
          ,@(mapcar #'ico:name (ico:initarg-asts ast))
          :readers
          ,@(mapcar #'ico:name (ico:reader-asts ast))
          ,@(mapcar #'ico:name (ico:accessor-asts ast))
          :writers
          ,@(mapcar #'ico:name (ico:writer-asts ast))
          ,@(mapcar (lambda (name) `(setf ,name))
                    (ico:accessor-asts ast))
          ,@(let ((documentation-ast (ico:documentation-ast ast)))
              (if (null documentation-ast)
                  '()
                  `(:documentation
                    ,(ico:documentation documentation-ast))))
          ,@(let ((allocation (ico:allocation ast)))
              (if (null allocation)
                  '()
                  `(:allocation ,allocation)))))

(defun canonicalize-default-initarg-ast (ast)
  (list (ico:name (ico:name-ast ast))
        `',(ico:form (ico:initform-ast ast))
        `(lambda () ,(ico:form (ico:initform-ast ast)))))

(defmacro cmd:defclass
    (&whole form
     class-name
     superclass-names
     slot-specifiers
     &rest class-options)
  (declare (ignore class-name superclass-names)
           (ignore slot-specifiers class-options))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'defclass))
         (ast (ses:parse builder syntax form)))
    `(ensure-class
      ,(ico:name (ico:name-ast ast))
      ,@(if (null (ico:metaclass-ast ast))
            '()
            (list (ico:metaclass-ast ast)))
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
                                default-initarg-asts))))))))
