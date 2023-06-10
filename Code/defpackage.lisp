(cl:in-package #:common-macros)

(defun find-symbols (import-from-asts)
  (loop for import-from-ast in import-from-asts
        for package-name-ast = (ico:package-name-ast import-from-ast)
        for package-name-string = (ico:designated-string package-name-ast)
        for package = (find-package package-name-string)
        append (loop for name-ast in (ico:name-asts import-from-ast)
                     for name-string = (ico:designated-string name-ast)
                     collect
                     (multiple-value-bind (symbol status)
                         (find-symbol name-string package)
                       (if (null status)
                           (error "no such symbol")
                           symbol)))))

(defmacro cmd:defpackage (&whole form name &rest options)
  (declare (ignore name options))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'cl:defpackage))
         (modified-form (cons 'cl:defpackage (cdr form)))
         (ast (ses:parse builder syntax modified-form))
         (name (ico:name (ico:name-ast ast))))
    `(progn
       (make-package
        ',name
        :nicknames 
        '`(,(mapcar #'ico:designated-string (ico:nickname-asts ast)))
        :use '())
       (shadow
        '`(,(mapcar #'ico:designated-string (ico:shadow-asts ast)))
        ',name)
       (shadowing-import
        '`(find-symbols (ico:shadowing-import-from-asts ast)))
       (use-package
        '`(,(mapcar #'ico:designated-string (ico:use-asts ast)))
        ',name)
       (import
        '`(find-symbols (ico:import-from-asts ast)))
        :intern
        '`(,(mapcar #'ico:designated-string (ico:intern-asts ast)))
        :export
        '`(,(mapcar #'ico:designated-string (ico:export-asts ast))))))
