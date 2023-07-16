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

(defmethod expand (client (ast ico:defpackage-ast) environment)
  (declare (ignore client environment))
  (let ((name (ico:name (ico:name-ast ast))))
    (node* (:progn)
      (1 :form
         (application
          'make-package
          (aquote name)
          (aliteral ':nicknames)
          (aquote (mapcar #'ico:designated-string (ico:nickname-asts ast)))
          (aliteral ':use)
          (aliteral '())))
      (1 :form
         (application
          'shadow
          (aquote (mapcar #'ico:designated-string (ico:shadow-asts ast)))
          (aquote name)))
      (1 :form
         (application
          'shadowing-import
          (aquote (find-symbols (ico:shadowing-import-from-asts ast)))))
      (1 :form
         (application
          'use-package
          (aquote (mapcar #'ico:designated-string (ico:use-asts ast)))
          (aquote name)))
      (1 :form
         (application
          'import
          (aquote (find-symbols (ico:import-from-asts ast)))
          (aliteral ':intern)
          (aquote (mapcar #'ico:designated-string (ico:intern-asts ast)))
          (aliteral ':export)
          (aquote (mapcar #'ico:designated-string (ico:export-asts ast))))))))
