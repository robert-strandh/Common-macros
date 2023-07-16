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
          (node* (:quote :object name))
          (node* (:literal :value ':nicknames))
          (node* (:quote
                  :object
                  (mapcar #'ico:designated-string (ico:nickname-asts ast))))
          (node* (:literal :value ':use))
          (node* (:literal :value '()))))
      (1 :form
         (application
          'shadow
          (node* (:quote
                  :object
                  (mapcar #'ico:designated-string (ico:shadow-asts ast))))
          (node* (:quote :object name))))
      (1 :form
         (application
          'shadowing-import
          (node* (:quote
                  :object (find-symbols (ico:shadowing-import-from-asts ast))))))
      (1 :form
         (application
          'use-package
          (node* (:quote
                  :object
                  (mapcar #'ico:designated-string (ico:use-asts ast))))
          (node* (:quote :object name))))
      (1 :form
         (application
          'import
          (node* (:quote :object (find-symbols (ico:import-from-asts ast))))
          (node* (:literal :value ':intern))
          (node* (:quote
                  :object
                  (mapcar #'ico:designated-string (ico:intern-asts ast))))
          (node* (:literal :value ':export))
          (node* (:quote
                  :object
                  (mapcar #'ico:designated-string (ico:export-asts ast)))))))))
