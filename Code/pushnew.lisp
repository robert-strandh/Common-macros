(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:pushnew-ast) environment)
  (declare (ignore client))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast) environment)
    (let ((item-var (gensym)))
      (alet* ((b (make-variable-name-ast item-var) (ico:item-ast ast))
              binding-asts
              (b (first store-variable-asts)
                 (if (null (ico:key-ast ast))
                     (if (null (ico:test-ast ast))
                         (if (null (ico:test-not-ast ast))
                             (application
                              'adjoin
                              (make-variable-name-ast item-var)
                              read-ast)
                             (application
                              'adjoin
                              (make-variable-name-ast item-var)
                              read-ast
                              (aliteral ':test-not)
                              (unparse (ico:test-not-ast ast))))
                         ;; TEST-AST is not NULL, so TEST-NOT-AST must
                         ;; be NULL.
                         (application
                          'adjoin
                          (make-variable-name-ast item-var)
                          read-ast
                          (aliteral ':test)
                          (unparse (ico:test-ast ast))))
                     (if (null (ico:test-ast ast))
                         (if (null (ico:test-not-ast ast))
                             (application
                              'adjoin
                              (make-variable-name-ast item-var)
                              read-ast
                              (aliteral ':key)
                              (unparse (ico:key-ast ast)))
                             (application
                              'adjoin
                              (make-variable-name-ast item-var)
                              read-ast
                              (aliteral ':key)
                              (unparse (ico:key-ast ast))
                              (aliteral ':test-not)
                              (unparse (ico:test-not-ast ast))))
                         ;; TEST-AST is not NULL, so TEST-NOT-AST must
                         ;; be NULL.
                         (application
                          'adjoin
                          (make-variable-name-ast item-var)
                          read-ast
                          (aliteral ':key)
                          (unparse (ico:key-ast ast))
                          (aliteral ':test)
                          (unparse (ico:test-ast ast)))))))
        store-ast))))
