(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:push-ast) environment)
  (declare (ignore client))
  (multiple-value-bind
        (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast (ico:place-ast ast) environment)
    (let ((item-var (gensym)))
      (alet* ((b (make-variable-name-ast item-var) (ico:item-ast ast))
              binding-asts
              (b (first store-variable-asts)
                 (application
                  'cons
                  (make-variable-name-ast item-var) read-ast)))
        store-ast))))
