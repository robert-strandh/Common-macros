(cl:in-package #:common-macros)

(defun expand-place-value-pair (client place-ast values-ast environment)
  (multiple-value-bind (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast client place-ast environment)
    (alet* (binding-asts)
      (node* (:multiple-value-bind)
        (* :name store-variable-asts)
        (1 :values values-ast)
        (1 :form store-ast))
      read-ast)))

(defmethod expand (client (ast ico:setf-ast) environment)
  (node* (:progn)
    (* :form
       (loop for place-ast in (ico:place-asts ast)
             for value-ast in (ico:value-asts ast)
             collect (expand-place-value-pair
                      client place-ast value-ast environment)))))
