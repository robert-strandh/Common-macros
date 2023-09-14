(cl:in-package #:common-macros)

(defun expand-place-value-pair (client place-ast values-ast environment)
  (multiple-value-bind
        (temporary-asts form-asts store-variable-asts store-ast read-ast)
      (ast-setf-expansion client place-ast environment)
    (make-instance 'ico:let*-ast
      :binding-asts
      (mapcar #'make-let-binding-ast temporary-asts form-asts)
      :form-asts
      (make-instance 'ico:multiple-value-bind-ast
        :values-ast values-ast
        :variable-name-asts store-variable-asts
        :form-asts (list store-ast read-ast)))))

(defmethod expand (client (ast ico:setf-ast) environment)
  (node* (:progn)
    (* :form
       (loop for place-ast in (ico:place-asts ast)
             for value-ast in (ico:value-asts ast)
             collect (expand-place-value-pair
                      client place-ast value-ast environment)))))
