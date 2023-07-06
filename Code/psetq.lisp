(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:psetq-ast) environment)
  (declare (ignore client environment))
  (node* (:psetf)
    (* :place
       (loop for variable-name-ast in (ico:variable-name-asts ast)
             collect (node* (:place) (1 :place variable-name-ast))))
    (* :value (ico:value-asts ast))))
