(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:psetq-ast) environment)
  (declare (ignore client environment))
  (node* (:psetf)
    (* :place (ico:variable-name-asts ast))
    (* :value (ico:value-asts ast))))
