(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:declaim-ast) environment)
  (declare (ignore client environment))
  (node* (:eval-when)
    (* :situation
       (make-eval-when-situation-asts
        :compile-toplevel :load-toplevel :execute))
    (* :form
       (loop for declaration-specifier-ast in (ico:declaration-specifier-asts ast)
             collect
             (application
              'proclaim
              (node* (:qoute :object (unparse declaration-specifier-ast))))))))
