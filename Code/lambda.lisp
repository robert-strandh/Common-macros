(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:lambda-ast) environment)
  (declare (ignore client environment))
  (node* (:function)
    (1 :name ast)))
