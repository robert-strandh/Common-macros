(cl:in-package #:common-macros)

;;; This definition is taken from the Common Lisp standard.

(defmethod expand (client (ast ico:with-simple-restart-ast) environment)
  (declare (ignore client environment))
  (let ((stream-parameter (gensym)))
    (node* (:restart-case)
      (1 :clause
         (node* (:restart-clause)
           (* :form (ico:form-asts ast))
           (1 :name (ico:name-ast ast))
           (1 :lambda-list (node* (:ordinary-lambda-list)))
           (1 :report
              (node* (:lambda)
                (1 :lambda-list
                   (node* (:ordinary-lambda-list)
                     (1 :required-section
                        (node* (:required-section)
                          (1 :required-parameter
                             (node* (:required-parameter)
                               (1 :name (make-variable-name-ast
                                         stream-parameter))))))))
                (1 :form
                   (application
                    'format
                    (make-variable-name-ast stream-parameter)
                    (ico:format-control-ast ast)
                    (ico:format-argument-asts ast))))))))))
