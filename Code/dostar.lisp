(cl:in-package #:common-macros)

(defmethod expand (client (ast ico:do*-ast) environment)
  (declare (ignore client environment))
  (let ((start-tag (gensym))
        (variable-asts (ico:do-iteration-variable-asts ast)))
    (ablock 'nil
      (node* (:let*)
        (* :binding
           (loop for variable-ast in variable-asts
                 collect
                 (make-let-binding-ast
                  (ico:do-variable-name-ast variable-ast)
                  (ico:init-form-ast variable-ast))))
        (* :declaration (ico:declaration-asts ast))
        (1 :form
           (node* (:tagbody)
             (1 :segment
                (node* (:segment)
                  (1 :tag (make-tag-ast start-tag))
                  (1 :statement
                     (node* (:when)
                       (1 :test (ico:end-test-ast ast))
                       (1 :form
                          (node* (:return)
                            (1 :form
                               (node* (:progn)
                                 (* :form (ico:result-asts ast))))))))))
             (* :segment (ico:segment-asts ast))
             (1 :segment
                (node* (:segment)
                  (1 :statement
                     (node* (:psetq)
                       (* :variable-name
                          (mapcar #'ico:do-variable-name-ast variable-asts))
                       (* :value
                          (mapcar #'ico:step-form-ast variable-asts))))
                  (1 :statement
                     (node* (:go)
                       (1 :tag (make-tag-ast start-tag))))))))))))
