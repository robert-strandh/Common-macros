(cl:in-package #:common-macros)

;;; Macro DOTIMES.
;;;
;;; The spec says we have a choice between binding or assigning the
;;; variable in each iteration.  We have chosen to bind the variable
;;; once for the entire loop body.

(defmethod expand (client (ast ico:dotimes-ast) environment)
  (declare (ignore client environment))
  (let ((start-tag (gensym))
        (end-tag (gensym))
        (count-var (gensym)))
    (node* (:let)
      (1 :binding
         (make-let-binding-ast
          (make-variable-name-ast count-var)
          (ico:count-form-ast ast)))
      (1 :binding
         (make-let-binding-ast
          (ico:var-ast ast)
          (make-unparsed-form-ast '0)))
      (* :declaration (ico:declaration-asts ast))
      (wrap-in-block-ast
       nil
       (node* (:tagbody)
         (1 :segment
            (node* (:segment)
              (1 :tag (node* (:tag :name start-tag)))
              (1 :statement
                 (node* (:when)
                   (1 :test
                      (node* (:application)
                        (1 :function-name (make-function-name-ast '=))
                        (1 :argument (ico:var-ast ast))
                        (1 :argument (make-variable-name-ast count-var))))
                   (1 :form
                      (node* (:go) (1 :tag (node* (:tag :name end-tag)))))))
              (1 :statement
                 (node* (:tagbody) (* :segment (ico:segment-asts ast))))
              (1 :statement
                 (node* (:incf) (1 :place (ico:var-ast ast))))
              (1 :statement
                 (node* (:go) (1 :tag (node* (:tag :name end-tag)))))))
         (1 :segment
            (node* (:segment)
              (1 :tag (node* (:tag :name end-tag))))))
       (ico:result-ast ast)))))
