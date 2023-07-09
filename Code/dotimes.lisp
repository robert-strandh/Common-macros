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
    (alet ((b (make-variable-name-ast count-var) (ico:count-form-ast ast))
           (b (ico:var-ast ast) (make-unparsed-form-ast '0)))
      (ico:declaration-asts ast)
      (ablock 'nil
        (node* (:tagbody)
          (1 :segment
             (node* (:segment)
               (1 :tag (make-tag-ast start-tag))
               (1 :statement
                  (node* (:when)
                    (1 :test
                       (application '= (ico:var-ast ast)
                                    (make-variable-name-ast count-var)))
                    (1 :form
                       (node* (:go) (1 :tag (make-tag-ast end-tag))))))
               (1 :statement
                  (node* (:tagbody) (* :segment (ico:segment-asts ast))))
               (1 :statement
                  (node* (:incf) (1 :place (ico:var-ast ast))))
               (1 :statement
                  (node* (:go) (1 :tag (make-tag-ast end-tag))))))
          (1 :segment
             (node* (:segment)
               (1 :tag (make-tag-ast end-tag)))))
        (ico:result-ast ast)))))
