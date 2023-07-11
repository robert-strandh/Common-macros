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
        (atagbody
         (atag start-tag)
         (awhen 
             (application '= (ico:var-ast ast)
                          (make-variable-name-ast count-var))
           (node* (:go) (1 :tag (atag end-tag))))
         (node* (:tagbody) (* :segment (ico:segment-asts ast)))
         (node* (:incf) (1 :place (ico:var-ast ast)))
         (node* (:go) (1 :tag (atag start-tag)))
         (atag end-tag)))
      (ico:result-ast ast))))
