(cl:in-package #:common-macros)

;;; Macro DOLIST.
;;;
;;; The spec says that the variable is bound to nil when the
;;; result-form is evaluated.  But we don't want the declarations to
;;; have to include nil as one of the values of var.  For that reason,
;;; there needs to be a different binding of the variable when the
;;; forms of the body are evaluated and when the result-form is
;;; evaluated.
;;;
;;; The spec says we have a choice between binding or assigning the
;;; variable in each iteration.  For dolist, choosing assignment gets
;;; complicated in the first iteration though, because we would have
;;; to come up with an initial value of the variable that is
;;; compatible with the declarations.  For that reason, we choose to
;;; bind it.

(defmethod expand (client (ast ico:dolist-ast) environment)
  (let ((start-tag (gensym))
        (end-tag (gensym))
        (list-var (gensym)))
    (alet ((b (make-variable-name-ast list-var) (ico:list-form-ast ast)))
      (ablock 'nil
        (node* (:tagbody)
          (1 :segment
             (node* (:tagbody-segment)
               (1 :label (make-tag-ast start-tag))
               (1 :statement
                  (awhen (application 'endp list-var)
                    (node* (:go) (1 :tag (make-tag-ast end-tag)))))
               (1 :statement

                  (alet ((b (ico:variable-name-ast ast)
                            (node* (:application)
                              (1 :function-name (make-function-name-ast 'car))
                              (1 :argument (make-variable-name-ast list-var)))))
                    (ico:declaration-asts ast)
                    (node* (:tagbody)
                      (* :segment (ico:segment-asts ast)))))
               (1 :statement
                  (node* (:pop)
                    (1 :place (make-variable-name-ast list-var))))
               (1 :statement
                  (node* (:go) (1 :tag (make-tag-ast start-tag))))))
          (1 :segment
             (node* (:tagbody-segment)
               (1 :label (make-tag-ast end-tag)))))
        (alet ((b (ico:variable-name-ast ast) (make-unparsed-form-ast 'nil)))
          (node* (:declaration)
            (1 :declaration-specifier
               (node* (:declaration-specifier :kind 'ignorable)
                 (1 :argument (ico:variable-name-ast ast)))))
          (ico:result-ast ast))))))
