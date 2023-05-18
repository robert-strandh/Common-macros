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

;;; FIXME: use Iconoclast when the s-expression-syntax library has
;;; DOLIST, so that Iconoclast can define an AST for it.  Until then,
;;; the syntax checking is going to be skimpy.

(defmacro cmd:dolist ((var list-form &optional result-form) &body body)
  (let ((boundary (position-if (lambda (item)
                                 (and (consp item) (eq (first item) 'declare)))
                               body)))
    (multiple-value-bind (declarations forms)
        (values (if (null boundary)
                    '()
                    (subseq body 0 boundary))
                (if (null boundary)
                    body
                    (subseq body boundary)))
      (let ((start-tag (gensym))
            (end-tag (gensym))
            (list-var (gensym)))
        `(let ((,list-var ,list-form))
           (block nil
             (tagbody
                ,start-tag
                (when (endp ,list-var)
                  (go ,end-tag))
                (let ((,var (car ,list-var)))
                  ,@declarations
                  (tagbody ,@forms))
                (pop ,list-var)
                (go ,start-tag)
                ,end-tag)
             (let ((,var nil))
               (declare (ignorable ,var))
               ,result-form)))))))
