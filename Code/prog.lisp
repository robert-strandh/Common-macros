(cl:in-package #:common-macros)

;;; This definition is brittle.  Once the s-expression-syntax library
;;; can handle PROG, we should use Iconoclast for this macro.

(defmacro cmd:prog (bindings &body body)
  (multiple-value-bind (declarations tags-or-statements)
      (separate-ordinary-body body)
    `(block nil (let ,bindings
                  ,@declarations
                  (tagbody ,@tags-or-statements)))))
