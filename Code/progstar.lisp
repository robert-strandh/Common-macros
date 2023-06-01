(cl:in-package #:common-macros)

;;; This definition is brittle.  Once the s-expression-syntax library
;;; can handle PROG, we should use Iconoclast for this macro.

(defmacro cmd:prog* (bindings &body body)
  (let ((position
          (position-if
           (lambda (declaration-or-form)
             (and (consp declaration-or-form)
                  (eq (car declaration-or-form) 'declare)))
           body
           :from-end t)))
    `(block nil (let* ,bindings
                  ,@(if (null position)
                        '()
                        (subseq body 0 (1+ position)))
                  (tagbody
                     ,@(if (null position)
                           body
                           (subseq body (1+ position))))))))
