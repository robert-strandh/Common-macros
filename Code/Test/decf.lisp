(cl:in-package #:common-macros-test)

(define-test decf)

(define-test decf-variable-default-delta
  :parent decf
    (is #'eql
        1
        (eval `(let ((x 2))
                 ,(expand-expression '(decf x))))))
