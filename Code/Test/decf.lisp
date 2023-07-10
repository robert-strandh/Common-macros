(cl:in-package #:common-macros-test)

(define-test decf)

(define-test decf-variable-default-delta
  :parent decf
    (is #'eql
        1
        (eval `(let ((x 2))
                 ,(expand-expression '(decf x))))))

(define-test decf-variable-default-delta-side-effect
  :parent decf
    (is #'eql
        1
        (eval `(let ((x 2))
                 ,(expand-expression '(decf x))
                 x))))

(define-test decf-variable-with-delta
  :parent decf
    (is #'eql
        1
        (eval `(let ((x 3))
                 ,(expand-expression '(decf x 2))))))
