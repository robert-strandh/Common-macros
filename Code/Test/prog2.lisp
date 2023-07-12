(cl:in-package #:common-macros-test)

(define-test prog2)

(define-test prog2-only-first-and-second-form
  :parent prog2
    (is #'eql
        #1=(prog2 234 345)
        (eval (expand-expression `#1#))))

(define-test prog2-one-additional-form
  :parent prog2
    (is #'equal
        (let #2=((x 0)) (list #1=(prog2 234 345 (setq x 1)) x))
        (eval `(let #2#
                 (list ,(expand-expression
                         '#1#)
                       x)))))

