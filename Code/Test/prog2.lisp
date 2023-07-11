(cl:in-package #:common-macros-test)

(define-test prog2)

(define-test prog2-only-first-and-second-form
  :parent prog2
    (is #'eql
        (prog2 234 345)
        (eval (expand-expression `(prog2 234 345)))))

(define-test prog2-one-additional-form
  :parent prog2
    (is #'equal
        (let ((x 0)) (list (prog2 234 345 (setq x 1)) x))
        (eval `(let ((x 0))
                 (list ,(expand-expression
                         `(prog2 234 345 (setq x 1)))
                       x)))))

