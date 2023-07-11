(cl:in-package #:common-macros-test)

(define-test prog1)

(define-test prog1-only-first-form
  :parent prog1
    (is #'eql
        (prog1 234)
        (eval (expand-expression `(prog1 234)))))

(define-test prog1-one-additional-form
  :parent prog1
    (is #'equal
        (let ((x 0)) (list (prog1 234 (setq x 1)) x))
        (eval `(let ((x 0))
                 (list ,(expand-expression `(prog1 234 (setq x 1))) x)))))

