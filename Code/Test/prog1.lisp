(cl:in-package #:common-macros-test)

(define-test prog1)

(define-test prog1-only-first-form
  :parent prog1
    (is #'eql
        #1=(prog1 234)
        (eval (expand-expression `#1#))))

(define-test prog1-one-additional-form
  :parent prog1
    (is #'equal
        (let #2=((x 0)) (list #1=(prog1 234 (setq x 1)) x))
        (eval `(let #2#
                 (list ,(expand-expression `#1#) x)))))

