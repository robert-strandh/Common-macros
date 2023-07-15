(cl:in-package #:common-macros-test)

(define-test return)

(define-test return-no-value
  :parent return
  (is #'equal
      (block nil #1=(return))
      (eval `(block nil ,(expand-expression '#1#)))))

(define-test return-with-value
  :parent return
  (is #'equal
      (block nil #1=(return 234))
      (eval `(block nil ,(expand-expression '#1#)))))
