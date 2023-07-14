(cl:in-package #:common-macros-test)

(define-test assert)

(define-test assert-true-no-place
  :parent assert
  (is #'equal
      #1=(assert t)
      (eval (expand-expression '(assert t)))))

(define-test assert-false-no-place
  :parent assert
  (fail (eval (expand-expression '(assert nil)))))
