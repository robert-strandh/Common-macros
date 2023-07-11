(cl:in-package #:common-macros-test)

(define-test push)

(define-test push-with-empty-list
  :parent push
  (is #'equal
      (let ((x '())) (push 1 x))
      (eval `(let ((x '()))
               ,(expand-expression '(push 1 x))))))

(define-test push-with-atom
  :parent push
  (is #'equal
      (let ((x 2)) (push 1 x))
      (eval `(let ((x 2))
               ,(expand-expression '(push 1 x))))))

(define-test push-with-non-empty-list
  :parent push
  (is #'equal
      (let ((x '(2))) (push 1 x))
      (eval `(let ((x '(2)))
               ,(expand-expression '(push 1 x))))))
