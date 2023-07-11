(cl:in-package #:common-macros-test)

(define-test pop)

(define-test pop-with-one-element
  :parent pop
  (is #'equal
      (let ((x '(1))) (pop x))
      (eval `(let ((x '(1)))
               ,(expand-expression '(pop x))))))

(define-test pop-with-one-element-side-effect
  :parent pop
  (is #'equal
      (let ((x '(1))) (pop x) x)
      (eval `(let ((x '(1)))
               ,(expand-expression '(pop x)) x))))

(define-test pop-with-two-elements
  :parent pop
  (is #'equal
      (let ((x '(1 2))) (pop x))
      (eval `(let ((x '(1 2)))
               ,(expand-expression '(pop x))))))

(define-test pop-with-two-elements-side-effect
  :parent pop
  (is #'equal
      (let ((x '(1 2))) (pop x) x)
      (eval `(let ((x '(1 2)))
               ,(expand-expression '(pop x)) x))))
