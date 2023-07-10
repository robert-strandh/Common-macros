(cl:in-package #:common-macros-test)

(define-test and)

(define-test and-empty
  :parent and
  (is #'eql
      t
      (eval (expand-expression '(and)))))

(define-test and-one-argument-false
  :parent and
  (is #'eql
      nil
      (eval `(let ((x nil))
               ,(expand-expression '(and x))))))

(define-test and-one-argument-true
  :parent and
  (is #'eql
      'x
      (eval `(let ((x 'x))
               ,(expand-expression '(and x))))))

(define-test and-two-arguments-true-false
  :parent and
  (is #'eql
      nil
      (eval `(let ((x 'x) (y nil))
               ,(expand-expression '(and x y))))))

(define-test and-two-arguments-true-true
  :parent and
  (is #'eql
      'y
      (eval `(let ((x 'x) (y 'y))
               ,(expand-expression '(and x y))))))

(define-test and-two-arguments-true-values
  :parent and
  (is #'equal
      '(y 234)
      (eval `(let ((x 'x) (y 'y))
               (multiple-value-list
                ,(expand-expression '(and x (values y 234))))))))
