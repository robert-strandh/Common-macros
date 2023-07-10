(cl:in-package #:common-macros-test)

(define-test or)

(define-test or-empty
  :parent or
  (is #'eql
      (or)
      (eval (expand-expression '(or)))))

(define-test or-one-argument-false
  :parent or
  (is #'eql
      (let ((x nil)) (or x))
      (eval `(let ((x nil))
               ,(expand-expression '(or x))))))

(define-test or-one-argument-true
  :parent or
  (is #'eql
      (let ((x 'x)) (or x))
      (eval `(let ((x 'x))
               ,(expand-expression '(or x))))))

(define-test or-two-arguments-true-false
  :parent or
  (is #'eql
      (let ((x 'x) (y nil)) (or x y))
      (eval `(let ((x 'x) (y nil))
               ,(expand-expression '(or x y))))))

(define-test or-two-arguments-true-true
  :parent or
  (is #'eql
      (let ((x 'x) (y 'y)) (or x y))
      (eval `(let ((x 'x) (y 'y))
               ,(expand-expression '(or x y))))))

(define-test or-two-arguments-false-values
  :parent or
  (is #'equal
      (let ((x nil) (y 'y))
        (multiple-value-list (or x (values y 234))))
      (eval `(let ((x nil) (y 'y))
               (multiple-value-list
                ,(expand-expression '(or x (values y 234))))))))

(define-test or-two-arguments-values-false
  :parent or
  (is #'equal
      (let ((x nil) (y 'y))
        (multiple-value-list (or (values y 234) x)))
      (eval `(let ((x nil) (y 'y))
               (multiple-value-list
                ,(expand-expression '(or (values y 234) x)))))))
