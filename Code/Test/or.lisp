(cl:in-package #:common-macros-test)

(define-test or)

(define-test or-empty
  :parent or
  (is #'eql
      #1=(or)
      (eval (expand-expression '#1#))))

(define-test or-one-argument-false
  :parent or
  (is #'eql
      (let #2=((x nil)) #1=(or x))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test or-one-argument-true
  :parent or
  (is #'eql
      (let #2=((x 'x)) #1=(or x))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test or-two-arguments-true-false
  :parent or
  (is #'eql
      (let #2=((x 'x) (y nil)) #1=(or x y))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test or-two-arguments-true-true
  :parent or
  (is #'eql
      (let #2=((x 'x) (y 'y)) #1=(or x y))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test or-two-arguments-false-values
  :parent or
  (is #'equal
      (let #2=((x nil) (y 'y))
        (multiple-value-list #1=(or x (values y 234))))
      (eval `(let #2#
               (multiple-value-list
                ,(expand-expression '#1#))))))

(define-test or-two-arguments-values-false
  :parent or
  (is #'equal
      (let #2=((x nil) (y 'y))
        (multiple-value-list #1=(or (values y 234) x)))
      (eval `(let #2#
               (multiple-value-list
                ,(expand-expression '#1#))))))
