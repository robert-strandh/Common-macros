(cl:in-package #:common-macros-test)

(define-test unless)

(define-test unless-false-no-forms
  :parent unless
  (is #'eql
      nil
      (eval `(let ((x nil))
               ,(expand-expression '(unless x))))))

(define-test unless-false-one-form
  :parent unless
  (is #'eql
      3
      (eval `(let ((x nil))
               ,(expand-expression '(unless x 3))))))

(define-test unless-false-two-forms
  :parent unless
  (is #'eql
      'a
      (eval `(let ((x nil))
               ,(expand-expression '(unless x 2 'a))))))

(define-test unless-true-no-forms
  :parent unless
  (is #'eql
      nil
      (eval `(let ((x 'x))
               ,(expand-expression '(unless x))))))

(define-test unless-true-one-form
  :parent unless
  (is #'eql
      nil
      (eval `(let ((x 'x))
               ,(expand-expression '(unless x 2))))))

(define-test unless-true-three-form
  :parent unless
  (is #'eql
      nil
      (eval `(let ((x 'x))
               ,(expand-expression '(unless x 2 [a))))))
