(cl:in-package #:common-macros-test)

(define-test when)

(define-test when-true-no-forms
  :parent when
    (is #'eql
        nil
        (eval `(let ((x 'x))
                 ,(expand-expression '(when x))))))

(define-test when-true-one-form
  :parent when
    (is #'eql
        3
        (eval `(let ((x 'x))
                 ,(expand-expression '(when x 3))))))

(define-test when-true-two-forms
  :parent when
    (is #'eql
        'a
        (eval `(let ((x 'x))
                 ,(expand-expression '(when x 2 'a))))))

(define-test when-false-no-forms
  :parent when
    (is #'eql
        nil
        (eval `(let ((x nil))
                 ,(expand-expression '(when x))))))

(define-test when-false-one-form
  :parent when
    (is #'eql
        nil
        (eval `(let ((x nil))
                 ,(expand-expression '(when x 2))))))

(define-test when-false-three-form
  :parent when
    (is #'eql
        nil
        (eval `(let ((x nil))
                 ,(expand-expression '(when x 2 [a))))))
