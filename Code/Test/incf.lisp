(cl:in-package #:common-macros-test)

(define-test incf)

(define-test incf-variable-default-delta
  :parent incf
    (is #'eql
        3
        (eval `(let ((x 2))
                 ,(expand-expression '(incf x))))))

(define-test incf-variable-default-delta-side-effect
  :parent incf
    (is #'eql
        3
        (eval `(let ((x 2))
                 ,(expand-expression '(incf x))
                 x))))

(define-test incf-variable-with-delta
  :parent incf
    (is #'eql
        5
        (eval `(let ((x 3))
                 ,(expand-expression '(incf x 2))))))

(define-test incf-array-default-delta
  :parent incf
    (is #'eql
        3
        (eval `(let ((x (make-array 1 :initial-element 2)))
                 ,(expand-expression '(incf (aref x 0)))))))

(define-test incf-array-with-delta
  :parent incf
    (is #'eql
        5
        (eval `(let ((x (make-array 1 :initial-element 3)))
                 ,(expand-expression '(incf (aref x 0) 2))))))
