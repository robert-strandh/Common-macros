(cl:in-package #:common-macros-test)

(define-test shiftf)

(define-test shiftf-one-place-one-value
  :parent shiftf
  (is #'equal
      (let ((x 10)) #1=(shiftf x 2))
      (eval `(let ((x 10)) ,(expand-expression '#1#)))))
