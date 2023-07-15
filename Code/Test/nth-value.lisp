(cl:in-package #:common-macros-test)

(define-test nth-value)

(define-test nth-value-no-values
  :parent nth-value
  (is #'equal
      #1=(nth-value 0 (values))
      `(eval ,(expand-expression '#1#))))

(define-test nth-value-one-value-n=0
  :parent nth-value
  (is #'equal
      #1=(nth-value 0 234)
      `(eval ,(expand-expression '#1#))))

(define-test nth-value-one-value-n=1
  :parent nth-value
  (is #'equal
      #1=(nth-value 1 234)
      `(eval ,(expand-expression '#1#))))

(define-test nth-value-two-values-n=0
  :parent nth-value
  (is #'equal
      #1=(nth-value 0 (floor 100 13))
      `(eval ,(expand-expression '#1#))))

(define-test nth-value-two-values-n=1
  :parent nth-value
  (is #'equal
      #1=(nth-value 1 (floor 100 13))
      `(eval ,(expand-expression '#1#))))

(define-test nth-value-two-values-n=2
  :parent nth-value
  (is #'equal
      #1=(nth-value 2 (floor 100 13))
      `(eval ,(expand-expression '#1#))))
