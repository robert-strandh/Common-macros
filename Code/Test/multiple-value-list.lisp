(cl:in-package #:common-macros-test)

(define-test multiple-value-list)

(define-test multiple-value-list-no-values
  :parent multiple-value-list
  (is #'equal
      #1=(multiple-value-list (values))
      (expand-expression '#1#)))

(define-test multiple-value-list-one-value
  :parent multiple-value-list
  :depends-on (multiple-value-list-no-values)
  (is #'equal
      #1=(multiple-value-list 234)
      (expand-expression '#1#)))

(define-test multiple-value-list-two-values
  :parent multiple-value-list
  :depends-on (multiple-value-list-no-values)
  (is #'equal
      #1=(multiple-value-list (floor 123 10))
      (expand-expression '#1#)))
