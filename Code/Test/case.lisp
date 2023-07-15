(cl:in-package #:common-macros-test)

(define-test case)

(define-test case-no-match
  :parent case
  (is #'equal
      #1=(case 1 (0 (error "x")))
      (eval (expand-expression '#1#))))

(define-test case-first-match
  :parent case
  (is #'equal
      #1=(case 1 (1 234) (0 (error "x")))
      (eval (expand-expression '#1#))))

(define-test case-second-match
  :parent case
  (is #'equal
      #1=(case 1 (0 (error "x")) (1 234))
      (eval (expand-expression '#1#))))

(define-test case-second-match-second-key
  :parent case
  (is #'equal
      #1=(case 1 (0 (error "x")) ((2 1) 234))
      (eval (expand-expression '#1#))))
