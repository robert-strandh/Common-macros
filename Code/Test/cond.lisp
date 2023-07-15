(cl:in-package #:common-macros-test)

(define-test cond)

(define-test cond-empty
  :parent cond
  (is #'equal
      #1=(cond)
      (eval (expand-expression '#1#))))

(define-test cond-no-match
  (is #'equal
      #1=(cond ((> (random 10) 20) (error "hello")))
      (eval (expand-expression '#1#))))

(define-test cond-match-first
  (is #'equal
      #1=(cond ((< (random 10) 4) 234)
               ((> (random 10) 20) (error "hello")))
      (eval (expand-expression '#1#))))
