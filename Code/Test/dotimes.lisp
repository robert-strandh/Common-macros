(cl:in-package #:common-macros-test)

(define-test dotimes)

(define-test dotimes-no-result-no-statements
  :parent dotimes
  (is #'eql
      (dotimes (var 10))
      (eval (expand-expression '(dotimes (var 10))))))

(define-test dotimes-no-result-one-statement
  :parent dotimes
  :depends-on (dotimes-no-result-no-statements)
  (is #'eql
      (let ((x 5))
        (dotimes (var 10) (setq x (+ x var))) x)
      (eval `(let ((x 5))
               ,(expand-expression
                 '(dotimes (var 10) (setq x (+ x var))))
               x))))

(define-test dotimes-with-result-one-statement
  :parent dotimes
  :depends-on (dotimes-no-result-one-statement)
  (is #'eql
      (let ((x 5))
        (dotimes (var 10 x) (setq x (+ x var))))
      (eval `(let ((x 5))
               ,(expand-expression
                 '(dotimes (var 10 x) (setq x (+ x var)) x))))))
