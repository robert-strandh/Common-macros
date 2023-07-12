(cl:in-package #:common-macros-test)

(define-test multiple-value-bind)

(define-test multiple-value-bind-empty
  :parent multiple-value-bind
  (is #'eql
      (multiple-value-bind () 234)
      (expand-expression '(multiple-value-bind () 234))))

(define-test multiple-value-bind-no-vars-side-effect
  :parent multiple-value-bind
  :depends-on (multiple-value-bind-empty)
  (is #'eql
      (let ((x 10))
        (multiple-value-bind () (setq x 11))
        x)
      (eval `(let ((x 10))
               ,(expand-expression '(multiple-value-bind () (setq x 11)))
               x))))
