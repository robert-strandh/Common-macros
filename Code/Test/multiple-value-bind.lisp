(cl:in-package #:common-macros-test)

(define-test multiple-value-bind)

(define-test multiple-value-bind-empty
  :parent multiple-value-bind
  (is #'eql
      #1=(multiple-value-bind () 234)
      (expand-expression '#1#)))

(define-test multiple-value-bind-no-vars-side-effect
  :parent multiple-value-bind
  :depends-on (multiple-value-bind-empty)
  (is #'eql
      (let ((x 10))
        #1=(multiple-value-bind () (setq x 11))
        x)
      (eval `(let ((x 10))
               ,(expand-expression '#1#)
               x))))

(define-test multiple-value-bind-one-var
  :parent multiple-value-bind
  :depends-on (multiple-value-bind-empty)
  (is #'eql
      #1=(multiple-value-bind (x) 234 x)
      (eval (expand-expression '#1#))))

(define-test multiple-value-bind-two-vars-one-value
  :parent multiple-value-bind
  :depends-on (multiple-value-bind-empty)
  (is #'equal
      #1=(multiple-value-bind (x y) 234 (list x y))
      (eval (expand-expression '#1#))))
