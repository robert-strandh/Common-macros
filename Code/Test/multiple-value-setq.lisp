(cl:in-package #:common-macros-test)

(define-test multiple-value-setq)

(define-test multiple-value-setq-no-vars-one-value
  :parent multiple-value-setq
  (is #'equal
      #1=(multiple-value-setq () 234)
      (expand-expression '#1#)))

(define-test multiple-value-setq-one-var-no-values
  :parent multiple-value-setq
  :depends-on (multiple-value-setq-no-vars-one-value)
  (is #'equal
      (let #2=((x 0))
        #1=(multiple-value-setq (x) (values)))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test multiple-value-setq-one-var-one-value
  :parent multiple-value-setq
  :depends-on (multiple-value-setq-no-vars-one-value)
  (is #'equal
      (let #2=((x 0))
        #1=(multiple-value-setq (x) 234))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test multiple-value-setq-one-var-two-values
  :parent multiple-value-setq
  :depends-on (multiple-value-setq-no-vars-one-value)
  (is #'equal
      (let #2=((x 0))
        #1=(multiple-value-setq (x) (floor 234 10)))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test multiple-value-setq-two-vars-two-values
  :parent multiple-value-setq
  :depends-on (multiple-value-setq-no-vars-one-value)
  (is #'equal
      (let #2=((x 0) (y 0))
        #1=(multiple-value-setq (x y) (floor 234 10)))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test multiple-value-setq-three-vars-two-values
  :parent multiple-value-setq
  :depends-on (multiple-value-setq-no-vars-one-value)
  (is #'equal
      (let #2=((x 0) (y 0) (z 0))
        #1=(multiple-value-setq (x y z) (floor 234 10)))
      (eval `(let #2#
               ,(expand-expression '#1#)))))
