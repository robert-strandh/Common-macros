(cl:in-package #:common-macros-test)

(define-test pop)

(define-test pop-with-one-element
  :parent pop
  (is #'equal
      (let #2=((x '(1))) #1=(pop x))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test pop-with-one-element-side-effect
  :parent pop
  (is #'equal
      (let #2=((x '(1))) #1=(pop x) x)
      (eval `(let #2#
               ,(expand-expression '#1#) x))))

(define-test pop-with-two-elements
  :parent pop
  (is #'equal
      (let #2=((x '(1 2))) #1=(pop x))
      (eval `(let #2#
               ,(expand-expression '#1#)))))

(define-test pop-with-two-elements-side-effect
  :parent pop
  (is #'equal
      (let #2=((x '(1 2))) #1=(pop x) x)
      (eval `(let #2#
               ,(expand-expression '#1#) x))))
