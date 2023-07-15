(cl:in-package #:common-macros-test)

(define-test remf)

(define-test remf-nothing-removed
  :parent remf
  (is #'equal
      (let #2=((plist (copy-list '(a 1 b 2 c 3 b 4))))
        (list #1=(remf plist 'x) plist))
      (eval `(let #2# (list ,(expand-expression '#1#) plist)))))

(define-test remf-first-removed
  :parent remf
  (is #'equal
      (let #2=((plist (copy-list '(a 1 b 2 c 3 b 4))))
        (list #1=(remf plist 'a) plist))
      (eval `(let #2# (list ,(expand-expression '#1#) plist)))))

(define-test remf-first-of-two-removed
  :parent remf
  (is #'equal
      (let #2=((plist (copy-list '(a 1 b 2 c 3 b 4))))
        (list #1=(remf plist 'b) plist))
      (eval `(let #2# (list ,(expand-expression '#1#) plist)))))
