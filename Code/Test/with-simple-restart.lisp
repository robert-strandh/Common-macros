(cl:in-package #:common-macros-test)

(define-test with-simple-restart)

(define-test with-simple-restart-no-restart-invocation
  :parent with-simple-restart
  (is #'equal
      (multiple-value-list #1=(with-simple-restart (hello "") 234))
      (eval `(multple-value-list ,(expand-expression '#1#)))))

(define-test with-simple-restart-with-restart-invocation
  :parent with-simple-restart
  (is #'equal
      (multiple-value-list #1=(with-simple-restart (hello "") 234))
      (eval `(multple-value-list ,(expand-expression '#1#)))))
