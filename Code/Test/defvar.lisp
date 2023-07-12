(cl:in-package #:common-macros-test)

(define-test defvar)

(define-test defvar-no-initial-value
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn #1=(defvar ,name) #2=(boundp ',name)))
        (eval `(progn ,(expand-expression `#1#) #2#)))))

(define-test defvar-with-initial-value
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn #1=(defvar ,name 234) ,name))
        (eval `(progn ,(expand-expression `#1#) ,name)))))

(define-test defvar-check-update
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn #2=(defvar ,name 234)
                      #1=(defvar ,name 345)
                      ,name))
        (eval `(progn
                 #2#
                 ,(expand-expression `#1#) ,name)))))
