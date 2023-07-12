(cl:in-package #:common-macros-test)

(define-test defparameter)

(define-test defparameter-check-update
  :parent defparameter
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn #2=(defparameter ,name 234)
                      #1=(defparameter ,name 345)
                      ,name))
        (eval `(progn
                 #2#(defparameter ,name 234)
                 ,(expand-expression `#1#) ,name)))))
