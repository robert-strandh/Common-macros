(cl:in-package #:common-macros-test)

(define-test defparameter)

(define-test defparameter-check-update
  :parent defparameter
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn (defparameter ,name 234)
                      (defparameter ,name 345)
                      ,name))
        (eval `(progn
                 (defparameter ,name 234)
                 ,(expand-expression `(defparameter ,name 345)) ,name)))))
