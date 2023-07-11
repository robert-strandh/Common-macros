(cl:in-package #:common-macros-test)

(define-test defparameter)

(define-test defparameter-no-documentation
  :parent defparameter
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn (defparameter ,name 234) ,name))
        (eval `(progn ,(expand-expression `(defparameter ,name)) ,name)))))
