(cl:in-package #:common-macros-test)

(define-test defvar)

(define-test defvar-no-initial-value
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn (defvar ,name) (boundp ',name)))
        (eval `(progn ,(expand-expression `(defvar ,name))
                      (boundp ',name))))))

(define-test defvar-with-initial-value
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn (defvar ,name 234) ,name))
        (eval `(progn ,(expand-expression `(defvar ,name)) ,name)))))

(define-test defvar-check-update
  :parent defvar
  (let ((name (gensym)))
    (is #'equal
        (eval `(progn (defvar ,name 234)
                      (defvar ,name 345)
                      ,name))
        (eval `(progn
                 (defvar ,name 234)
                 ,(expand-expression `(defvar ,name 345)) ,name)))))
