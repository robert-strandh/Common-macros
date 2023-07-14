(cl:in-package #:common-macros-test)

(define-test defclass)

(define-test defclass-empty
  :parent defclass
  (let ((name (gensym)))
    (true (eval `(progn ,(expand-expression `(defclass ,name () ()))
                        (not (null (find-class ,name))))))))
