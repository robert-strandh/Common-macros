(cl:in-package #:common-macros)

(defmacro and (&rest forms)
  (labels ((aux (forms)
             (if (null (cdr forms))
                 (car forms)
                 `(if ,(car forms)
                      ,(aux (cdr forms))
                      nil))))
    (if (null forms)
        t
        (aux forms))))
