(cl:in-package #:common-macro-definitions)

(defmacro and (&body forms)
  (labels ((aux (forms)
             (if (null (cdr forms))
                 (car forms)
                 `(if ,(car forms)
                      ,(aux (cdr forms))
                      nil))))
    (if (null forms)
        t
        (aux forms))))
