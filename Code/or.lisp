(cl:in-package #:common-macros)

(defmacro cmd:or (&rest forms)
  (labels ((aux (forms)
             (if (null (cdr forms))
                 (car forms)
                 (let ((temp-var (gensym)))
                   `(let ((,temp-var ,(car forms)))
                      (if ,temp-var
                          ,temp-var
                          ,(aux (cdr forms))))))))
    (if (null forms)
        nil
        (aux forms))))
