(cl:in-package #:common-macro-definitions)

(defmacro prog* (bindings &body body)
  (multiple-value-bind (declarations items)
      (ecc:separate-ordinary-body body)
    `(block nil
       (let* ,bindings
         ,@declarations
         (tagbody ,@items)))))
