(cl:in-package #:common-macro-definitions)

(defgeneric wrap-in-setf-type-function (client name function environment))

(defmacro deftype (&environment environment name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(wrap-in-setf-type-function
       *client* name (ecc:parse-deftype name lambda-list body) environment)))
