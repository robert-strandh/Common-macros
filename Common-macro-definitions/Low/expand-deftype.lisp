(cl:in-package #:common-macro-definitions)

(defun expand-deftype
    (environment name lambda-list body setf-type-function-wrapper)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(funcall setf-type-function-wrapper
               name (ecc:parse-deftype name lambda-list body) environment)))
