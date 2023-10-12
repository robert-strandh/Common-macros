(cl:in-package #:common-macro-definitions)

(defmacro defmacro (name lambda-list &body body)
  (let ((expansion (ecc:parse-macro name lambda-list body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (macro-function ',name) ,expansion))))
