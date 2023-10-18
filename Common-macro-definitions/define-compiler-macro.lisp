(cl:in-package #:common-macro-definitions)

(defmacro define-compiler-macro (name lambda-list &body body)
  (let ((expansion (ecc:parse-compiler-macro name lambda-list body)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name) ,expansion))))
