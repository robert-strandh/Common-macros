(in-package #:common-macro-definitions)

(defgeneric wrap-in-setf-setf-expander (client name function environment))

(defmacro define-setf-expander (access-fn lambda-list &body body &environment environment)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(wrap-in-setf-setf-expander
       ;; DEFINE-SETF-EXPANDER just has an ordinary macro lambda list, ergo
       *client* access-fn (ecc:parse-macro access-fn lambda-list body environment)
       environment)
     ',access-fn))
