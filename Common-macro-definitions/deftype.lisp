(cl:in-package #:common-macro-definitions)

(defgeneric setf-type-function-wrapper (client))

(defmacro deftype (&environment environment name lambda-list &body body)
  (expand-deftype
   environment name lambda-list body (setf-type-function-wrapper *client*)))
