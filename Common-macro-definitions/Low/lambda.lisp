(cl:in-package #:common-macro-definitions)

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))
