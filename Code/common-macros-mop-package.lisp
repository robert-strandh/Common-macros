(cl:in-package #:common-lisp-user)

(defpackage #:common-macros-mop-package
  (:use)
  (:export #:ensure-class
           #:ensure-method
           #:class-prototype
           #:generic-function-class-names
           #:make-method-lambda))
