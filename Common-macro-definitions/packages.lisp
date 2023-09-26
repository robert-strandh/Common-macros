(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use #:common-lisp)
  (:shadow #:macroexpand-1
           #:defmacro
           #:get-setf-expansion
           #:ensure-generic-function)
  (:export
   #:macroexpand-1
   #:defmacro
   #:get-setf-expansion
   #:wrap-in-method-lambda
   #:wrap-in-ensure-method
   #:defun-compile-time-action
   #:defclass-compile-time-action
   #:ensure-class
   #:defgeneric-compile-time-action
   #:ensure-generic-function))
