(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use #:common-lisp)
  (:local-nicknames (#:ecc #:ecclesia))
  (:shadow #:macroexpand-1
           #:defmacro
           #:get-setf-expansion
           #:ensure-generic-function
           #:proclaim)
  (:export
   #:*client*
   #:macroexpand-1
   #:defmacro
   #:get-setf-expansion
   #:wrap-in-method-lambda
   #:wrap-in-ensure-method
   #:defun-compile-time-action
   #:defclass-compile-time-action
   #:ensure-class
   #:defgeneric-compile-time-action
   #:ensure-generic-function
   #:wrap-in-setf-type-function
   #:proclaim
   #:macro-function-exists-p))
