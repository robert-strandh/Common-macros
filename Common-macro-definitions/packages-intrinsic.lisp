(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use #:common-lisp)
  (:local-nicknames (#:ecc #:ecclesia))
  (:shadow #:ensure-generic-function
           #:proclaim)
  (:export
   #:*client*
   #:wrap-in-method-lambda
   #:wrap-in-ensure-method
   #:defun-compile-time-action
   #:defclass-compile-time-action
   #:ensure-class-name
   #:defgeneric-compile-time-action
   #:ensure-generic-function
   #:wrap-in-setf-type-function
   #:wrap-in-make-method-lambda
   #:macro-function-exists-p))
