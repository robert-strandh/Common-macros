(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use #:common-lisp)
  (:local-nicknames (#:ecc #:ecclesia))
  (:shadow #:ensure-generic-function
           #:proclaim)
  (:export
   #:*client*
   #:defun-compile-time-action
   #:defclass-compile-time-action
   #:ensure-class-name
   #:defgeneric-compile-time-action
   #:ensure-generic-function
   #:wrap-in-setf-type-function
   #:make-method-lambda-wrapper
   #:ensure-method-wrapper
   #:macro-function-exists-p))
