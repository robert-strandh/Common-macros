(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use #:common-lisp)
  (:shadow #:macroexpand-1
           #:defmacro
           #:get-setf-expansion)
  (:export
   #:macroexpand-1
   #:defmacro
   #:gets-setf-expansion))
