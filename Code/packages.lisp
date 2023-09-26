(cl:in-package #:common-lisp-user)

(defpackage #:common-macros
  (:use #:common-lisp)
  (:shadow #:get-setf-expansion)
  (:local-nicknames
   (#:ico #:iconoclast)
   (#:bld #:iconoclast-builder)
   (#:abp #:architecture.builder-protocol)
   (#:ses #:s-expression-syntax))
  (:export #:with-builder
           #:with-ast-origin
           #:destructure-lambda-list
           #:get-setf-expansion
           #:expand))

