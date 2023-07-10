(cl:in-package #:common-lisp-user)

(defpackage #:common-macros-test
  (:use #:common-lisp #:parachute)
  (:local-nicknames
   (#:ico #:iconoclast)
   (#:bld #:iconoclast-builder)
   (#:ses #:s-expression-syntax)
   (#:abp #:architecture.builder-protocol)
   (#:cm #:common-macros)))
