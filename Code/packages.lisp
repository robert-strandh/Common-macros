(cl:in-package #:common-lisp-user)

(defpackage #:common-macros
  (:use #:common-lisp)
  (:shadow
   . #1=(#:when
         #:unless
         #:and
         #:or
         #:cond))
  (:export . #1#))
