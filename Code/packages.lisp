(cl:in-package #:common-lisp-user)

(defpackage #:common-macros
  (:use #:common-lisp)
  (:local-nicknames (#:ico #:iconoclast)
                    (#:bld #:iconoclast-builder)
                    (#:ses #:s-expression-syntax))
  (:shadow
   . #1=(#:when
         #:unless
         #:and
         #:or
         #:cond
         #:case))
  (:export . #1#))