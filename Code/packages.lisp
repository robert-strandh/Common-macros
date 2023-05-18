(cl:in-package #:common-lisp-user)

(defpackage #:common-macro-definitions
  (:use)
  (:export
   #:when
   #:unless
   #:and
   #:or
   #:cond
   #:case
   #:incf
   #:decf
   #:push
   #:pop
   #:return
   #:nth-value
   #:shiftf
   #:rotatef
   #:defvar
   #:defparameter
   #:defun
   #:declaim))

(defpackage #:common-macros
  (:use #:common-lisp)
  (:local-nicknames (#:ico #:iconoclast)
                    (#:bld #:iconoclast-builder)
                    (#:ses #:s-expression-syntax)
                    (#:cmd #:common-macro-definitions)))
