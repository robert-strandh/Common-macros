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
   #:ecase
   #:ccase
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
   #:declaim
   #:lambda
   #:prog1
   #:prog2
   #:pushnew
   #:remf
   #:in-package
   #:check-type
   #:dolist
   #:dotimes
   #:do))

(defpackage #:common-macros
  (:use #:common-lisp)
  (:local-nicknames (#:ico #:iconoclast)
                    (#:bld #:iconoclast-builder)
                    (#:ses #:s-expression-syntax)
                    (#:cmd #:common-macro-definitions)))
