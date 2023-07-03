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
   #:multiple-value-bind
   #:multiple-value-list
   #:multiple-value-setq
   #:nth-value
   #:shiftf
   #:rotatef
   #:defmethod
   #:defvar
   #:defpackage
   #:defparameter
   #:defun
   #:defclass
   #:define-condition
   #:defgeneric
   #:declaim
   #:lambda
   #:prog
   #:prog*
   #:prog1
   #:prog2
   #:pushnew
   #:remf
   #:ignore-errors
   #:in-package
   #:check-type
   #:dolist
   #:dotimes
   #:do
   #:do*
   #:setf
   #:psetf
   #:psetq
   #:restart-case
   #:typecase
   #:with-accessors
   #:with-input-from-string
   #:with-open-file
   #:with-open-stream
   #:with-output-to-string
   #:with-simple-restart
   #:with-slots))

(defpackage #:common-macros
  (:use #:common-lisp)
  (:local-nicknames
   (#:ico #:iconoclast)
   (#:bld #:iconoclast-builder)
   (#:abp #:architecture.builder-protocol)
   (#:ses #:s-expression-syntax)
   (#:cmd #:common-macro-definitions)))
