(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions"
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "when")
   (:file "unless")
   (:file "and")
   (:file "or")
   (:file "cond")
   (:file "case")
   (:file "ecase")
   (:file "ccase")
   (:file "incf")
   (:file "decf")
   (:file "push")
   (:file "pop")
   (:file "return")
   (:file "multiple-value-bind")
   (:file "multiple-value-list")
   (:file "multiple-value-setq")
   (:file "nth-value")
   (:file "shiftf")
   (:file "rotatef")
   (:file "defmethod")
   (:file "defvar")
   (:file "defpackage")
   (:file "defparameter")
   (:file "defun")
   (:file "defclass")
   (:file "define-condition")
   (:file "defgeneric")
   (:file "declaim")
   (:file "lambda")
   (:file "prog")
   (:file "progstar")
   (:file "prog1")
   (:file "prog2")
   (:file "pushnew")
   (:file "remf")
   (:file "ignore-errors")
   (:file "in-package")
   (:file "check-type")
   (:file "dolist")
   (:file "dotimes")
   (:file "do")
   (:file "dostar")
   (:file "setf")
   (:file "psetf")
   (:file "psetq")
   (:file "restart-case")
   (:file "typecase")
   (:file "with-accessors")
   (:file "with-input-from-string")
   (:file "with-open-file")
   (:file "with-open-stream")
   (:file "with-output-to-string")
   (:file "with-simple-restart")
   (:file "with-slots")))