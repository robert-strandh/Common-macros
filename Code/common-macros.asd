(cl:in-package #:asdf-user)

(defsystem #:common-macros
  :depends-on (#:iconoclast-builder)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "conditions")
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
   (:file "defvar")
   (:file "defparameter")
   (:file "defun")
   (:file "defclass")
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
   (:file "with-accessors")
   (:file "with-open-file")
   (:file "with-open-stream")
   (:file "with-slots")))
