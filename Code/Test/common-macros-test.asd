(cl:in-package #:asdf-user)

(defsystem #:common-macros-test
  :depends-on (#:common-macros
               #:parachute)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "and")
   (:file "decf")
   (:file "defparameter")
   (:file "defvar")
   (:file "dolist")
   (:file "dotimes")
   (:file "incf")
   (:file "multiple-value-bind")
   (:file "multiple-value-list")
   (:file "multiple-value-setq")
   (:file "or")
   (:file "pop")
   (:file "prog1")
   (:file "prog2")
   (:file "push")
   (:file "unless")
   (:file "with-simple-restart")
   (:file "with-slots")
   (:file "when")))
