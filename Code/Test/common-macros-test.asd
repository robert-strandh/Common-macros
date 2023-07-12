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
   (:file "or")
   (:file "pop")
   (:file "prog1")
   (:file "prog2")
   (:file "push")
   (:file "unless")
   (:file "when")))
