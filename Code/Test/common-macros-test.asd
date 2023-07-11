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
   (:file "incf")
   (:file "or")
   (:file "pop")
   (:file "unless")
   (:file "when")))
