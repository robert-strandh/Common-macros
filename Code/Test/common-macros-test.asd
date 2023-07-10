(cl:in-package #:asdf-user)

(defsystem #:common-macros-test
  :depends-on (#:common-macros
               #:parachute)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "decf")
   (:file "incf")
   (:file "unless")
   (:file "when")))
