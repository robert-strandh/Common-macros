(cl:in-package #:asdf-user)

(defsystem #:common-macros-test
  :depends-on (#:common-macros
               #:parachute)
  :serial t
  :components
  ((:file "packages")))
