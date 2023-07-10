(cl:in-package #:asdf-user)

(defsystem #:common-macros-test
  :depends-on (#:common-macros)
  :serial t
  :components
  ((:file "packages")))
