(cl:in-package #:asdf-user)

(defsystem #:common-macros
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "when")
   (:file "unless")
   (:file "and")
   (:file "or")
   (:file "cond")))
