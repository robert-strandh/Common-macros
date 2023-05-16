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
   (:file "incf")
   (:file "decf")
   (:file "push")
   (:file "pop")))
