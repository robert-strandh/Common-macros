(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions-low"
  :serial t
  :components
  ((:file "condition-types")
   (:file "utilities")))
