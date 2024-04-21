(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions-low"
  :serial t
  :components
  ((:file "condition-types")
   (:file "utilities")
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
   (:file "rotatef")))
