(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions-intrinsic"
  :depends-on ("ecclesia"
               "common-macro-definitions-packages-intrinsic"
               "common-macro-definitions-shared"))
