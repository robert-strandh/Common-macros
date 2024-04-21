(cl:in-package #:asdf-user)

(defsystem "common-macro-definitions"
  :depends-on ("ecclesia"
               "common-macro-definitions-packages-extrinsic"
               "common-macro-definitions-low"
               "common-macro-definitions-shared"))
