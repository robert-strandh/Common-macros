(cl:in-package #:common-macro-definitions)

(defmacro defpackage (name &rest options)
  (expand-defpackage name options))
