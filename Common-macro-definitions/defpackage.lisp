(cl:in-package #:common-macro-definitions)

(defgeneric add-local-nickname (client))

(defmacro defpackage (name &rest options)
  (expand-defpackage name options (add-local-nickname *client*)))
