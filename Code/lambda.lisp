(cl:in-package #:common-macros)

;;; FIXME: define this macro using Iconoclast for better syntax
;;; verification.
(defmacro cmd:lambda (&whole form &rest arguments)
  (declare (ignore arguments))
  `(function ,form))
