(cl:in-package #:common-macros)

(defun proper-list-p (object)
  (numberp (ignore-errors (list-length object))))
