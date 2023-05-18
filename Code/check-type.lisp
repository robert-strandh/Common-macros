(cl:in-package #:common-macros)

;;; This definition is from the Common Lisp standard.  The STRING
;;; argument is not used.
(defmacro cmd:check-type (place typespec string)
  (declare (ignore string))
  `(assert (typep ,place ',typespec) (,place)
           'type-error :datum ,place :expected-type ',typespec))
