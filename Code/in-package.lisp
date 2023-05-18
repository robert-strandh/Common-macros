(cl:in-package #:common-macros)

(defmacro cmd:in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-package ',name))))
