(cl:in-package #:common-macro-definitions)

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-package ',string-designator))))
