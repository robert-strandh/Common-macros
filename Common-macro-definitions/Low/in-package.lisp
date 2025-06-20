(cl:in-package #:common-macro-definitions)

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (or (find-package ',string-designator)
                       (error 'package-error :package ',string-designator)))))
