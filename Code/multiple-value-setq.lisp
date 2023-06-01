(cl:in-package #:common-macros)

(defmacro cmd:multiple-value-setq (symbols form)
  `(values (setf (values ,@symbols) ,form)))
