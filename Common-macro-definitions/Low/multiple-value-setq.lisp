(cl:in-package #:common-macro-definitions)

(defmacro multiple-value-setq (vars value-form)
  (mapc #'check-variable-name vars)
  (if (null vars)
      ;; skip SETF if unnecessary.
      `(values ,value-form)
      ;; As defined in CLHS.
      `(values (setf (values ,@vars) ,value-form))))
