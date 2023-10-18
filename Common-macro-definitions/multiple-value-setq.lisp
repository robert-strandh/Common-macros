(cl:in-package #:common-macro-definitions)

(defmacro multiple-value-setq (vars value-form)
  (if (null vars)
      ;; skip SETF if unnecessary.
      `(values ,value-form)
      ;; As defined in CLHS.
      ;; FIXME: Validate that vars are symbols.
      `(values (setf (values ,@vars) ,value-form))))
