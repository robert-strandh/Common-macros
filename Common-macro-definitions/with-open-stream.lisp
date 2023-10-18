(cl:in-package #:common-macro-definitions)

(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (declarations forms) (ecc:separate-ordinary-body body)
    `(let ((,var ,stream))
       (declare (dynamic-extent ,var)) ; per CLHS
       ,@declarations
       (unwind-protect (progn ,@forms)
         (close ,var)))))
