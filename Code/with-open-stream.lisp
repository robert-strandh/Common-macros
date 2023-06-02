(cl:in-package #:common-macros)

;;; This definition is brittle.  Use Iconoclast instead.

(defmacro cmd:with-open-stream ((stream-variable stream-form) &body body)
  (multiple-value-bind (declarations forms)
      (separate-ordinary-body body)
    `(let ((,stream-variable ,stream-form))
       ,@declarations
       (unwind-protect
            (progn ,@forms)
         (close ,stream-variable)))))
