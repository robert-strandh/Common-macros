(cl:in-package #:common-macro-definitions)

(defmacro with-input-from-string ((var string &key index (start 0) end) &body body)
  (check-variable-name var)
  (if (null index)
      ;; simple case
      `(let ((,var (make-string-input-stream ,string ,start ,end))) ,@body)
      ;; Have to update INDEX, so more elaborate.
      (multiple-value-bind (declarations forms) (ecc:separate-ordinary-body body)
        `(let ((,var (make-string-input-stream ,string ,start ,end)))
           ,@declarations
           (unwind-protect
                (multiple-value-prog1 (progn ,@forms)
                  (setf ,index (file-position ,var)))
             (close ,var))))))
