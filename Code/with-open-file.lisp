(cl:in-package #:common-macros)

;;; This definition is brittle.  Use Iconoclast instead.

(defmacro cmd:with-open-file
    ((stream-variable filespec &rest options) &body body)
  (let ((abort-variable (gensym)))
    (multiple-value-bind (declarations forms)
        (separate-ordinary-body body)
      `(let ((,stream-variable (open ,filespec ,@options))
             (,abort-variable t))
         ,@declarations
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@forms)
                (setq ,abort-variable nil))
           (unless (null ,stream-variable)
             (close ,stream-variable :abort ,abort-variable)))))))
