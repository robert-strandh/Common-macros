(cl:in-package #:common-macros)

;;; This definition is taken from the Common Lisp standard.

(defmacro cmd:with-simple-restart
    ((restart-name format-control &rest format-arguments) &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
       :report (lambda (stream)
                 (format stream ,format-control ,@format-arguments))
       (values nil t))))
