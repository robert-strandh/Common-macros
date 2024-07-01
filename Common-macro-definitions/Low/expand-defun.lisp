(cl:in-package #:common-macro-definitions)

(defun expand-defun (name lambda-list body compile-time-action)
  (multiple-value-bind (declarations documentation forms)
      (ecc:separate-function-body body)
    `(progn
       (eval-when (:compile-toplevel)
         ,compile-time-action)
       (eval-when (:load-toplevel :execute)
         (setf (fdefinition ',name)
               (lambda ,lambda-list
                 ,@declarations
                 ,@(if (null documentation)
                       '()
                       (list documentation))
                 (block ,(if (symbolp name) name (second name))
                   ,@forms)))
         ',name))))
