(cl:in-package #:common-macro-definitions)
;;; FIXME: Do more syntax verification.

(defgeneric defun-compile-time-action
    (client name lambda-list environment))

(defmacro defun (&environment environment name lambda-list &body body)
  (multiple-value-bind (declarations documentation forms)
      (ecc:separate-function-body body)
    `(progn
       (eval-when (:compile-toplevel)
         ,(defun-compile-time-action *client* name lambda-list environment))
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
