(cl:in-package #:common-macro-definitions)
;;; FIXME: Do more syntax verification.

(defgeneric defun-compile-time-action
    (client name lambda-list environment)
  (:method (client name lambda-list environment)
    ;; By default do nothing, since this is conforming.
    (declare (ignore client name lambda-list environment))))

(defmacro defun (&environment environment name lambda-list &body body)
  (expand-defun
   name lambda-list body
   (defun-compile-time-action *client* name lambda-list environment)))
