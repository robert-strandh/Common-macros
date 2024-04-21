(cl:in-package #:common-macro-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro MULTIPLE-VALUE-BIND.
;;;
;;; We define this macro pretty much exactly as the HyperSpec says in
;;; the "Notes:" section on the page describing MULTIPLE-VALUE-BIND.
;;; As a slight optimization, if there is only one variable we reduce
;;; to LET. This is nice for e.g. SETF.

(defmacro multiple-value-bind (variables values-form &body body)
  (if (= (length variables) 1)
      `(let ((,(first variables) ,values-form)) ,@body)
      (let ((rest-variable (gensym)))
        `(multiple-value-call
             (lambda (&optional ,@variables &rest ,rest-variable)
               (declare (ignore ,rest-variable))
               ,@body)
           ,values-form))))
