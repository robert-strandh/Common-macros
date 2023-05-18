(cl:in-package #:common-macros)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate an ordinary body such as a let or let* body that may
;;; contain declarations (but no documentation) into the declarations
;;; and the executable forms.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.

(defun separate-ordinary-body (body)
  (let ((pos (position-if-not (lambda (item)
                                (and (consp item)
                                     (eq (car item) 'declare)))
                              body)))
    (if (null pos)
        (values body '())
        (values (subseq body 0 pos) (subseq body pos)))))
