(cl:in-package #:common-macros)

;;; This definition is brittle.  Use Iconoclast instead.

(defmacro cmd:with-open-stream ((stream-variable stream-form) &body body)
  (let ((position
          (position-if
           (lambda (declaration-or-form)
             (and (consp declaration-or-form)
                  (eq (car declaration-or-form) 'declare)))
           body
           :from-end t)))
    `(let ((,stream-variable ,stream-form))
       ,@(if (null position)
             '()
             (subseq body 0 (1+ position)))
       (unwind-protect
            (progn ,@(if (null position)
                         body
                         (subseq body (1+ position))))
         (close ,stream-variable)))))
