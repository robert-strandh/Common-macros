(cl:in-package #:common-macros)

;;; This definition is brittle.  Use Iconoclast instead.

(defmacro cmd:with-open-file
    ((stream-variable filespec &rest options) &body body)
  (let ((position
          (position-if
           (lambda (declaration-or-form)
             (and (consp declaration-or-form)
                  (eq (car declaration-or-form) 'declare)))
           body
           :from-end t))
        (abort-variable (gensym)))
    `(let ((,stream-variable (open ,filespec ,@options))
           (,abort-variable t))
       ,@(if (null position)
             '()
             (subseq body 0 (1+ position)))
       (unwind-protect
            (multiple-value-prog1
                (progn ,(if (null position)
                            body
                            (subseq body (1+ position))))
              (setq ,abort-variable nil))
         (unless (null ,stream-variable)
           (close ,stream-variable :abort ,abort-variable))))))
    
