(cl:in-package #:common-macros)

;;;; This definition is temporary, and it does very little
;;;; error checking.  It should be replaced by a definition
;;;; that uses Iconoclast.

(defun expand-typecase-clauses (clauses key-variable)
  (if (null clauses)
      'nil
      (let ((clause (car clauses)))
        (if (or (eq (car clause) 'otherwise)
                (eq (car clause) t))
            `(progn ,@(cdr clause))
            (let ((type (car clause))
                  (forms (cdr clause)))
              `(if (typep ,key-variable ',type)
                   (progn ,@forms)
                   ,(expand-typecase-clauses
                     (cdr clauses) key-variable)))))))

(defmacro cmd:typecase (keyform &rest clauses)
  (let ((key-variable (gensym)))
    `(let ((,key-variable ,keyform))
       ,(expand-typecase-clauses clauses key-variable))))
