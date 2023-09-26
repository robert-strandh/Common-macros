(cl:in-package #:common-macro-definitions)

;;; Expand a list of clauses for ECASE or CCASE.  We turn the clauses
;;; into nested IFs, where the innermost form (final) depends on
;;; whether we use ecase or ccase.  We check that the list of clauses
;;; is a proper list, and that each clause is a proper list.
(defun expand-e/ccase-clauses (clauses variable final name)
  (if (null clauses)
      final
      (if (not (consp clauses))
          (error 'malformed-case-clauses
                 :clauses clauses)
          (let ((clause (car clauses)))
            (unless (and (proper-list-p clause)
                         (not (null clause)))
              (error 'malformed-case-clause
                     :clause clause))
            (let ((keys (car clause))
                  (forms (cdr clause)))
              (if (and (atom keys)
                       (not (null keys)))
                  `(if (eql ,variable ',keys)
                       (progn ,@forms)
                       ,(expand-e/ccase-clauses (cdr clauses) variable final name))
                  `(if (or ,@(eql-ify keys variable))
                       (progn ,@forms)
                       ,(expand-e/ccase-clauses (cdr clauses) variable final name))))))))

;;; For ECASE, the default is to signal a type error.
(defmacro ecase (keyform &body clauses)
  (let* ((variable (gensym))
         (keys (collect-e/ccase-keys clauses 'ecase))
         (final `(error 'ecase-type-error
                        :datum ,variable
                        :expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ccase-clauses clauses variable final 'ecase))))
