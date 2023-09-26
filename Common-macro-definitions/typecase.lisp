(cl:in-package #:common-macro-definitions)

;;; Turn a list of TYPECASE clauses into nested IFs.  We check that
;;; the list of clauses is a proper list, that each clause is a proper
;;; list as well, and that, if there is an otherwise clause, it is the
;;; last one.
(defun expand-typecase-clauses (clauses variable)
  (if (null clauses)
      'nil
      (if (not (consp clauses))
          (error 'malformed-typecase-clauses
                 :clauses clauses)
          (let ((clause (car clauses)))
            (unless (and (proper-list-p clause)
                         (not (null clause)))
              (error 'malformed-typecase-clause
                     :clause clause))
            (if (or (eq (car clause) 'otherwise)
                    (eq (car clause) t))
                (if (null (cdr clauses))
                    `(progn ,@(cdr clause))
                    (error 'otherwise-clause-not-last
                           :clauses (cdr clauses)))
                ;; it is a normal clause
                (let ((type (car clause))
                      (forms (cdr clause)))
                  `(if (typep ,variable ',type)
                       (progn ,@forms)
                       ,(expand-typecase-clauses (cdr clauses) variable))))))))

;;; Collect a list of all the types for etypecase or ctypecase
;;; to be used as the `exptected type' in error reporting.
(defun collect-e/ctypecase-keys (clauses)
  (if (null clauses)
      nil
      (cons (caar clauses)
            (collect-e/ctypecase-keys (cdr clauses)))))

;;; Turn a list of clauses for ETYPCASE or CTYPECASE into nested IFs.
;;; We check that the list of clauses is a proper list, and that each
;;; clause is a proper list.  The default case depends on whether we
;;; have a CCTYPECASE or an ETYPECASE, so we pass that as an argument
;;; (final).
(defun expand-e/ctypecase-clauses (clauses variable final name)
  (if (null clauses)
      final
      (if (not (consp clauses))
          (error 'malformed-typecase-clauses
                 :clauses clauses)
          (let ((clause (car clauses)))
            (unless (and (proper-list-p clause)
                         (not (null clause)))
              (error 'malformed-typecase-clause
                     :clause clause))
            (let ((type (car clause))
                  (forms (cdr clause)))
              `(if (typep ,variable ',type)
                   (progn ,@forms)
                   ,(expand-e/ctypecase-clauses (cdr clauses) variable final name)))))))

(defmacro typecase (keyform &body clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-typecase-clauses clauses variable))))
