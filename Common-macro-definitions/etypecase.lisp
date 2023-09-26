(cl:in-package #:common-macro-definitions)

;;; As with ECASE, the default for ETYPECASE is to signal an error.
(defmacro etypecase (keyform &rest clauses)
  (let* ((variable (gensym))
         (keys (collect-e/ctypecase-keys clauses))
         (final `(error 'etypecase-type-error
                        :datum ,variable
                        :expected-type '(member ,@keys))))
    `(let ((,variable ,keyform))
       ,(expand-e/ctypecase-clauses clauses variable final 'etypecase))))
