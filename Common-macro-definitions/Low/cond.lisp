(cl:in-package #:common-macro-definitions)

(defmacro cond (&body clauses)
  (labels ((aux (clauses)
             (if (null clauses)
                 nil
                 (let ((clause (car clauses)))
                   (if (not (and (ecc:proper-list-p clause)
                                 (not (null clause))))
                       (error 'malformed-cond-clause
                              :clause clause)
                       (if (null (cdr clause))
                           `(or ,(car clause)
                                ,(aux (cdr clauses)))
                           `(if ,(car clause)
                                (progn ,@(cdr clause))
                                ,(aux (cdr clauses)))))))))
    (aux clauses)))
