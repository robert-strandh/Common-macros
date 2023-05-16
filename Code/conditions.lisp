(cl:in-package #:common-macros)

(define-condition malformed-cond-clauses (program-error)
  ((%clauses :initarg :clauses :reader clauses))
  (:report (lambda (condition stream)
             (format stream
                     "A proper list of COND clauses was expected~@
                      but the following was given instead:~@
                      ~s"
                     (clauses condition)))))

(define-condition malformed-cond-clause (program-error)
  ((%clause :initarg :clause :reader clause))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed COND clause:~@
                      ~s"
                     (clause condition)))))

