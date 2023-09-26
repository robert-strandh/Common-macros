(cl:in-package #:common-macro-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper functions for macros CASE, ECASE, CCASE.
;;;
;;; A normal CASE/ECASE/CCASE clause has the form (KEYS FORM*) where
;;; KEYS is a designator for a list of objects, except that for CASE,
;;; the symbols T and OTHERWISE may not be used as such.  Instead,
;;; when T or OTHERWISE are present in the CAR of a clause, then they
;;; do not designate a list of objects, and instead that clause is an
;;; otherwise-clause.  For ECASE and CCASE, T and OTHERWISE can be
;;; used as as designators for lists, and they then designate the
;;; singleton list containing itself.
;;;
;;; In the glossary of the HyperSpec (under "list designator"), we
;;; learn that a list designator is ether a non-NIL atom, in which
;;; case the denoted list is the list containing that one atom, or
;;; else it is a proper list, and the denoted list is that list.  In
;;; particular, this means that if NIL (or equivalently `()') is used
;;; in the CAR of a CASE clause, then the denoted list is the empty
;;; list and NOT the list containing NIL.  Thus, to obtain the
;;; singleton list containing NIL, the user has to use `(NIL)'.

;;; Take a list of keys (known to be a proper list), and the name of a
;;; variable, and produce a list of forms (eql <variable> key).
(defun eql-ify (keys variable)
  (if (null keys)
      '()
      (cons `(eql ,variable ',(car keys))
            (eql-ify (cdr keys) variable))))

;;; Collect a list of all the keys for ecase or ccase
;;; to be used as the `exptected type' in error reporting.
(defun collect-e/ccase-keys (clauses name)
  (if (null clauses)
      nil
      (append
       (let ((keys (caar clauses)))
         (if (and (atom keys)
                  (not (null keys)))
             (list keys)
             (if (not (proper-list-p keys))
                 (error 'malformed-keys
                        :keys keys)
                 keys)))
       (collect-e/ccase-keys (cdr clauses) name))))

;;; This function turns a list of CASE clauses into nested IFs.  It
;;; checks that the list of clauses is a proper list and that each
;;; clause is also a proper list.  It also checks that, if there is an
;;; otherwise clause, it is the last one.

(defun expand-case-clauses (clauses variable)
  (if (null clauses)
      'nil
      (if (not (consp clauses))
          (error 'malformed-case-clauses
                 :clauses clauses)
          (let ((clause (car clauses)))
            (unless (and (proper-list-p clause)
                         (not (null clause)))
              (error 'malformed-case-clause
                     :clause clause))
            (if (or (eq (car clause) 'otherwise)
                    (eq (car clause) t))
                (if (null (cdr clauses))
                    `(progn ,@(cdr clause))
                    (error 'otherwise-clause-not-last
                           :clauses (cdr clauses)))
                ;; it is a normal clause
                (let ((keys (car clause))
                      (forms (cdr clause)))
                  (if (and (atom keys)
                           (not (null keys)))
                      `(if (eql ,variable ',keys)
                           (progn ,@forms)
                           ,(expand-case-clauses (cdr clauses) variable))
                      (if (not (proper-list-p keys))
                          (error 'malformed-keys
                                 :keys keys)
                          `(if (or ,@(eql-ify keys variable))
                               (progn ,@forms)
                               ,(expand-case-clauses (cdr clauses) variable))))))))))

(defmacro case (keyform &body clauses)
  (let ((variable (gensym)))
    `(let ((,variable ,keyform))
       ,(expand-case-clauses clauses variable))))
