(cl:in-package #:common-macro-definitions)

;;; Check that the binding var is a symbol.
(defun binding-var-must-be-symbol (binding-var)
  (unless (symbolp binding-var)
    (error 'malformed-binding-var
           :datum binding-var)))

;;; Check that the list-form is a list
;;; FIXME: signal a warning if list-form is not a proper-list
(defun list-form-must-be-list (list-form)
  (unless (or (listp list-form) (symbolp list-form))
    (error 'malformed-list-form
           :datum list-form)))

;;; Check that the count form is a positive integer.
(defun count-form-must-be-nonnegative-integer (count-form)
  (unless (or (and (numberp count-form)
                   (not (minusp count-form)))
              (not (constantp count-form)))
    (error 'malformed-count-form
           :datum count-form)))

;;; Check that iteration body is a proper list.
(defun body-must-be-proper-list (body)
  (unless (proper-list-p body)
    (error 'malformed-body
           :datum body)))

(defun check-variable-clauses (variable-clauses)
  (unless (proper-list-p variable-clauses)
    (error 'malformed-variable-clauses
           :datum variable-clauses))
  (mapcar
   (lambda (clause)
     (unless (or (symbolp clause)
                 (and (consp clause)
                      (symbolp (car clause))
                      (or (null (cdr clause))
                          (null (cddr clause))
                          (null (cdddr clause)))))
       (error 'malformed-variable-clause
              :found clause)))
   variable-clauses))

(defun extract-bindings (variable-clauses)
  (mapcar
   (lambda (clause)
     (cond ((symbolp clause) clause)
           ((null (cdr clause)) (car clause))
           (t (list (car clause) (cadr clause)))))
   variable-clauses))

(defun extract-updates (variable-clauses)
  (if (null variable-clauses) '()
      (let ((clause (car variable-clauses)))
        (if (and (consp clause)
                 (not (null (cddr clause))))
            (list* (car clause)
                   (caddr clause)
                   (extract-updates (cdr variable-clauses)))
            (extract-updates (cdr variable-clauses))))))

;;; This implementation does not use any iteration construct, nor any
;;; operations on sequences (other than the ones we define ourself
;;; here).  Implementations can therefore load this file very early on
;;; in the bootstrap process.  It allows for operations on sequences
;;; and the loop macro to be defined in terms of the macros defined
;;; here.

(defun do-dostar-expander
    (let-type setq-type variable-clauses end-test body)
  ;; Do some syntax checking.
  (check-variable-clauses variable-clauses)
  (body-must-be-proper-list body)
  (unless (and (proper-list-p end-test)
               (not (null end-test)))
    (error 'malformed-end-test
           :found end-test))
  (multiple-value-bind (declarations forms)
      (separate-ordinary-body body)
    (let ((start-tag (gensym)))
      `(block nil
         (,let-type ,(extract-bindings variable-clauses)
                    ,@declarations
                    (tagbody
                       ,start-tag
                       (when ,(car end-test)
                         (return
                           (progn ,@(cdr end-test))))
                       ,@forms
                       (,setq-type ,@(extract-updates
                                        variable-clauses))
                       (go ,start-tag)))))))
