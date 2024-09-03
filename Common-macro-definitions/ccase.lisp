(cl:in-package #:common-macro-definitions)

;;; For CCASE, the default is to signal a correctable error, allowing
;;; a new value to be stored in the place passed as argument to CCASE,
;;; using the restart STORE-VALUE.  We use GET-SETF-EXPANSION so as to
;;; avoid multiple evaluation of the subforms of the place, even
;;; though the HyperSpec allows such multiple evaluation.
(defmacro ccase (keyplace &body clauses &environment environment)
  (multiple-value-bind (vars vals store-vars writer-forms reader-forms)
      (get-setf-expansion *client* keyplace environment)
    (let* ((label (gensym))
           (keys (collect-e/ccase-keys clauses 'ccase))
           (final `(restart-case (error 'ccase-type-error
                                        :datum ,(car store-vars)
                                        :expected-type '(member ,@keys))
                     (store-value (v)
                       :interactive
                       (lambda ()
                         (format *query-io*
                                 "New value: ")
                         (list (read *query-io*)))
                       :report "Supply a new value"
                       (setq ,(car store-vars) v)
                       ,writer-forms
                       (go ,label)))))
      `(let* ,(mapcar #'list vars vals)
         (declare (ignorable ,@vars))
         (multiple-value-bind ,store-vars ,reader-forms
           (tagbody
              ,label
              ,(expand-e/ccase-clauses clauses (car store-vars) final 'ccase)))))))
