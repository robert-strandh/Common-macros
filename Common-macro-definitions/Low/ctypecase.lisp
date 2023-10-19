(cl:in-package #:common-macro-definitions)

;;; As with CCASE, the default for CTYPECASE is is to signal a
;;; correctable error, and to allow the value to be altered by the
;;; STORE-VALUE restart.
(defmacro ctypecase (keyplace &body clauses &environment environment)
  (multiple-value-bind (vars vals store-vars writer-forms reader-forms)
      (get-setf-expansion *client* keyplace environment)
    (let* ((label (gensym))
           (keys (collect-e/ctypecase-keys clauses))
           (final `(restart-case (error 'ctypecase-type-error
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
              ,(expand-e/ctypecase-clauses clauses (car store-vars) final 'ctypecase)))))))
