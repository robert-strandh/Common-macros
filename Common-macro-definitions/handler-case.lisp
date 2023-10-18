(cl:in-package #:common-macro-definitions)

(defmacro handler-case (form &rest clauses)
  (let ((no-error-clause (assoc :no-error clauses)))
    (if no-error-clause
        ;; if we have a no-error clause, we handle it specially,
        ;; and use an inner handler-case with no no-error clause.
        (let ((normal-return (gensym "NORMAL-RETURN"))
              (error-return (gensym "ERROR-RETURN")))
          ;; If there's no error, it goes to the m-v-call.
          ;; If there is, and the handler-case returns, it aborts
          ;; back to error-return.
          `(block ,error-return
             (multiple-value-call (lambda ,@(rest no-error-clause))
               (block ,normal-return
                 (return-from ,error-return
                   (handler-case (return-from ,normal-return ,form)
                     ,@(remove no-error-clause clauses)))))))
        ;; Now in the usual no no-error case, what we do is expand
        ;; to a tagbody. All the handlers set a variable to the condition,
        ;; then GO to a body in the tagbody. This handles HANDLER-CASE's
        ;; unwinding behavior.
        ;; We also FLET the handlers with DX because why not?
        (let ((exit (gensym "HANDLER-CASE-EXIT"))
              (tags (loop for clause in clauses
                          collect (gensym "HANDLER-TAG")))
              (fnames (loop for clause in clauses
                            collect (gensym "HANDLER")))
              (condition (gensym "CONDITION"))
              ;; This is just to avoid our package name leaking in.
              (temp '#:temp))
          `(block ,exit
             (let ((,condition nil))
               (declare (ignorable ,condition))
               (tagbody
                  (flet (,@(loop for fname in fnames
                                 for tag in tags
                                 collect `(,fname (,temp)
                                            (setq ,condition ,temp)
                                            (go ,tag))))
                    (declare (dynamic-extent
                              ,@(loop for fname in fnames
                                      collect `(function ,fname))))
                    (return-from ,exit
                      (handler-bind
                          (,@(loop for (type) in clauses
                                   for fname in fnames
                                   collect `(,type (function ,fname))))
                        ,form)))
                  ;; Now the handler bodies.
                  ,@(loop for tag in tags
                          for (_ maybe-ll . body) in clauses
                          collect tag
                          collect `(return-from ,exit
                                     ,(etypecase maybe-ll
                                        ((cons symbol null)
                                         `(let ((,(first maybe-ll) ,condition))
                                            ,@body))
                                        (null `(locally ,@body))))))))))))
