(cl:in-package #:common-macros)

;; (defmacro cmd:ecase (&whole form keyform &rest clauses)
;;   (declare (ignore keyform clauses))
;;   (let* ((builder (make-instance 'bld:builder))
;;          (syntax (ses:find-syntax 'ecase))
;;          (ast (ses:parse builder syntax form))
;;          (keyform-ast (ico:form-ast ast))
;;          (keyform (ico:form keyform-ast))
;;          (form-variable (gensym)))
;;     (labels ((process-clauses (clause-asts)
;;                (if (null clause-asts)
;;                    `(error 'type-error
;;                            :datum ,form-variable
;;                            :expected-type
;;                            '(member ,@(loop for clause-ast
;;                                               in (ico:clause-asts ast)
;;                                             for key-asts
;;                                               = (ico:key-asts clause-ast)
;;                                             append
;;                                             (mapcar #'ico:key key-asts))))
;;                    (let* ((clause-ast (first clause-asts))
;;                           (form-asts (ico:form-asts clause-ast))
;;                           (forms (mapcar #'ico:form form-asts)))
;;                      (let* ((key-asts (ico:key-asts clause-ast))
;;                             (keys (mapcar #'ico:key key-asts))
;;                             (tests (mapcar (lambda (key)
;;                                              `(eql ,form-variable ,key))
;;                                            keys)))
;;                        `(if (or ,@tests)
;;                             (progn ,@forms)
;;                             ,(process-clauses (rest clause-asts))))))))
;;       `(let (,form-variable ,keyform)
;;          ,(process-clauses (ico:clause-asts ast))))))
