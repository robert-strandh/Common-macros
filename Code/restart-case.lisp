(cl:in-package #:common-macros)

(defun transform-restart-case-options (clause-ast)
  (with-accessors ((interactive-ast ico:interactive-ast)
                   (report-ast ico:report-ast)
                   (test-ast ico:test-ast))
      clause-ast
    `(,@(if (null interactive-ast)
            '()
            `(:interactive
              (function 
               ,(if (typep interactive-ast 'ico:function-name-ast)
                    (ico:name interactive-ast)
                    (ico:form (ico:form-ast interactive-ast))))))
      ,@(if (null report-ast)
            '()
            `(:report
              (function 
               ,(cond ((typep report-ast 'ico:unparsed-form-ast)
                       `(lambda (stream)
                          (write-string ,(ico:form report-ast))))
                      ((typep report-ast 'ico:function-name-ast)
                       (ico:name report-ast))
                      (t
                       (ico:form (ico:form-ast report-ast)))))))
      ,@(if (null test-ast)
            '()
            `(:test
              (function 
               ,(if (typep test-ast 'ico:function-name-ast)
                    (ico:name test-ast)
                    (ico:form (ico:form-ast test-ast)))))))))

;;; FIXME: handle restarts associated with a condition.
(defmacro cmd:restart-case (&whole form restartable-form &rest clauses)
  (declare (ignore restartable-form clauses))
  (let* ((builder (make-instance 'bld:builder))
         (syntax (ses:find-syntax 'restart-case))
         (ast (ses:parse builder syntax form))
         (clause-asts (ico:clause-asts ast)))
    (let ((block-name (gensym))
          (variable-name (gensym))
          (tagbody-tags (loop for clause-ast in clause-asts
                              collect (gensym))))
      `(block ,block-name
         (let ((,variable-name nil))
           (tagbody
              (restart-bind
                  ,(loop for clause-ast in clause-asts
                         for tagbody-tag in tagbody-tags
                         collect
                         `((,(ico:name (ico:name-ast clause-ast))
                            (lambda (&rest temp)
                              (setq ,variable-name temp)
                              (go ,tagbody-tag)))
                           ,@(transform-restart-case-options clause-ast)))
                (return-from ,block-name ,(ico:form (ico:form-ast ast))))
              ,@(loop for clause-ast in clause-asts
                      for tagbody-tag in tagbody-tags
                      collect tagbody-tag
                      collect `(return-from ,block-name
                                 (apply (lambda
                                          ,(ico:unparse-lambda-list-ast
                                            (ico:lambda-list-ast ast))
                                          ;; FIXME: handle declarations
                                          ,@(loop for form-ast
                                                    in (ico:form-asts ast)
                                                  collect
                                                  (ico:form form-ast)))
                                        ,variable-name)))))))))
                                        
        
