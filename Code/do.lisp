(cl:in-package #:common-macros)

(defmacro cmd:do (variable-clauses end-test &body body)
  (multiple-value-bind (declarations forms)
      (separate-ordinary-body body)
    (let ((start-tag (gensym)))
      `(block nil
         (let ,(extract-bindings variable-clauses)
           ,@declarations
           (tagbody
              ,start-tag
              (when ,(car end-test)
                (return
                  (progn ,@(cdr end-test))))
              ,@forms
              (psetq ,@(extract-updates variable-clauses))
              (go ,start-tag)))))))
