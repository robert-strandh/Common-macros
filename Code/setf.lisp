(cl:in-package #:common-macros)

;;; This macro should be rewritten when the s-expression-syntax
;;; library can handle SETF.

(defmacro cmd:setf
    (&whole form &environment env &rest pairs)
  (cond ((null pairs)
         'nil)
        ((null (rest pairs))
         (error 'odd-number-of-arguments-to-setf :form form))
        ((null (rest (rest pairs)))
         (multiple-value-bind (variables
                               values
                               store-variables
                               writer-form
                               reader-form)
             (get-setf-expansion (first pairs) env)
           (declare (ignore reader-form))
           `(let* ,(mapcar #'list variables values)
              ;; Optimize a bit when there is only one store variable.
              ,(if (= 1 (length store-variables))
                   `(let ((,(first store-variables) ,(second pairs)))
                      ,writer-form)
                   `(multiple-value-bind ,store-variables
                        ,(second pairs)
                      ,writer-form)))))
        (t
         `(progn (setf ,(first pairs) ,(second pairs))
                 (setf ,@(rest (rest pairs)))))))

(defun expand-place-value-pair (client place-ast values-ast environment)
  (multiple-value-bind (binding-asts store-variable-asts store-ast read-ast)
      (expand-place-ast client place-ast environment)
    (alet* (binding-asts)
      (node* (:multiple-value-bind)
        (* :name store-variable-asts)
        (1 :values values-ast)
        (1 :form store-ast))
      read-ast)))
