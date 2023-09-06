(cl:in-package #:common-macros)

(defmacro maybe-error (datum predicate expected-type offending-list)
  `(unless (,predicate ,datum)
     (error 'must-be-property-list
            :datum ,datum
            :expected-type ',expected-type
            :offending-list ,offending-list)))
     

(defmacro cmd:remf (place indicator &environment environment)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    (let ((indicator-value-variable (gensym))
          (store-var (car store-vars)))
      `(block nil
         (let ,(mapcar #'list vars vals)
           (let* ((,store-var ,reader-form)
                  (,indicator-value-variable ,indicator))
             (when (null ,store-var)
               (return nil))
             (maybe-error ,store-var consp list ,store-var)
             (maybe-error (cdr ,store-var) consp cons ,store-var)
             (when (eq ,indicator-value-variable (car ,store-var))
               (setq ,store-var (cddr ,store-var))
               ,writer-form
               (return t))
             (loop for rest on (cdr ,store-var) by #'cddr
                   ;; We know that REST is a CONS cell.
                   do (when (null (cdr rest))
                        ;; There are no more pairs to test, so we are
                        ;; done.
                        (return nil))
                      (maybe-error (cdr rest) atom list ,store-var)
                      (maybe-error (cddr rest) atom cons ,store-var)
                      (when (eq ,indicator-value-variable (cadr rest))
                        ;; We found a match.
                        (setf (cdr rest) (cdddr rest))
                        (return t)))))))))

;; (defmethod expand (client (ast ico:remf-ast) environment)
;;   (declare (ignore client))
;;   (multiple-value-bind
;;         (binding-asts store-variable-asts store-ast read-ast)
;;       (expand-place-ast (ico:place-ast ast) environment)
;;     (let ((indicator-value-variable (gensym))
;;           (store-variable-ast (first store-variable-asts)))
;;       (ablock 'nil
;;         (alet* (binding-asts
;;                 (b store-variable-ast read-ast)
;;                 (b (make-variable-name-ast indicator-value-variable)
;;                    (ico:indicator-ast ast)))
;;           (awhen (application 'null store-ast)
;;             (node* (:return) (1 :form (make-quote-ast 'nil))))
;;           (awhen (application
;;                   'eq
;;                   (make-variable-name-ast indicator-value-variable)
;;                   store-variable-ast)
;;             (node* (:setq)
;;               (1 :variable-name store-variable-ast)
;;               (1 :value (appliacation 'cddr store-variable-ast)))
;;             store-ast
;;             (node* (:return) (1 :form (make-quote-ast 't))))
          
;;; Either we wait until we have a LOOP AST, or we write
;;; this expansion so that it contains DO.
