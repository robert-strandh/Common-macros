(cl:in-package #:common-macros)

(defmacro pop (place &environment environment)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place environment)
    `(let* (,(mapcar #'list vars vals)
            (,(car store-vars) ,reader-form))
       (if (listp ,(car store-vars))
           (prog1 (car ,(car store-vars))
             (setq ,(car store-vars) (cdr ,(car store-vars)))
             ,writer-form)
           (error 'must-be-list
                  :datum ',(car store-vars))))))
