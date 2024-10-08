(cl:in-package #:common-macro-definitions)

(defmacro incf (&environment env place &optional (delta-form 1))
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion *client* place env)
    (if (null vars)
        `(let ((,(first store-vars) (+ ,reader-form ,delta-form)))
           ,writer-form)
        `(let* (,@(mapcar (lambda (var val) `(,var ,val)) vars vals)
                (,(first store-vars) (+ ,reader-form ,delta-form)))
           ,writer-form))))
