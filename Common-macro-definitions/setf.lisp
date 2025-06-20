(cl:in-package #:common-macro-definitions)

(defun expand-1-setf (place new-value-form env)
  (multiple-value-bind (variables
                        values
                        store-variables
                        writer-form
                        reader-form)
      (get-setf-expansion *client* place env)
    (declare (ignore reader-form))
    `(let* ,(mapcar #'list variables values)
       ;; Optimize a bit when there is only one store variable.
       ,(if (= 1 (length store-variables))
            `(let ((,(first store-variables) ,new-value-form))
               ,writer-form)
            `(multiple-value-bind ,store-variables
                 ,new-value-form
               ,writer-form)))))

(defmacro setf (&whole form &environment env &rest pairs)
  `(progn
     ,@(loop for sub on pairs by #'cddr
             when (null (cdr sub))
               do (error 'odd-number-of-arguments-to-setf :form form)
             collect (expand-1-setf (first sub) (second sub) env))))
