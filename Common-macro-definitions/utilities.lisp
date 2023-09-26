(cl:in-package #:common-macro-definitions)

(defparameter *operator-table*
  (make-hash-table :test #'eq))

(defun transform-name (name)
  (let ((result (gethash name *operator-table*)))
    (when (null result)
      (setq result (intern (string-downcase (symbol-name name))))
      (setf (gethash name *operator-table* ) result))
    result))

(cl:defmacro defmacro (name lambda-list &body body)
  `(cl:defmacro ,(transform-name name) ,lambda-list ,@body))

(defun macroexpand-1 (form &optional environment)
  (cl:macroexpand-1 (cons (gethash (first form) *operator-table*)
                          (rest form))
                    environment))

(defun get-setf-expansion (place &optional environment)
  (declare (ignore environment))
  (if (symbolp place)
      (let ((store-variable (gensym)))
        (values '()
                '()
                (list store-variable)
                `(setq ,place ,store-variable)
                place))
      (let ((temporaries (loop repeat (length (rest place))
                               collect (gensym)))
            (store-variable (gensym)))
        (values temporaries
                (rest place)
                (list store-variable)
                `(funcall #'(setf ,(first place))
                          ,store-variable
                          ,@temporaries)
                `(,(first place) ,@temporaries)))))

(defparameter *client* nil)
