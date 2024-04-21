(cl:in-package #:common-macro-definitions)

(defun transform-name (name)
  (intern (string-downcase (symbol-name name))
          (find-package "COMMON-MACRO-DEFINITIONS")))

(defun macro-function (operator)
  (cl:macro-function (transform-name operator)))

(defun macro-function-exists-p (operator)
  (not (null (macro-function operator))))

(cl:defmacro defmacro (name lambda-list &body body)
  `(cl:defmacro ,(transform-name name) ,lambda-list ,@body))

(defun macroexpand-1 (form &optional environment)
  (declare (ignore environment))
  (cl:macroexpand-1 (cons (transform-name (first form))
                          (rest form))))

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

