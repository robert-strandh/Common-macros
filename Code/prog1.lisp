(cl:in-package #:common-macros)

(defmacro cmd:prog1 (first-form &rest forms)
  (let ((first-form-variable (gensym)))
    `(let ((,first-form-variable ,first-form))
       (progn ,@forms
              ,first-form-variable))))
