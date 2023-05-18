(cl:in-package #:common-macros)

(defmacro cmd:prog2 (first-form second-form &rest forms)
  (let ((second-form-variable (gensym)))
    `(progn ,first-form
            (let ((,second-form-variable ,second-form))
              ,@forms
              ,second-form-variable))))
