(cl:in-package #:common-macro-definitions)

(defmacro prog2 (first-form second-form &rest forms)
  (let ((temp-var (gensym)))
    `(progn
       ,first-form
       (let ((,temp-var ,second-form))
         ,@forms
         ,temp-var))))
