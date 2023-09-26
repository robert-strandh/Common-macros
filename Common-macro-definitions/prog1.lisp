(cl:in-package #:common-macro-definitions)

(defmacro prog1 (first-form &rest forms)
  (let ((temp-var (gensym)))
    `(let ((,temp-var ,first-form))
       ,@forms
       ,temp-var)))
