(cl:in-package #:common-macros)

(defmacro cmd:with-output-to-string
    ((string-stream-variable
      &optional
        string-form
      &key
        (element-type ''character)) &body body)
  (if (null string-form)
      `(with-open-stream
           (,string-stream-variable
            (make-string-output-stream
             :element-type ,element-type))
         ,@body
         (get-output-stream-string ,string-stream-variable))
      `(with-open-stream
           (,string-stream-variable
            (make-string-output-stream
             :string ,string-form
             :element-type ,element-type))
         ,@body)))
