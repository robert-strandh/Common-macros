(cl:in-package #:common-macro-definitions)

(defgeneric output-stream-from-string-function-name (client))

(defmethod output-stream-from-string-function-name (client)
  '(lambda (string) (error "Can't create stream from string ~s" string)))

(defmacro with-output-to-string
    ((stream-variable &optional string-form &key (element-type ''character))
     &body body)
  (if (null string-form)
      `(let ((,stream-variable
               (make-string-output-stream :element-type ,element-type)))
         (unwind-protect
              (progn (locally ,@body)
                     (get-output-stream-string ,stream-variable))
           (close ,stream-variable)))
      (let ((string-variable (gensym)))
        `(let* ((,string-variable ,string-form)
                (,stream-variable
                  (,(output-stream-from-string-function-name *client*)
                   ,string-variable)))
           (unwind-protect (locally ,@body)
             (close ,stream-variable))))))
