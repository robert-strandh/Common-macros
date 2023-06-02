(cl:in-package #:common-macros)

;;; This definition does not quite work, because it uses a
;;; non-standard function STRING-INPUT-STREAM-POSITION.

(defmacro cmd:with-input-from-string
    ((string-stream-variable
      string
      &key
        (start 0) end index)
     &body body)
  (if (null index)
      (multiple-value-bind (declarations forms)
          (separate-ordinary-body body)
        `(with-open-stream
             (,string-stream-variable
              (make-string-input-stream ,string ,start ,end))
           ,@declarations
           (multiple-value-prog1
               (progn ,@forms)
             (setf ,index
                   (string-input-stream-position
                    ,string-stream-variable)))))
      `(with-open-stream
           (,string-stream-variable
            (make-string-input-stream ,string ,start ,end))
         ,@body)))
