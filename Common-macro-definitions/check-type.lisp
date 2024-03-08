(cl:in-package #:common-macro-definitions)

(defun store-value-read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((temp-var place tag) &body forms)
  (let ((report-var (gensym "STORE-VALUE-REPORT"))
        (new-value-var (gensym "NEW-VALUE"))
        (form-or-forms (if (= 1 (length forms))
                           (first forms)
                           `(progn ,@forms))))
    `(flet ((,report-var (stream)
              (format stream "Supply a new value of ~S." ',place)))
       (restart-case ,form-or-forms
         (store-value (,new-value-var)
           :report ,report-var
           :interactive store-value-read-evaluated-form
           (setf ,temp-var ,new-value-var
                 ,place ,new-value-var)
           (go ,tag))))))

(defun check-type-error (place value type type-string)
  (error
   'simple-type-error
   :datum value
   :expected-type type
   :format-control (if type-string
                       "The value of ~S is ~S, which is not ~A."
                       "The value of ~S is ~S, which is not of type ~S.")
   :format-arguments (list place value (or type-string type))))

(defmacro check-type (place type &optional type-string)
  (let ((variable (gensym "CHECK-TYPE-VARIABLE"))
        (tag (gensym "CHECK-TYPE-TAG")))
    `(let ((,variable ,place))
       (tagbody ,tag
          (unless (typep ,variable ',type)
            (with-store-value-restart (,variable ,place ,tag)
              (check-type-error ',place ,variable ',type ,type-string)))))))
