(cl:in-package #:common-macro-definitions)

(defmacro with-open-file ((stream filespec &rest options
                           &key (direction :input) (element-type 'character)
                             if-exists if-does-not-exist external-format)
                          &body body)
  ;; keywords are only there to make the lambda list nice.
  (declare (ignore direction element-type if-exists if-does-not-exist external-format))
  (check-variable-name stream)
  (multiple-value-bind (declarations forms) (ecc:separate-ordinary-body body)
    (let ((abortedp (gensym "ABORT")))
      `(let ((,stream (open ,filespec ,@options)) (,abortedp t))
         (declare (dynamic-extent ,stream)) ; per CLHS
         ,@declarations
         (unwind-protect
              (multiple-value-prog1 (progn ,@forms) (setq ,abortedp nil))
           (when ,stream ; can be NIL due to :if-exists nil etc.
             (close ,stream :abort ,abortedp)))))))
